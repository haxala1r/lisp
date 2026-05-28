
let ( let* ) = Result.bind
let traverse = Util.traverse

type literal = Core_ast.literal

type info = {
    defs : (string, Core_ast.expression) Hashtbl.t;
    toplevel : Core_ast.expression list;
    globals : (string, int) Hashtbl.t;
  }


let make_id_counter start =
  let i = ref start in
  fun () ->
  let x = !i in
  i := x + 1 ; x

let rec collect_globals f i = function
  | [] -> Ok ()      
  | Core_ast.Define (s, _) :: rest ->
     let* () = f s (i ()) in
     collect_globals f i rest
  | Core_ast.Expr _ :: rest ->
     collect_globals f i rest

let default_globals = ["+"; "-"]

(* extract global definitions from a program.
   Symbols that are not defined cannot be used at all.
 *)
let get_globals program =
  let ht = Hashtbl.create 256 in
  Hashtbl.add_seq ht (List.to_seq (List.mapi (fun i x -> (x, i)) default_globals));
  let add s i = if Option.is_some (Hashtbl.find_opt ht s) then Error ("error: re-definition of " ^ s) else Ok (Hashtbl.add ht s i) in
  let* () = collect_globals add (make_id_counter (List.length default_globals)) program in
  Ok ht

let rec find_conversion s get_global = function
  | [] ->
     (match get_global s with
     | Some _ -> Ok s
     | None -> Error ("symbol " ^ s ^ " not defined!"))
  | b :: bs ->
     match List.find_opt (fun (x, _) -> String.equal s x) b with
     | Some (_, binding) -> Ok binding
     | None -> find_conversion s get_global bs

(* Performs alpha conversion - all local bindings in the program
   tree are replaced with a globally unique identifier.
 *)
let rec alpha_convert gensym get_global bindings =
  let self = alpha_convert gensym get_global in
  let find_conversion s = find_conversion s get_global bindings in  
  Core_ast.(function
  | Literal _ as l -> Ok l
  | Begin es ->
     let* b = (Util.traverse (self bindings) es) in
     Ok (Begin b)
  | Var s ->
     let* b = find_conversion s in
     Ok (Var b)
  | Apply (f, args) ->
     let* f = self bindings f in
     let* args = traverse (self bindings) args in
     Ok (Apply (f, args))
  | Lambda (args, b) ->
     let argsp = List.map gensym args in
     let zipped = List.map2 (fun a b -> (a, b)) args argsp in
     let* b = alpha_convert gensym get_global (zipped :: bindings) b in
     Ok (Lambda (argsp, b))
  | Let (s, e, b) ->
     let sp = gensym s in
     let* e = self bindings e in
     let* b = self ([(s, sp)] :: bindings) b in
     Ok (Let (sp, e, b))
  | If (e1, e2, e3) -> (* If does not alter any bindings so this is simple *)
     let* e1 = self bindings e1 in
     let* e2 = self bindings e2 in
     let* e3 = self bindings e3 in
     Ok (If (e1, e2, e3))
                                   )

let alpha_convert_top global_table = function
  | Core_ast.Define (s, e) ->
     let* e = alpha_convert Gensym.gensym (Hashtbl.find_opt global_table) [] e in
     Ok (Core_ast.Define (s, e))
  | Core_ast.Expr e ->
     let* e = alpha_convert Gensym.gensym (Hashtbl.find_opt global_table) [] e in
     Ok (Core_ast.Expr e)

let separate_program program =
  let tbl = Hashtbl.create 127 in
  let rec aux acc = function
    | [] -> (acc, tbl)
    | Core_ast.Define (s, e) :: rest -> Hashtbl.add tbl s e ; aux acc rest
    | Core_ast.Expr e :: rest -> aux (e :: acc) rest
  in aux [] program

let extract_info program =
  let* globals = get_globals program in
  let* program = traverse (alpha_convert_top globals) program in
  let (toplevel, defs) = separate_program program in
  Ok {
    defs; toplevel; globals;
  }



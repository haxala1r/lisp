
let ( let* ) = Result.bind

type literal = Core_ast.literal

(* primitive operations *)
type primop =
  | Add
  | Sub
  | Mul
  | Div

(* Trivial values *)
type value =
  | Literal of literal
  | Var of string
  (* (lambda (args... k) e)*)
  | Lambda of string list * string * expr
  (* (lambda (v) e), reified continuation *)
  | Cont of string * expr

(* Complex expressions.
   The current continuation and meta-continuation are threaded explicitly
   The equivalents of each node in scheme syntax are written for clarity
 *)
and expr =
  (* (f args... k) *)
  | App of value * value list * value
  (* (k v) *)
  | CApp of value * value
  | If of value * expr * expr
  | Halt of value

(* debug prints *)
let p = Printf.sprintf
let rec print_literal = function
  | Core_ast.Nil -> p "()"
  | (Int x) -> p "<int %d>" x
  | (Double x) -> p "<double %f>" x
  | (String s) -> p "<string \"%s\">" s
  | (Symbol s) -> p "<symbol %s>" s
  | (Cons (a, b)) -> p "(%s . %s)" (print_literal a) (print_literal b)

let rec print_value = function
  | Literal l -> print_literal l
  | Var s -> p "%s" s
  | Lambda (args, k, b) -> p "<lambda (%s) %s %s>" (List.fold_left (fun x y -> x ^ " " ^ y) "" args) k (print_expr b)
  | Cont (k, e) -> p "<cont %s %s>" k (print_expr e)

and print_expr = function
  | App (f, args, k) -> p "(%s %s %s)" (print_value f) (List.fold_left (fun x y -> x ^ " " ^ (print_value y)) "" args) (print_value k)
  | CApp (k, v) -> p "(%s %s)" (print_value k) (print_value v)
  | If (t, th, el) -> p "if %s then %s else %s" (print_value t) (print_expr th) (print_expr el)
  | Halt v -> print_value v


let rec sub_symbol_in_expr s target =
  let self x = sub_symbol_in_expr s target x in
  function
  | Core_ast.Var sp when String.equal s sp ->
     target
  | Core_ast.Apply (v, vs) ->
     Core_ast.Apply (self v, List.map self vs)
  | Core_ast.Lambda (keep, b) ->
     Core_ast.Lambda (keep, self b)
  | Core_ast.Let (keep, e1, e2) ->
     Core_ast.Let (keep, self e1, self e2)
  | Core_ast.If (e1, e2, e3) ->
     Core_ast.If (self e1, self e2, self e3)
  | Core_ast.Begin (es) ->
     Core_ast.Begin (List.map self es)
  | rest -> rest

let gensym = Gensym.gensym

let rec cps (e : Core_ast.expression) (k : value -> expr) : expr =
  Core_ast.(
    match e with
    | Literal l -> k (Literal l)
    | Var s -> k (Var s)
    | Lambda (args, e) ->
       let kvar = gensym "k" in
       let e = cps e (fun v -> CApp (Var kvar, v)) in
       k (Lambda (args, kvar, e))
    | Apply (f, args) ->
       let rec aux g acc = function
         | [] -> g acc
         | a :: rest -> cps a (fun a -> aux g (List.append acc [a]) rest) in
       cps f (fun f ->
         aux (fun vs ->
             let res = gensym "result" in
             let cont = Cont (res, k (Var res)) in
             App (f, vs, cont)) [] args)
    | Let (s, e, b) ->
       (* We actually perform some reduction here *)
       let contarg = gensym "karg" in
       let k' : value -> expr = function
         | Literal l -> cps (sub_symbol_in_expr s (Core_ast.Literal l) b) k
         | Var v -> cps (sub_symbol_in_expr s (Core_ast.Var v) b) k
         | v -> CApp (Cont (contarg, cps (sub_symbol_in_expr s (Core_ast.Var contarg) b) k), v) in
       cps e k'
    | If (e1, e2, e3) ->
       let kargsym = gensym "karg" in
       let ksym = gensym "k" in
       let k = Cont (kargsym, k (Var kargsym)) in
       let c v = CApp (Var ksym, v) in
       CApp ((Cont (ksym, cps e1 (fun v ->
                              If (v, (cps e2 c), (cps e3 c))))),
             k)
    | Begin [] -> k (Literal Nil)
    | Begin (e :: []) ->
       cps e k
    | Begin (e :: rest) ->
       cps e (fun _ -> cps (Begin rest) k)
  )

(*
  Flattening the converted tree
  Here we perform closure conversion, and flatten the tree performed by
  the CPS transformation
 *)
type flat_access =
  | Global of int
  | Arg of int
  | Env of int
type flat_label = string
type flat_value =
  | FLiteral of literal
  | FVar of flat_access
  | FLambda of flat_label * int * flat_access list
  | FCont of flat_label * flat_access list
type flat_expr =
  | FSet of flat_access * flat_value * flat_expr
  | FApp of flat_value * flat_value list * flat_value
  | FCApp of flat_value * flat_value
  | FIf of flat_value * flat_expr * flat_expr
  | FHalt of flat_value

type info = {
    defs : (flat_label, flat_expr) Hashtbl.t;
    env : (string, flat_access) Hashtbl.t;
    funs : (flat_label * int * flat_expr) Queue.t;
    globals : (string, int) Hashtbl.t;
    toplevel : flat_expr list;
  }


let try_find_sym info acc =
  match (Seq.find (fun (_, acc2) -> acc2 == acc) (Hashtbl.to_seq info.env)) with
  | Some (s, _) -> s
  | None -> "CANTFIND"

let print_access info = function
  | Global i as acc -> p "(global %d %s)" i (try_find_sym info acc)
  | Arg i as acc -> p "(arg %d %s)" i (try_find_sym info acc)
  | Env i as acc -> p "(env %d %s)" i (try_find_sym info acc)
let print_flat_val info = function
  | FLiteral l -> print_literal l
  | FVar a -> print_access info a
  | FLambda (l, a, pack) -> p "<closure '%s' of %d args, packing %s)>" l a (List.fold_left (fun x y -> x ^ " " ^ (print_access info y)) "(" pack)
  | FCont (l, pack) -> p "<continuation closure '%s' of 1 arg, packing %s)>" l (List.fold_left (fun x y -> x ^ " " ^ (print_access info y)) "(" pack)

let rec print_flat info = function
  | FSet (acc, v, e) -> p "(set! %s %s %s)" (print_access info acc) (print_flat_val info v) (print_flat info e)
  | FApp (f, args, k) -> p "(%s %s %s)" (print_flat_val info f) (List.fold_left (fun x y -> x ^ " " ^ (print_flat_val info y)) "" args) (print_flat_val info k)
  | FCApp (k, v) -> p "(%s %s)" (print_flat_val info k) (print_flat_val info v)
  | FIf (v, e1, e2) -> p "(if %s %s %s)" (print_flat_val info v) (print_flat info e1) (print_flat info e2)
  | FHalt v -> p "%s" (print_flat_val info v)


let rec freevar_value globals args = function
  | Literal _ -> []
  | Var s ->
     (if Option.is_some (Hashtbl.find_opt globals s)
     then []
     else (if List.exists (String.equal s) args
          then []
          else [s]))
  | Lambda (a, ka, body) ->
     freevar_expr globals (a @ (ka :: args)) body
  | Cont (ka, body) ->
     freevar_expr globals (ka :: args) body
and freevar_expr globals args = function
  | App (f, ass, k) ->
     (freevar_value globals args f) @
       (List.concat (List.map (freevar_value globals args) ass)) @
         (freevar_value globals args k)
  | CApp (k, v) ->
     (freevar_value globals args k) @
       (freevar_value globals args v)
  | If (v, e1, e2) ->
     (freevar_value globals args v) @
       (freevar_expr globals args e1) @
         (freevar_expr globals args e2)
  | Halt v -> freevar_value globals args v
    

let rec flatten_val (info : Static.info) env funs = function
  | Literal l -> FLiteral l
  | Var v -> FVar (Hashtbl.find env v)
  | Lambda (args, karg, body) ->
     let label = gensym "lambda" in
     let to_pack = List.sort_uniq (String.compare) (freevar_expr info.globals (karg :: args) body) in
     let before_pack = List.map (Hashtbl.find env) to_pack in
     List.iteri (fun i a -> Hashtbl.add env a (Arg i)) args;
     List.iteri (fun i s -> Hashtbl.add env s (Env i)) to_pack;
     Hashtbl.add env karg (Arg (List.length args));
     Queue.add (label, (List.length args) + 1, closure_convert info env funs body) funs;
     FLambda (label, (List.length args) + 1, before_pack)
  | Cont (karg, body) ->
     let label = gensym "continuation" in
     let to_pack = List.sort_uniq (String.compare) (freevar_expr info.globals (karg :: []) body) in
     let before_pack = List.map (Hashtbl.find env) to_pack in
     Hashtbl.add env karg (Arg 0);
     List.iteri (fun i s -> Hashtbl.add env s (Env i)) to_pack;
     Queue.add (label, 1, closure_convert info env funs body) funs;
     FCont (label, before_pack)

and closure_convert info (env : (string, flat_access) Hashtbl.t) (funs : (flat_label * int * flat_expr) Queue.t) e =
  let self x = closure_convert info env funs x in
  let flatten v = flatten_val info env funs v in
  match e with
  | App (f, args, k) ->
     FApp (flatten f, List.map flatten args, flatten k)
  | CApp (k, v) -> FCApp (flatten k, flatten v)
  | If (v, e1, e2) -> FIf (flatten v, self e1, self e2)
  | Halt v -> FHalt (flatten v)


let print_info i =
  print_endline "Definitions:";
  Hashtbl.iter
    (fun n e -> print_endline (n ^ ": " ^ (print_flat i e)))
    i.defs;
  print_endline "Toplevel:";
  List.iter
    (fun e -> print_endline (print_flat i e)) i.toplevel ;
  print_endline "Function labels:";
  Queue.iter
    (fun (s, arg, e) -> Printf.printf "'%s' of %d args:\n%s\n" s arg (print_flat i e))
    i.funs

let top_level e =
  let* info = Static.extract_info e in
  let toplevel = List.map (fun e -> cps e (fun v -> Halt v)) info.toplevel in
  let env = Hashtbl.create 256 in
  Hashtbl.(Seq.iter (fun (s, _) -> add env s (Global (find info.globals s))) (to_seq info.defs));
  let funs = Queue.create () in
  let defs = Hashtbl.create 256 in
  Hashtbl.(Seq.iter (fun (s, e) -> add defs s (closure_convert info env funs (cps e (fun v -> Halt v)))) (to_seq info.defs));
  let toplevel = List.map (closure_convert info env funs) toplevel in
  let new_info = {
      defs;
      env;
      funs;
      globals=info.globals;
      toplevel;
    } in
  Ok new_info

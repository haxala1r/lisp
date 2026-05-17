

module SymbolTable = Map.Make(String);;

let ( let* ) = Result.bind
let traverse = Util.traverse

(* literals are not modified. *)
type literal = Core_ast.literal

(* I made this a separate type, because this behaviour is common to both symbol
   accesses, and to set! operations on symbols.
   They can both either refer to a local, or refer to a global, and making a
   separate type for this lets us statically eliminate a couple potential
   runtime errors
 *)
type variable =
  | Local of int
  | Global of int

(* Note:
   all symbol accesses are either referring to a local binding or a global one,
   and this is distinguished through the variable type above.

   Lambda expressions are stripped of the symbol name of their single parameter.
   This name is not needed at runtime, as all symbol accesses will be resolved
   into an index into either the local scope linked list or the global symbol table.

   Set is also split into its global and local versions, using the above variable type.

   The rest aren't modified at all.
 *)
type expression =
  | Literal of literal
  | Var of variable
  | Apply of expression * expression list
  | Lambda of int * expression
  | If of expression * expression * expression
  | Set of variable * expression
  | Begin of expression list
  | Native of int 
(* Native is effectively a VM primitive. Emitted here for convenience. *)


(* IMPORTANT:
   This is a predefined global table.
   Some symbols in the standard library have special importance, so
   they must have "special" values that exist before the program is
   even compiled.
   For example, the print function is always global. It must always
   be global number 0. Most other primitives have similar assignments.

   The runtime is not stable as it is now, so a program compiled with
   a current version of the compiler may not remain functional with
   later versions of the runtime. The source program should remain
   good though.
 *)
let default_global_table =
  SymbolTable.of_list [
      ("PRINT", (0, Native 0));
      ("+", (1, Native 1));
      ("-", (2, Native 2));
      ("*", (3, Native 3));
      ("/", (4, Native 4))
    ]

(* extract all defined global symbols, given the top-level expressions
   and definitions of a program

   The returned table maps symbol names to unique integers, representing
   an index into a global array where the values of all global symbols will
   be kept at runtime.
 *)
let extract_globals (top : Core_ast.top_level list) =
  let id_counter = (ref (SymbolTable.cardinal default_global_table)) in
  let id () =
    id_counter := !id_counter + 1; !id_counter in
  let rec aux tbl = function
    | [] -> tbl
    | Core_ast.Define (sym, _) :: rest ->
       aux (SymbolTable.add sym ((id ()), Literal Nil) tbl) rest
    | Expr _ :: rest ->
       aux tbl rest
  in aux default_global_table top

(* The current lexical scope is simply a linked list of entries,
   and each symbol access will be resolved as an access to an index
   in this linked list. The symbol names are erased before runtime.
   During this analysis we keep the lexical scope as a linked list of
   symbols, and we find the index by traversing this linked list.
 *)

let resolve_global tbl sym =
  match SymbolTable.find_opt sym tbl with
  | Some (x, _) -> Ok (Global x)
  | None -> Error ("symbol " ^ sym ^ " is not defined!")

(* First we try to resolve it to a local symbol, then look it up in the
   global table if we can't find it in the local environment
 *)
let resolve_symbol tbl env sym =
  let rec aux counter env_num = function
    | [] -> resolve_global tbl sym
    | x :: rest ->
       match List.find_index (String.equal sym) x with
       | Some i -> Ok (Local (counter + i))
       | None  -> aux (counter + (List.length (x :: rest))) (env_num + 1) rest
  in aux 0 0 env

let resolve_var tbl env sym =
  let* sym = resolve_symbol tbl env sym in
  Ok (Var sym)

let resolve_set tbl env sym expr =
  let* sym = resolve_symbol tbl env sym in
  Ok (Set (sym, expr))

let extract_function = function
  | Core_ast.Define (s, Core_ast.Lambda (args, rest, _)) -> Some (s, args, rest)
  | _ -> None

let extract_functions exprs =
  let fs = List.filter Option.is_some (List.map extract_function exprs) in
  let fs = List.map Option.get fs in
  List.fold_left (fun t (s, args, rest) -> SymbolTable.add s (args, rest) t) SymbolTable.empty fs


let rec analyze global_tbl =
  let rec aux tbl current = function
    | Core_ast.Literal s -> Ok (Literal s)
    | Var sym -> resolve_var tbl current sym
    | Set (sym, expr) ->
       let* inner = analyze global_tbl tbl current expr in
       resolve_set tbl current sym inner
    | Lambda (args, rest, body) ->
       let args = (match rest with
                  | Some s -> List.append args [s]
                  | None -> args) in
       let* body = (aux global_tbl (args :: current) body) in
       Ok (Lambda (List.length args, body))
    | Apply (f, es) ->
       let* f = aux tbl current f in
       let* e = Util.traverse (aux tbl current) es in
       Ok (Apply (f, e))
    | If (test, pos, neg) ->
       let* test = aux tbl current test in
       let* pos = aux tbl current pos in
       let* neg = aux tbl current neg in
       Ok (If (test, pos, neg))
    | Begin el ->
       let* body = traverse (aux tbl current) el in
       Ok (Begin body)
  in aux

let is_constantish = function
  | Literal _ -> true
  | Lambda _ -> true
  | Native _ -> true
  | _ -> false
(* We need to do some more sophisticated analysis to detect cases where
   a symbol is accessed before it is defined.
   If a symbol is accessed in a lambda body, that is fine, since that computation
   is delayed, but for top-level forms that are directly executed we must be strict.

   This function is strict by default, until it encounters a lambda, at which
   point it switches to resolving against all symbols.
   global_tbl is a table that contains ALL defined symbols,
   tbl is a table that contains symbols defined only until this point.

   NOTE: because we currently convert all let expressions into lambdas, things like
   this won't immediately be rejected by the compiler:

   (let ((a 5))
     b)
   (define b 5)

   I may consider adding special support for let forms, as this is pretty annoying.
 *)
let convert program =
  let global_tbl = ref (extract_globals program) in
  let rec aux tbl = function
    | [] -> Ok []
    | (Core_ast.Expr e) :: rest ->
       let* analysis = (analyze !global_tbl tbl [] e) in
       let* rest = aux tbl rest in
       Ok (analysis :: rest)
    | (Define (s, e)) :: rest ->
       let (id, _) = SymbolTable.find s !global_tbl in
       let* analysis = analyze !global_tbl tbl [] e in
       global_tbl := SymbolTable.remove s !global_tbl;
       global_tbl := SymbolTable.add s (id, analysis) !global_tbl;
       let tbl = SymbolTable.add s (SymbolTable.find s !global_tbl) tbl in
       let* rest = aux tbl rest in
       if is_constantish analysis then Ok (rest) else Ok (analysis :: rest)
  in
  let* program = (aux default_global_table program) in
  Ok (program, !global_tbl)

let of_src src =
  let* core = (Core_ast.of_src src) in
  convert core

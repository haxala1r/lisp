

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
  | Apply of expression * expression
  | Lambda of expression
  | If of expression * expression * expression
  | Set of variable * expression
  | Begin of expression list

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
      ("print", 0);
      ("add", 1)
    ]

(* extract all defined global symbols, given the top-level expressions
   and definitions of a program

   The returned table maps symbol names to unique integers, representing
   an index into a global array where the values of all global symbols will
   be kept at runtime.
 *)
let extract_globals (top : Core_ast.top_level list) =
  let id_counter = (ref (-1)) in
  let id () =
    id_counter := !id_counter + 1; !id_counter in
  let rec aux tbl = function
    | [] -> tbl
    | Core_ast.Define (sym, _) :: rest ->
       aux (SymbolTable.add sym (id ()) tbl) rest
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
  | Some x -> Ok (Global x)
  | None -> Error ("symbol " ^ sym ^ " is not defined!")

(* First we try to resolve it to a local symbol, then look it up in the
   global table if we can't find it in the local environment
 *)
let resolve_symbol tbl env sym =
  let rec aux counter = function
    | [] -> resolve_global tbl sym
    | x :: _ when String.equal x sym -> Ok (Local counter)
    | _ :: rest -> aux (counter + 1) rest
  in aux 0 env

let resolve_var tbl env sym =
  let* sym = resolve_symbol tbl env sym in
  Ok (Var sym)

let resolve_set tbl env sym expr =
  let* sym = resolve_symbol tbl env sym in
  Ok (Set (sym, expr))
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
  let global_tbl = extract_globals program in
  let rec analyze tbl current = function
    | Core_ast.Literal s -> Ok (Literal s)
    | Var sym -> resolve_var tbl current sym
    | Set (sym, expr) ->
       let* inner = analyze tbl current expr in
       resolve_set tbl current sym inner
    | Lambda (s, body) ->
       let* body = (analyze global_tbl (s :: current) body) in
       Ok (Lambda body)
    | Apply (f, e) ->
       let* f = analyze tbl current f in
       let* e = analyze tbl current e in
       Ok (Apply (f, e))
    | If (test, pos, neg) ->
       let* test = analyze tbl current test in
       let* pos = analyze tbl current pos in
       let* neg = analyze tbl current neg in
       Ok (If (test, pos, neg))
    | Begin el ->
       let* body = traverse (analyze tbl current) el in
       Ok (Begin body)
  in
  let[@tail_mod_cons] rec aux tbl = function
    | [] -> []
    | (Core_ast.Expr e) :: rest -> (analyze tbl [] e) :: (aux tbl rest)
    | (Define (s, e)) :: rest ->
       let tbl = SymbolTable.add s (SymbolTable.find s global_tbl) tbl in
       (analyze tbl [] (Set (s, e))) :: (aux tbl rest)
  in
  let* program = traverse (fun x -> x) (aux SymbolTable.empty program) in
  Ok (program, global_tbl)

let of_src src =
  let* core = (Core_ast.of_src src) in
  convert core

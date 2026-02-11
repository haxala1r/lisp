

module SymbolTable = Map.Make(String);;

let ( let* ) = Result.bind
let traverse = Util.traverse

(* literals are not modified. *)
type literal = Core_ast.literal

(* Note:
   all symbol accesses are replaced with either a local or global access.
   Local accesses a symbol in the local scope.
   Global accesses a symbol in the global scope.

   Lambda expressions are stripped of the symbol name of their single parameter.
   This name is not needed at runtime, as all symbol accesses will be resolved
   into an index into either the local scope linked list or the global symbol table.

   Set is also split into its global and local versions, just like Var. 

   The rest aren't modified at all.
 *)
type expression =
  | Literal of literal
  | Local of int
  | Global of int
  | Apply of expression * expression
  | Lambda of expression
  | If of expression * expression * expression
  | SetLocal of int * expression
  | SetGlobal of int * expression
  | Begin of expression list


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
  in aux SymbolTable.empty top

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

let resolve_lexical tbl env sym =
  let rec aux counter = function
    | [] -> resolve_global tbl sym
    | x :: _ when String.equal x sym -> Ok (Local counter)
    | _ :: rest -> aux (counter + 1) rest
  in aux 0 env

let resolve_symbol tbl env sym =
  resolve_lexical tbl env sym

let resolve_set tbl env sym expr =
  let* sym = resolve_symbol tbl env sym in
  match sym with
  | Local i -> Ok (SetLocal (i, expr))
  | Global i -> Ok (SetGlobal (i, expr))
  | _ -> Error "resolve_set: symbol resolution returned something invalid."

let rec analyze tbl current = function
  | Core_ast.Literal s -> Ok (Literal s)
  | Var sym -> resolve_symbol tbl current sym
  | Set (sym, expr) ->
     let* inner = analyze tbl current expr in
     resolve_set tbl current sym inner
  | Lambda (s, body) -> analyze tbl (s :: current) body
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


(* This is different from the lisp_ast data returned by the parser!
   We will first need to translate that into this in order to use it.
   This representation includes things that can only occur during runtime,
   like the various kinds of functions and macros.

   Additionally, since this is an interpreter, macros tend to be a little
   awkward in that they behave exactly like the macro gets expanded just
   before the result gets executed. This is different from the compiled
   behaviour where the macro is evaluated at compile time.

   Though of course, with the dynamic nature of lisp, and its capability
   to compile more code at runtime, there will naturally be complications.
 *)
type lisp_val = 
  | LInt of int 
  | LDouble of float 
  | LCons of lisp_val * lisp_val
  | LNil
  | LSymbol of string
  | LString of string
  
  (* a builtin function is expressed as a name and the ocaml function
  that performs the operation. The function should take a list of arguments.
  generally, builtin functions should handle their arguments directly,
  and eval forms in the environment as necessary. *)
  | LBuiltinFunction of string * (environment -> lisp_val -> lisp_val)
  | LBuiltinSpecial of string * (environment -> lisp_val -> lisp_val)
  (* a function is a name, captured environment, a parameter list, and function body. *)
  | LFunction of string * environment * lisp_val * lisp_val
  | LLambda of environment * lisp_val * lisp_val
  (* a macro is exactly the same as a function, with the distinction
     that it receives all of its arguments completely unevaluated
   *)
  | LMacro of string * environment * lisp_val * lisp_val
  | LUnnamedMacro of environment * lisp_val * lisp_val
  | LQuoted of lisp_val
(* the environment type needs to be defined here, as it is mutually
 recursive with lisp_val *)
and environment = (string, lisp_val) Hashtbl.t list

(* It is clear that we need some primitives for working with the lisp
   data structures.

   For example, the LCons and LNil values, together, form a linked list.
   This is the intended form of all source code in lisp, yet because
   we are using our own implementation of a linked list instead of
   ocaml's List, we can not use its many functions.

   It may be tempting to switch to a different implementation.
   Remember however, that classic lisp semantics allow for the
   CDR component of a cons cell (the part that would point to the
   next member) to be of a type other than the list itself.
 *)

let reverse vs =
  let rec aux prev = function
    | LNil -> prev
    | LCons (v, next) -> aux (LCons (v, prev)) next
    | _ -> invalid_arg "cannot reverse non-list!"
  in aux LNil vs

let map f =
  let rec aux accum = function
    | LNil -> reverse accum
    | LCons (v, next) -> aux (LCons (f v, accum)) next
    | _ -> invalid_arg "cannot map over non-list!"
  in aux LNil

let reduce init f =
  let rec aux accum = function
    | LNil -> accum
    | LCons (v, next) -> aux (f accum v) next
    | _ -> invalid_arg "cannot reduce over non-list!"
  in aux init

let rec dbg_print_list =
  let pf = Printf.sprintf in
  function
  | LCons (v, LNil) -> pf "%s" (dbg_print_one v)
  | LCons (v, rest) -> (pf "%s " (dbg_print_one v)) ^ (dbg_print_list rest)
  | v -> pf ". %s" (dbg_print_one v)

and dbg_print_one v =
  let pf = Printf.sprintf in
  match v with
  | LInt x ->  pf "<int: %d>" x
  | LSymbol s -> pf "<symbol: '%s'>" s
  | LString s -> pf "<string: '%s'>" s
  | LNil -> pf "<nil>"
  | LCons _ -> pf "<list: (%s)>" (dbg_print_list v) 
  | LDouble d -> pf "<double: %f>" d
  | LBuiltinSpecial (name, _)
  | LBuiltinFunction (name, _) -> pf "<builtin: %s>" name
  | LLambda (_, args, _) -> pf "<unnamed function, lambda-list: %s>"
    (dbg_print_one args)
  | LFunction (name, _, args, _) -> pf "<function: '%s' lambda-list: %s>" 
    name (dbg_print_one args)
  | LUnnamedMacro (_, args, _) -> pf "<unnamed macro, lambda-list: %s>"
    (dbg_print_one args)
  | LMacro (name, _, args, _) -> pf "<macro '%s' lambda-list: %s>"
    name (dbg_print_one args)
  | LQuoted v -> pf "<quote: %s>" (dbg_print_one v)
  (*| _ -> "<Something else>"*)

let rec pretty_print_one v =
  let pf = Printf.sprintf in
  match v with
  | LInt x -> pf "%d" x
  | LSymbol s -> pf "%s" s
  | LString s -> pf "\"%s\"" s
  | LNil -> pf "()"
  | LCons (a, b) -> pf "(%s)" (dbg_print_list (LCons (a,b)))
  | LDouble d -> pf "%f" d
  | LQuoted v -> pf "'%s" (pretty_print_one v)
  | LBuiltinSpecial _
  | LBuiltinFunction _ 
  | LLambda _
  | LFunction _
  | LUnnamedMacro _
  | LMacro _  -> dbg_print_one v

let pretty_print_all vs =
  let pr v = Printf.printf "%s\n" (pretty_print_one v) in
  List.iter pr vs

let dbg_print_all vs =
  let pr v = Printf.printf "%s\n" (dbg_print_one v) in
  List.iter pr vs


let rec convert_one = function
  | Parser.Ast.LInt x -> LInt x
  | Parser.Ast.LDouble x -> LDouble x
  | Parser.Ast.LNil -> LNil
  | Parser.Ast.LString s -> LString s
  | Parser.Ast.LSymbol s -> LSymbol s
  | Parser.Ast.LCons (a, b) -> LCons (convert_one a, convert_one b)


let read_from_str s =
  List.map convert_one (Parser.parse_str s)

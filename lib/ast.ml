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
  in a compiled lisp this would probably make more of a difference *)
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

let rec dbg_print_one v =
  let pf = Printf.sprintf in
  match v with
  | LInt x ->  pf "<int: %d>" x
  | LSymbol s -> pf "<symbol: '%s'>" s
  | LString s -> pf "<string: '%s'>" s
  | LNil -> pf "()"
  | LCons (a, b) -> pf "(%s . %s)" (dbg_print_one a) (dbg_print_one b)
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

let dbg_print_all vs =
  let pr v = Printf.printf "%s\n" (dbg_print_one v) in
  List.iter pr vs

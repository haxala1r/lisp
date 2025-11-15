(** This is a simplified representation for the source code.
    Note that this is different from the data representation used
    during execution - that must naturally include things like
    functions, structs, classes, etc.

    This is used just to represent source code in an easy to process
    manner. We translate this before actually using it.
    The interpreter directly translates to its value type, the
    compiler uses this to optimise and compile.
 **)

type lisp_ast =
  | LInt of int
  | LDouble of float
  | LSymbol of string
  | LString of string
  | LNil
  | LCons of lisp_ast * lisp_ast

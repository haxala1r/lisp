

type lisp_ast =
  | LInt of int
  | LDouble of float
  | LSymbol of string
  | LString of string
  | LNil
  | LCons of lisp_ast * lisp_ast


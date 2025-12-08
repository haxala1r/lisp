
open Parser.Ast;;

(* This type represents an intermediate step between the AST and opcodes in our
   compiler. We need this extra step to resolve addresses, e.g. how do you know
   what exact address an if expression needs to jump to before you compile it?
   you don't, you just keep a symbolic label there, resolve later.
 *)
type intermediate_opcode =
  | ISelect of string * string
  | ILDF of string
  | ILD of int (* an index into the constant table *)
  | INil
  | IRet
  | IAdd
  | IJoin
  | ILabel of string (* does not emit any byte code *)




(* TODO: Complete *)
let (compile : lisp_ast -> intermediate_opcode list) = function
  | LInt x -> [ILD x]
  | _ -> [];;

type symbol = string

(* These are just used for the GADT *)
type expression
type definition
type clause (* for cond *)
type binding (* for let *)
type lambda_list
type body

type _ t =
  (* Literals *)
  | LitInt : int -> expression t
  | LitDouble : float -> expression t
  | LitString : string -> expression t
  | LitNil : expression t
  | QuotedList : expression t list * expression t option -> expression t

  | Body : definition t list * expression t list -> body t
  | LambdaList : symbol list * symbol option -> lambda_list t
  | Lambda : lambda_list t * body t -> expression t
  | Define : symbol * expression t -> definition t

  | LetBinding : symbol * expression t -> binding t
  | Let : binding t list * body t -> expression t
  | LetRec : binding t list * body t -> expression t

  | CondClause : expression t * expression t -> clause t
  | Cond : clause t list -> expression t

  | If : expression t * expression t * expression t -> expression t
  | Set : string * expression t -> expression t

  | Var : symbol -> expression t
  | Apply : expression t * expression t list -> expression t

type top_level =
  | Def of definition t
  | Exp of expression t


val make : Parser.Ast.lisp_ast -> (top_level, string) result

val print_def : definition t -> string
val print_expr : expression t -> string
val print : top_level -> string


type literal =
  | Int of int
  | Double of float
  | String of string
  | Nil
  | Cons of literal * literal

(* The Core Abstract Syntax Tree.
   This tree does not use a GADT, as every type of expression
   will be reduced to its simplest equivalent form before ending
   up here. There is no reason to make this tree typed.
 *)
type expression =
  | Literal of literal
  | Var of string
  | Apply of expression * expression list
  | Lambda of string list * expression
  | Let of (string * expression) list * expression
  | LetRec of (string * expression) list * expression
  | If of expression * expression * expression
  | Set of string * expression
  | Begin of expression list


type top_level =
  | Define of string * expression
  | Expr of expression





let rec pair_of_def : Syntactic_ast.definition Syntactic_ast.t -> string * expression = function
  | Syntactic_ast.Define (s, e) -> (s, of_expr e)
and pair_of_binding = function
  | Syntactic_ast.LetBinding (s, e) -> (s, of_expr e)
and pair_of_clause = function
  | Syntactic_ast.CondClause (e1, e2) -> (of_expr e1, of_expr e2)
(* We convert a body into a regular letrec form.
   A body is defined as a series of definitions followed by a series
   of expressions. The definitions behave exactly as a letrec, so
   it makes sense to convert the body into a normal letrec.
 *)
and of_body : Syntactic_ast.body Syntactic_ast.t -> expression = function
  | Body (defs, exprs) ->
     let exprs = List.map of_expr exprs in
     let defs = List.map pair_of_def defs in
     let b = Begin exprs in
     LetRec (defs, b)

(* TODO: currently this ignores the "optional" part of the lambda list,
   fix this *)
and of_ll : Syntactic_ast.lambda_list Syntactic_ast.t -> string list = function
  | LambdaList (sl, _) -> sl

and of_expr : Syntactic_ast.expression Syntactic_ast.t -> expression = function
  | LitNil -> Literal Nil
  | LitInt x -> Literal (Int x)
  | LitDouble x -> Literal (Double x)
  | LitString x -> Literal (String x)
  | Var x -> Var x
  | QuotedList _ -> failwith "TODO: rethink how quoted lists should work, the current definition makes no sense."
  | Lambda (ll, b) -> Lambda (of_ll ll, of_body b)
  | Let (bindings, b) -> Let (List.map pair_of_binding bindings, of_body b)
  | LetRec (bindings, b) -> LetRec (List.map pair_of_binding bindings, of_body b)
  | Cond (clauses) -> 
     List.fold_right
       (fun (e1, e2) acc -> If (e1, e2, acc))
       (List.map pair_of_clause clauses)
       (Literal Nil)
  | If (e1, e2, e3) ->
     If (of_expr e1, of_expr e2, of_expr e3)
  | Set (s, e) -> Set (s, of_expr e)
  | Apply (f, es) -> Apply (of_expr f, List.map of_expr es)


and of_syntactic : Syntactic_ast.top_level -> top_level = function
  | Def (Define (s, e)) -> Define (s, of_expr e)
  | Exp (e) -> Expr (of_expr e)
  | _ -> .


let of_sexpr x =
  Result.bind (Syntactic_ast.make x)
    (fun x -> Ok (of_syntactic x))

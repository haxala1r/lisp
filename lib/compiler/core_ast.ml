
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
  | Apply of expression * expression
  | Lambda of string * expression
  | LetRec of (string * expression) list * expression
  | If of expression * expression * expression
  | Set of string * expression
  | Begin of expression list


type top_level =
  | Define of string * expression
  | Expr of expression



let rec pair_of_def : Syntactic_ast.def -> string * expression =
  fun (s, e) -> (s, of_expr e)
and pair_of_binding (s, e) = (s, of_expr e)
and pair_of_clause (e1, e2) = (of_expr e1, of_expr e2)

and make_lambda args body =
  match args with
    (* TODO: gensym here instead of using _ directly *)
  | [] -> Lambda ("_", of_body body)
  | x :: [] -> Lambda (x, of_body body)
  | x :: xs -> Lambda (x, make_lambda xs body)

and make_apply f args =
  let rec aux f = function
    | [] -> Apply (f, Literal Nil)
    | arg :: [] -> Apply (f, arg)
    | arg :: args -> aux (Apply (f, arg)) args
  in aux f args
(* desugars this...
  (let ((x 5) (y 4)) (f x y))
  ... into this...
  (((lambda (x) (lambda (y) ((f x) y)))  5)  4)
 *)
and make_let bs body =
  let bs = List.map pair_of_binding bs in
  let rec aux = function
    | (s, e) :: rest ->
       Apply (Lambda (s, aux rest), e)
    | [] -> of_body body in
  aux bs
(* We convert a body into a regular letrec form.
   A body is defined as a series of definitions followed by a series
   of expressions. The definitions behave exactly as a letrec, so
   it makes sense to convert the body into a normal letrec.
 *)
and of_body : Syntactic_ast.body -> expression = function
  | ([], exprs) ->
     let exprs = List.map of_expr exprs in
     Begin exprs
  | (defs, exprs) ->
     let exprs = List.map of_expr exprs in
     let defs = List.map pair_of_def defs in
     let b = Begin exprs in
     LetRec (defs, b)

(* TODO: currently this ignores the "optional" part of the lambda list,
   fix this *)
and of_ll : Syntactic_ast.lambda_list -> string list = function
  | (sl, _) -> sl

and of_literal : Syntactic_ast.literal -> literal  = function
  | LitInt x -> Int x
  | LitDouble x -> Double x
  | LitString x -> String x
  | LitCons (a, b) -> Cons (of_literal a, of_literal b)
  | LitNil -> Nil
  
and of_expr : Syntactic_ast.expr -> expression = function
  | Literal l -> Literal (of_literal l)
  | Var x -> Var x
  | Lambda (ll, b) -> make_lambda (of_ll ll) b
  | Let (bindings, b) -> make_let bindings b
  | LetRec (bindings, b) -> LetRec (List.map pair_of_binding bindings, of_body b)
  | Cond (clauses) -> 
     List.fold_right
       (fun (e1, e2) acc -> If (e1, e2, acc))
       (List.map pair_of_clause clauses)
       (Literal Nil)
  | If (e1, e2, e3) ->
     If (of_expr e1, of_expr e2, of_expr e3)
  | Set (s, e) -> Set (s, of_expr e)
  | Apply (f, es) -> make_apply (of_expr f) (List.map of_expr es)


and of_syntactic : Syntactic_ast.top_level -> top_level = function
  | Def (s, e) -> Define (s, of_expr e)
  | Exp (e) -> Expr (of_expr e)
  | _ -> .


let of_sexpr x =
  Result.bind (Syntactic_ast.make x)
    (fun x -> Ok (of_syntactic x))

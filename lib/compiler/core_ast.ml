
let traverse = Util.traverse

type literal =
  | Int of int
  | Double of float
  | String of string
  | Symbol of string
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
  | Let of string * expression * expression
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
  Lambda (args, body)

and make_letrec defs e =
  let sets = List.map (fun (s, e) -> Set (s,e)) defs in
  let rec aux = function
    | [] -> (Begin (List.append sets [e]))
    | (s, _) :: rest -> Let (s, Literal Nil, aux rest) in
  aux defs
(* We convert a body into a regular letrec form.
   A body is defined as a series of definitions followed by a series
   of expressions. The definitions behave exactly as a letrec, so
   it makes sense to convert the body into a normal letrec.
 *)
and of_body : Syntactic_ast.body -> expression = function
  | ([], e :: []) -> of_expr e      
  | ([], exprs) ->
     let exprs = List.map of_expr exprs in
     Begin exprs
  | (defs, e :: []) ->
     let defs = List.map pair_of_def defs in
     make_letrec defs (of_expr e)
  | (defs, exprs) ->
     let exprs = List.map of_expr exprs in
     let defs = List.map pair_of_def defs in
     make_letrec defs (Begin exprs)

and of_ll : Syntactic_ast.lambda_list -> string list * string option = function
  | (sl, rest) -> (sl, rest)

and of_literal : Syntactic_ast.literal -> literal  = function
  | LitInt x -> Int x
  | LitDouble x -> Double x
  | LitString x -> String x
  | LitCons (a, b) -> Cons (of_literal a, of_literal b)
  | LitNil -> Nil
  | LitSymbol s -> Symbol s
  
and of_expr : Syntactic_ast.expr -> expression = function
  | Literal l -> Literal (of_literal l)
  | Var x -> Var x
  | Lambda ((args, _), b) -> Lambda (args, of_body b)
  | Let ([], b) -> of_body b
  | Let ((s,e) :: [] , b) -> Let (s, of_expr e, of_body b)
  | Let ((s, e) :: bindings, b) -> Let (s, of_expr e, of_expr (Let (bindings, b))) 
  | LetRec (bindings, b) -> make_letrec (List.map pair_of_binding bindings) (of_body b)
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
  | Def (s, e) -> Define (s, of_expr e)
  | Exp (e) -> Expr (of_expr e)
  | _ -> .


let of_sexpr x =
  Result.bind (Syntactic_ast.make x)
    (fun x -> Ok (of_syntactic x))

let of_src src =
  let sexprs = Parser.parse_str src in
  traverse of_sexpr sexprs

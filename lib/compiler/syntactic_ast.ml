
(* The entire point of this module is to transform a given sexpr tree into
   an intermediary AST that directly represents the grammar.
 *)

(* Literals *)
type literal =
  | LitInt of int
  | LitDouble of float
  | LitString of string
  | LitCons of literal * literal
  | LitSymbol of string
  | LitNil

type lambda_list = string list * string option

type expr =
  | Literal of literal
  | Lambda of lambda_list * body
  | Let of (string * expr) list * body
  | LetRec of (string * expr) list * body
  | Cond of (expr * expr) list
  | If of expr * expr * expr
  | Set of string * expr
  | Var of string
  | Apply of expr * expr list
and def = string * expr
and body = def list * expr list

(* On the top-level we only allow definitions and expressions *)
type top_level =
  | Def of def
  | Exp of expr


(* we use result here to make things nicer *)
let ( let* ) = Result.bind
let traverse = Util.traverse
let map = List.map



let unwrap_exp = function
  | Ok (Exp x) -> Ok x
  | Error _ as e -> e
  | _ -> Error "Definition found in Expression context"
let unwrap_def x =
  let* x = x in
  match x with
  | Def d -> Ok d
  | _ -> Error "Expression found in Definition context"
let exp x = Ok (Exp x)
let lit x = Ok (Exp (Literal x))
let def x = Ok (Def x)


open Parser.Ast

let sexpr_car = function
  | LCons (a, _) -> Ok a
  | _ -> Error "cannot take car of expression."
let sexpr_cdr = function
  | LCons (_, d) -> Ok d
  | _ -> Error "cannot take cdr of expression."
let sexpr_cadr cons =
  let* cdr = sexpr_cdr cons in
  sexpr_car cdr
let sexpr_cddr cons =
  let* cdr = sexpr_cdr cons in
  sexpr_cdr cdr
let sexpr_caddr cons =
  let* cddr = sexpr_cddr cons in
  sexpr_car cddr

let expect_sym = function
  | LSymbol s -> Ok s
  | _ -> Error "Expected symbol!"

(* We must now transform the s-expression tree into a proper, typed AST
   First, we need some utilities for transforming proper lists and s-expr conses.

   TODO: add diagnostics, e.g. what sexpr, specifically, couldn't be turned to a list?
   generally more debugging is needed in this module.
 *)
let rec list_of_sexpr = function
  | LCons (i, next) ->
     let* next = list_of_sexpr next in
     Ok (i :: next)
  | LNil -> Ok []
  | _ -> Error "cannot transform sexpr into list, malformed sexpr!"

(* parse the argument list of a lambda form *)
let parse_lambda_list cons =
  let rec aux acc = function
    | LCons (LSymbol a, LSymbol b) ->
       Ok (List.rev (a :: acc), Some b)
    | LCons (LSymbol a, rest) ->
       aux (a :: acc) rest
    | LNil -> Ok (List.rev acc, None)
    | _ -> Error "Improper lambda list."
  in aux [] cons

(* Is the given lisp_ast node a definition? *)
let is_def = function
    | LCons (LSymbol "define", _) -> true
    | _ -> false

(* These five functions all depend on each other, which is why they are
   defined in this let..and chain.
 *)
let rec parse_body body =
  (* This helper function separates the definitions and expressions from the body *)
  let rec aux acc = function
    | expr :: rest when is_def expr ->
       aux (expr :: acc) rest
    | rest ->
       Ok (List.rev acc, rest)
  in
  let* body = list_of_sexpr body in
  let* (defs, exprs) = aux [] body in  
  (* Once the expressions and definitions are separated we must parse them, then
     unpack them from the top_level type.
   *)
  let* defs = traverse (Fun.compose unwrap_def transform) defs in
  let* exprs = traverse (Fun.compose unwrap_exp transform) exprs in
  Ok (defs, exprs)

and builtin_define cons = 
  let* second = sexpr_cadr cons in
  match second with
  | LSymbol sym ->
     (* regular, symbol/variable definition *)
     let* third = sexpr_caddr cons in
     let* value = unwrap_exp (transform third) in
     Ok (Def (sym, value))
  | LCons (LSymbol sym, ll) ->
     (* function definition, we treat this as a define + lambda *)
     let* lambda_list = parse_lambda_list ll in
     let* body = sexpr_cddr cons in
     let* body = parse_body body in
     Ok (Def (sym, Lambda (lambda_list, body)))
  | _ -> Error "invalid definition!"

and builtin_lambda cons =
  let* lambda_list = sexpr_cadr cons in
  let* lambda_list = parse_lambda_list lambda_list in
  let* body = sexpr_cddr cons in
  let* body = parse_body body in
  exp (Lambda (lambda_list, body))

and parse_bindings cons =
  let parse_one cons =
    let* sym = sexpr_car cons in
    let* sym = expect_sym sym in
    let* expr = sexpr_cadr cons in
    let* expr = unwrap_exp (transform expr) in
    Ok (sym, expr)
  in
  let* l = list_of_sexpr cons in
  traverse parse_one l

and make_builtin_let f cons =
  let* bindings = sexpr_cadr cons in
  let* bindings = parse_bindings bindings in
  let* body = sexpr_cddr cons in
  let* body = parse_body body in
  exp (f bindings body)

and parse_clauses cons =
  let parse_one cons =
    let* test = sexpr_car cons in
    let* test = unwrap_exp (transform test) in
    let* expr = sexpr_cadr cons in
    let* expr = unwrap_exp (transform expr) in
    Ok (test, expr)
  in
  let* l = list_of_sexpr cons in
  traverse parse_one l

and builtin_cond cons =
  let* clauses = sexpr_cdr cons in
  let* clauses = parse_clauses clauses in
  exp (Cond clauses)

and builtin_if cons =
  let* cons = sexpr_cdr cons in
  let* test = sexpr_car cons in
  let* test = unwrap_exp (transform test) in
  let* then_branch = sexpr_cadr cons in
  let* then_branch = unwrap_exp (transform then_branch) in
  let* else_branch = (match sexpr_caddr cons with
  | Error _ -> Ok LNil
  | Ok x -> Ok x) in
  let* else_branch = unwrap_exp (transform else_branch) in
  exp (If (test, then_branch, else_branch))

and builtin_set cons =
  let* cons = sexpr_cdr cons in
  let* sym = sexpr_car cons in
  let* sym = (match sym with
             | LSymbol s -> Ok s
             | _ -> Error "cannot (set!) a non-symbol") in
  let* expr = sexpr_cadr cons in
  let* expr = unwrap_exp (transform expr) in
  exp (Set (sym, expr))

and builtin_quote cons =
  let* expr = sexpr_cadr cons in
  let lit x = exp (Literal x) in
  let rec aux = function
    | LSymbol s -> (LitSymbol s)
    | LInt x -> (LitInt x)
    | LDouble x -> (LitDouble x)
    | LString x -> (LitString x)
    | LCons (a, b) -> (LitCons (aux a, aux b))
    | LNil -> (LitNil) in
  lit (aux expr)

and apply f args =
  let* args = list_of_sexpr args in
  let* args = traverse (fun x -> unwrap_exp (transform x)) args in
  let* f = unwrap_exp (transform f) in
  exp (Apply (f, args))

and builtin_symbol = function
  | "define" -> builtin_define
  | "lambda" -> builtin_lambda
  | "let" -> (make_builtin_let (fun x y -> Let (x,y)))
  | "letrec" -> (make_builtin_let (fun x y -> LetRec (x,y)))
  | "cond" -> builtin_cond
  | "if" -> builtin_if
  | "set!" -> builtin_set
  | "quote" -> builtin_quote
  | _ -> (function
       | LCons (f, args) -> apply f args
       | _ -> Error "Invalid function application!")

and transform : lisp_ast -> (top_level, string) result = function
  | LInt x -> lit (LitInt x)
  | LDouble x -> lit (LitDouble x)
  | LString x -> lit (LitString x)
  (* NOTE: not all symbols are automatically Variable expressions,
     Some must be further parsed (such as inside a definition)
   *)
  | LSymbol x -> exp (Var x)
  | LNil -> lit (LitNil)
  | LCons (LSymbol s, _) as cons -> (builtin_symbol s) cons
  | LCons (f, args) -> apply f args

let make (expr : Parser.Ast.lisp_ast) : (top_level, string) result =
  transform expr

let of_src s =
  Util.traverse make (Parser.parse_str s)

(* Printing, for debug purposes *)
let pf = Printf.sprintf
let rec print_lambda_list = function
  | (strs, None) -> ("(" ^ (String.concat " " strs) ^ ")")
  | (strs, Some x) -> ("(" ^ (String.concat " " strs) ^ " . " ^  x ^ ")")
and print_let_binding x =
  let (s, expr) = x in
  pf "(%s %s)" s (print_expr expr)
and print_bindings l =
  ("(" ^ (String.concat "\n" (map print_let_binding l)) ^ ")")
and print_clause x =
  let (test, expr) = x in
  pf "(%s %s)" (print_expr test) (print_expr expr)
and print_clauses l =
  (String.concat "\n" (map print_clause l))
and  print_def = function
  | (s, expr) ->
     pf "(define %s
           %s)" s (print_expr expr)
and print_defs l =
  String.concat "\n" (map print_def l)

and print_literal = function
  | LitDouble x -> pf "%f" x
  | LitInt x -> pf "%d" x
  | LitString x -> pf "\"%s\"" x
  | LitNil -> pf "nil"
  | LitCons (a, b) -> pf "(%s . %s)" (print_literal a) (print_literal b)
  | LitSymbol s -> pf "'%s" s
and print_expr = function
  | Literal l -> print_literal l
  | Lambda (ll, (defs, exprs)) ->
     pf "(lambda %s
           ; DEFINITIONS
           %s
           ; BODY
           %s)"
       (print_lambda_list ll)
       (String.concat "\n" (map print_def defs))
       (String.concat "\n" (map print_expr exprs))
  | Let (binds, (defs, exprs)) ->
     pf "(let
           ; BINDINGS
           %s
           ; DEFINITIONS
           %s
           ; EXPRESSIONS
           %s)"
       (print_bindings binds)
       (print_defs defs)
       (print_exprs exprs)
  | LetRec (binds, (defs, exprs)) ->
     pf "(letrec
           ; BINDINGS
           %s
           ; BODY
           %s
           ; EXPRESSIONS
           %s)"
       (print_bindings binds)
       (print_defs defs)
       (print_exprs exprs)
  | Cond (clauses) ->
     pf "(cond
           %s)"
       (print_clauses clauses)
  | Var s -> s
  | If (e1, e2, e3) ->
     pf "(if %s %s %s)" (print_expr e1) (print_expr e2) (print_expr e3)
  | Set (s, expr) ->
     pf "(set! %s %s)" s (print_expr expr)
  | Apply (f, exprs) ->
     pf "(apply %s %s)"
       (print_expr f)
       ("(" ^ (String.concat " " (map print_expr exprs)) ^ ")")
(* | _ -> "WHATEVER" *)
and print_exprs l =
  String.concat "\n" (map print_expr l)

let print = function
  | Def x -> print_def x
  | Exp x -> print_expr x

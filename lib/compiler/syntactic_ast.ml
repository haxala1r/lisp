open Containers

(* The entire point of this module is to transform a given sexpr tree into
   an intermediary typed AST.
 *)

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

(* On the top-level we only allow definitions and expressions *)
type top_level =
  | Def of definition t
  | Exp of expression t


(* we use result here to make things nicer *)

let ( let* ) = Result.( let* )
let map = List.map
let exp x = Ok (Exp x)
let unwrap_exp = function
  | Ok (Exp x) -> Ok x
  | Error _ as e -> e
  | _ -> Error "Definition found in Expression context"
let unwrap_def x =
  let* x = x in
  match x with
  | Def d -> Ok d
  | _ -> Error "Expression found in Definition context"
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
       Ok (LambdaList (List.rev (a :: acc), Some b))
    | LCons (LSymbol a, rest) ->
       aux (a :: acc) rest
    | LNil -> Ok (LambdaList (List.rev acc, None))
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
  let* defs = Result.map_l (Fun.compose transform unwrap_def) defs in
  let* exprs = Result.map_l (Fun.compose transform unwrap_exp) exprs in
  Ok (Body (defs, exprs))

and builtin_define cons = 
  let* second = sexpr_cadr cons in
  match second with
  | LSymbol sym ->
     (* regular, symbol/variable definition *)
     let* third = sexpr_caddr cons in
     let* value = unwrap_exp (transform third) in
     def (Define (sym, value))
  | LCons (LSymbol sym, ll) ->
     (* function definition, we treat this as a define + lambda *)
     let* lambda_list = parse_lambda_list ll in
     let* body = sexpr_cddr cons in
     let* body = parse_body body in
     def (Define (sym, Lambda (lambda_list, body)))
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
    Ok (LetBinding (sym, expr))
  in
  let* l = list_of_sexpr cons in
  Result.map_l parse_one l

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
    Ok (CondClause (test, expr))
  in
  let* l = list_of_sexpr cons in
  Result.map_l parse_one l

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
  let* sym = sexpr_car cons in
  let* sym = (match sym with
             | LSymbol s -> Ok s
             | _ -> Error "cannot (set!) a non-symbol") in
  let* expr = sexpr_cadr cons in
  let* expr = unwrap_exp (transform expr) in
  exp (Set (sym, expr))

and apply f args =
  let* args = list_of_sexpr args in
  let* args = Result.map_l (fun x -> unwrap_exp (transform x)) args in
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
  | _ -> (function
       | LCons (f, args) -> apply f args
       | _ -> Error "Invalid function application!")

and transform : lisp_ast -> (top_level, string) result = function
  | LInt x -> exp (LitInt x)
  | LDouble x -> exp (LitDouble x)
  | LString x -> exp (LitString x)
  (* NOTE: not all symbols are automatically Variable expressions,
     Some must be further parsed (such as inside a definition)
   *)
  | LSymbol x -> exp (Var x)
  | LNil -> exp (LitNil)
  | LCons (LSymbol s, _) as cons -> (builtin_symbol s) cons
  | LCons (f, args) -> apply f args

let make (expr : Parser.Ast.lisp_ast) : (top_level, string) result =
  transform expr



(* Printing, for debug purposes *)
let pf = Printf.sprintf
let rec print_as_list l =
  let l = map print_expr l in
  String.concat " " l
and print_lambda_list = function
  | LambdaList (strs, None) -> ("(" ^ (String.concat " " strs) ^ ")")
  | LambdaList (strs, Some x) -> ("(" ^ (String.concat " " strs) ^ " . " ^  x ^ ")")
and print_let_binding x =
  let (LetBinding (s, expr)) = x in
  pf "(%s %s)" s (print_expr expr)
and print_bindings l =
  ("(" ^ (String.concat "\n" (map print_let_binding l)) ^ ")")
and print_clause x =
  let (CondClause (test, expr)) = x in
  pf "(%s %s)" (print_expr test) (print_expr expr)
and print_clauses l =
  (String.concat "\n" (map print_clause l))
and  print_def = function
  | Define (s, expr) ->
     pf "(define %s
           %s)" s (print_expr expr)
and print_defs l =
  String.concat "\n" (map print_def l)
and print_expr = function
  | LitDouble x -> pf "%f" x
  | LitInt x -> pf "%d" x
  | LitString x -> pf "\"%s\"" x
  | LitNil -> pf "nil"
  | QuotedList (exprs, None) -> "(" ^ (print_as_list exprs) ^ ")"
  | QuotedList (exprs, Some ex) -> "(" ^ (print_as_list exprs) ^ " . " ^ (print_expr ex) ^ ")"
  | Lambda (ll, Body (defs, exprs)) ->
     pf "(lambda %s
           ; DEFINITIONS
           %s
           ; BODY
           %s)"
       (print_lambda_list ll)
       (String.concat "\n" (map print_def defs))
       (String.concat "\n" (map print_expr exprs))
  | Let (binds, Body (defs, exprs)) ->
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
  | LetRec (binds, Body (defs, exprs)) ->
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

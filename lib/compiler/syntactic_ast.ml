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

type _ t =
  | LitInt : int -> expression t
  | LitDouble : float -> expression t
  | LitString : string -> expression t
  | LitNil : expression t
  | QuotedList : expression t list * expression t option -> expression t
  | LambdaList : symbol list * symbol option -> lambda_list t
  | Lambda : lambda_list t * definition t list * expression t list -> expression t
  | Define : symbol * expression t -> definition t

  | LetBinding : symbol * expression t -> binding t
  | Let : binding t list * expression t list -> expression t

  | CondClause : expression t * expression t -> clause t
  | Cond : clause t list -> expression t

  | Var : symbol -> expression t
  | Apply : expression t * expression t list -> expression t

(* On the top-level we only allow definitions and expressions *)
type top_level =
  | Def of definition t
  | Exp of expression t


(* we use result here to make things nicer *)

let ( let* ) = Result.( let* )
let map = List.map
let nth_err err l n =
  match List.nth_opt l n with
  | None -> err
  | Some v -> Ok v

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
let is_def = function
  | Def _ -> true
  | _ -> false

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
let sexpr_cdddr cons =
  let* cddr = sexpr_cddr cons in
  sexpr_cdr cddr

(* We must now transform the s-expression tree into a proper, typed AST
   First, we need some utilities for transforming proper lists and s-expr conses.

   TODO: add diagnostics, e.g. what sexpr, specifically, couldn't be turned to a list?
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
    | LCons (LSymbol a, LNil) ->
       Ok (LambdaList (List.rev (a :: acc), None))
    | LCons (LSymbol a, rest) ->
       aux (a :: acc) rest
    | _ -> Error "Improper lambda list."
  in aux [] cons

let next_of_cons err_msg = function
  | LCons (car, rest) -> Ok (car, rest)
  | _ -> Error err_msg

let rec a x = b x
and b x = c x
and c _ = a 5;;



let rec parse_lambda_body body =
  let is_def = function
    | LCons (LSymbol "define", _) -> true
    | _ -> false
  in
  let rec aux acc = function
    | expr :: rest when is_def expr ->
       aux (expr :: acc) rest
    | _ :: rest ->
       Ok (acc, rest)
    | [] -> Ok (acc, [])
  in
  let* body = list_of_sexpr body in
  let* (defs, exprs) = aux [] body in
  
  let* defs = Result.map_l (Fun.compose transform unwrap_def) defs in
  let* exprs = Result.map_l (Fun.compose transform unwrap_exp) exprs in
  Ok (defs, exprs)

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
     let* (defs, exprs) = parse_lambda_body body in
     def (Define (sym, Lambda (lambda_list, defs, exprs)))
  | _ -> Error "lmao"


and apply f args =
  let* args = list_of_sexpr args in
  let* args = Result.map_l (fun x -> unwrap_exp (transform x)) args in
  let* f = unwrap_exp (transform f) in
  exp (Apply (f, args))

and builtin_symbol = function
  | "define" -> builtin_define
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

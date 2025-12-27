

(* In this module we handle syntax sugar, i.e. simple built-in transformations
   on source code.

   Examples:
   (define (f x) ...) = (define f (lambda (x) ...))
 *)

open Parser.Ast

let rec sexpr_to_list = function
  | LCons (a, b) -> a :: (sexpr_to_list b)
  | LNil -> []
  | _ -> failwith "Not proper list!"
let rec list_to_sexpr = function
  | a :: b -> LCons (a, list_to_sexpr b)
  | [] -> LNil

let rec sexpr_map f = function
  | LCons (a, b) -> LCons (f a, sexpr_map f b)
  | LNil -> LNil
  | _ -> failwith "Not proper list!!!"

(* This MUST be called after function definitions have been desugared,
   i.e. desugar_define_functions has been called
 *)
let rec collect_definitions = function
  | LCons (LCons (LSymbol "define", LCons (LSymbol _ as var, LCons (value, LNil))), rest) ->
     let (defs, rest) = collect_definitions rest in
     LCons (LCons (var, LCons (value, LNil)), defs), rest
  | rest -> LNil, rest

(* Uses collect_definitions to rewrite a lambda body's (define) forms
   into letrec
   see desugar_internal_define
 *)
let make_letrec body =
  let (defs, rest) = collect_definitions body in
  match defs with
  | LNil -> rest
  | _ -> LCons (LCons (LSymbol "letrec", LCons (defs, rest)), LNil)

(* (define (f ...) ...)
   into
   (define f (lambda (...) ...))
   
 *)
let rec desugar_define_function = function
  | LCons (LSymbol "define", LCons (LCons (LSymbol _ as sym, args), body)) ->
     let body = sexpr_map desugar_define_function body in
     let lamb = LCons (LSymbol "lambda", LCons (args, body)) in
     let def = LCons (LSymbol "define", LCons (sym, LCons (lamb, LNil))) in
     def
  | LCons (_, _) as expr ->
     sexpr_map desugar_define_function expr 
  | expr -> expr

(* A lambda form's body must be a sequence of definitions, followed by
   expressions to be evaluated.
   This desugar phase rewrites the definitions (which must be at the start
   of the lambda body) into a letrec form.

   Example:
   (lambda ()
     (define (f) (display "hi"))
     (f)
     (f))
   into:
   (lambda ()
     (letrec
       ((f (lambda () (display "hi"))))
       (f) (f)))  
 *)
let rec desugar_internal_define = function
  | LCons (LSymbol "lambda", LCons (args, body)) ->
     LCons (LSymbol "lambda", LCons (args, (make_letrec body)))
  | LCons (_, _) as expr ->
     sexpr_map desugar_internal_define expr
  | expr -> expr

(* Turn bodies of lambdas and letrec's *)
let rec beginize = function
  | LCons (LSymbol "letrec" as sym, LCons (args, body))
  | LCons (LSymbol "lambda" as sym, LCons (args, body)) ->
     let body = beginize body in
     let body = (match body with
     | LCons (_, LCons (_, _)) as b ->
        LCons (LCons (LSymbol "begin", b), LNil)
     | _ -> body) in
     LCons (sym, LCons (args, body))
  | LCons (_, _) as expr ->
     sexpr_map beginize expr
  | expr -> expr

(* These are helper functions for the logical and/or desugars. *)
let make_single_let sym value body =
  let val_list = LCons (sym, LCons (value, LNil)) in
  let full_list = LCons (val_list, LNil) in
  list_to_sexpr
    [LSymbol "let"; full_list; body]

let make_if cond t e =
  list_to_sexpr
    [LSymbol "if"; cond; t; e]

let make_letif sym value cond t e =
  make_single_let sym value
    (make_if cond t e)

(*
  (or a b)
  turns into
  (let ((__generated_or1 a))
   (if __generated_or1
    __generated_or1
    (let ((__generated_or2 b))
     (if __generated_or2
       __generated_or2
       ()))))
 *)
let rec desugar_logical_or = function
  | LCons (LSymbol "or", LCons (f, rest)) ->
     let sym = LSymbol (Gensym.gensym "or") in
     let f = desugar_logical_or f in
     let rest = LCons (LSymbol "or", rest) in
     make_letif sym f sym sym (desugar_logical_or rest)
  | LCons (LSymbol "or", LNil) ->
     LNil (* TODO: Change this when/if you add #t/#f *)
  | LCons (_, _) as expr ->
     sexpr_map desugar_logical_or expr
  | expr -> expr

let rec desugar_logical_and = function
  | LCons (LSymbol "and", LCons (first, rest)) ->
     let sym = LSymbol (Gensym.gensym "and") in
     let first = desugar_logical_and first in
     let rest = LCons (LSymbol "and", rest) in
     make_letif sym first sym (desugar_logical_and rest) sym
  | LCons (LSymbol "and", LNil) ->
     LSymbol "t" (* TODO: change this when/if you add #t/#f *)
  | LCons (_, _) as expr ->
     sexpr_map desugar_logical_and expr
  | expr -> expr

let rec cond_helper = function
  | LCons (LCons (condition, then_), rest) ->
     (* we need to desugar recursively, here as well. *)
     let condition = desugar_cond condition in 
     let then_ = desugar_cond then_ in
     make_if condition (LCons (LSymbol "begin", then_)) (cond_helper rest)
  | LNil -> LNil
  | _ -> failwith "improper cond!" 

and desugar_cond = function
  | LCons (LSymbol "cond", (LCons (_, _) as conditions)) ->
     cond_helper conditions
  | LCons (_, _) as expr ->
     sexpr_map desugar_cond expr
  | expr -> expr
     

let desugar x =
  x
  |> desugar_define_function
  |> desugar_internal_define
  |> beginize
  |> desugar_logical_or
  |> desugar_logical_and
  |> desugar_cond

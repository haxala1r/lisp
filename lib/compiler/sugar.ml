

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

let make_letrec body =
  let (defs, rest) = collect_definitions body in
  LCons (LSymbol "letrec", LCons (defs, rest))
  

(* (define (f ...) ...)
   into
   (define f (lambda (...) ...))
   
 *)
let rec desugar_define_function = function
  | LCons (LSymbol "define", LCons (LCons (LSymbol _ as sym, args), body)) ->
     let body = sexpr_map desugar body in
     let lamb = LCons (LSymbol "lambda", LCons (args, body)) in
     let def = LCons (LSymbol "define", LCons (sym, LCons (lamb, LNil))) in
     def
  | LCons (_, _) as expr ->
     sexpr_map desugar_define_function expr 
  | expr -> expr


and desugar_internal_define = function
  | LCons (LSymbol "lambda", LCons (args, body)) ->
     LCons (LSymbol "lambda", LCons (args, (LCons (make_letrec body, LNil))))
  | LCons (_, _) as expr ->
     sexpr_map desugar_internal_define expr
  | expr -> expr
(* Turn all lambda and define bodies into begins *)
and beginize = function
  | LCons (LSymbol "lambda", LCons (args, body)) ->
     let body = (match body with
     | LCons (_, LCons (_, _)) as b ->
        LCons (LSymbol "begin", b)
     | _ -> body) in
     LCons (LSymbol "lambda", LCons (args, body))
  | LCons (_, _) as expr ->
     sexpr_map beginize expr
  | expr -> expr

and desugar x =
  x
  |> desugar_define_function
  |> desugar_internal_define

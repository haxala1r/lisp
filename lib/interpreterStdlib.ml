open Ast;;

let add _ vs =
  let rec aux accum = function
    | LCons (a, b) ->
      (match accum, a with
       | LInt x , LInt y -> aux (LInt (x + y)) b
       | LDouble x, LInt y -> aux (LDouble (x +. (float_of_int y))) b
       | LInt x, LDouble y -> aux (LDouble ((float_of_int x) +. y)) b
       | LDouble x, LDouble y -> aux (LDouble (x +. y)) b
       | _ -> invalid_arg "invalid args to +")
    | LNil -> accum
    | _ -> invalid_arg "invalid args to +"
  in aux (LInt 0) vs
let sub _ vs =
  let rec aux accum = function
    | LNil -> accum
    | LCons (a, b) -> (match accum, a, b with
        | LNil, LDouble x, LNil -> LDouble (-. x)
        | LNil, LInt x, LNil -> LInt (-x)
        | LNil, LDouble _, _
        | LNil, LInt _, _ -> aux a b
        | LInt x, LInt y, _ -> aux (LInt (x - y)) b
        | LInt x, LDouble y, _ -> aux (LDouble ((float_of_int x) -. y)) b
        | LDouble x, LDouble y, _ -> aux (LDouble (x -. y)) b
        | LDouble x, LInt y, _ -> aux (LDouble (x -. (float_of_int y))) b
        | _ -> invalid_arg "invalid argument to -")
    | _ -> invalid_arg "argument to -"
  in aux LNil vs
      


let car _ vs =
  match vs with
  | LCons (LCons (a, _), LNil) -> a
  | _ -> raise (Invalid_argument "car: invalid argument")

let cdr _ vs =
  match vs with
  | LCons (LCons (_, b), LNil) -> b
  | _ -> raise (Invalid_argument "cdr: invalid argument")

let cons _ vs =
  match vs with
  | LCons (a, LCons (b, LNil)) -> LCons (a, b)
  | _ -> invalid_arg "invalid args to cons!"

let lisp_list _ vs = vs


(* This is the special built-in function that allows us to create
a new function.

(bind-function 'sym '(a b) '(+ a b))
*)

(* Binds any value to a symbol, in the *global environment*. *)
let bind_symbol env =
  function
  (* Special case for setting a function to a symbol, if the function
     is a lambda then we turn it into a real "function" by giving it this
     new name *)
  | LCons (LQuoted (LSymbol s), LCons (LLambda (e, l, b), LNil))
  | LCons (LSymbol s, LCons (LLambda (e, l, b), LNil)) ->
    let f = LFunction (s, e, l, b) in
    env_set_global env s f;
    f
  | LCons (LQuoted (LSymbol s), LCons (v, LNil))
  | LCons (LSymbol s, LCons (v, LNil)) ->
    env_set_global env s v;
    v
  | _ -> raise (Invalid_argument "invalid args to set!")
           

let lambda env = function
  | LCons (l, body) ->
    LLambda (env, l, body)
  | _ -> raise (Invalid_argument "invalid args to lambda!")

let lambda_macro env = function
  | LCons (l, body) -> LUnnamedMacro (env, l, body)
  | _ -> invalid_arg "invalid args to lambda-macro"

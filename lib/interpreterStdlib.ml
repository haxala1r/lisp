open Ast;;

let iadd _ vs : lisp_val =
  let rec auxi vs accum = 
    match vs with
    | LCons (LInt a, b) -> (auxi b (accum + a))
    | LCons (LDouble a, b) -> (auxf b ((float_of_int accum) +. a))
    | _ -> LInt accum
  and auxf vs accum =
    match vs with
    | LCons (LInt a, b) -> (auxf b (accum +. (float_of_int a)))
    | LCons (LDouble a, b) -> (auxf b (accum +. a))
    | _ -> LDouble accum
  in (auxi vs 0);;
let isub _ vs =
  let rec auxi vs accum = 
    match vs with
    | LNil -> LInt accum
    | LCons (LInt a, b) -> auxi b (accum - a)
    | LCons (LDouble a, b) -> auxf b ((float_of_int accum) -. a)
    | _ -> raise (Invalid_argument "-: invalid argument to subtraction operator")
  and auxf vs accum = 
    match vs with
    | LNil -> LDouble accum
    | LCons (LInt a, b) -> auxf b (accum -. (float_of_int a))
    | LCons (LDouble a, b) -> auxf b (accum -. a)
    | _ -> raise (Invalid_argument "-: invalid argument to subtraction operator")
  in
  match vs with
  | LCons (LInt a, LNil) -> LInt (-a)
  | LCons (LInt a, b) -> auxi b a
  | LCons (LDouble a, LNil) -> LDouble (-. a)
  | LCons (LDouble a, b) -> auxf b a
  | _ -> auxi vs 0;;


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

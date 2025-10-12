open Ast;;
open InterpreterStdlib;;


let default_env: environment = [Hashtbl.create 1024];;

let add_builtin s f =
  env_add default_env s (LBuiltinFunction (s, f))
let () = add_builtin "+" iadd
let () = add_builtin "-" isub
let () = add_builtin "car" car
let () = add_builtin "cdr" cdr
let () = add_builtin "bind-function" bind_function

let make_env () = [Hashtbl.copy (List.hd default_env)]

(* the type annotations are unnecessary, but help constrain us from a
potentially more general function here *)
let rec eval_sym (env: environment) (s: string) =
  match env with
  | [] -> raise (Invalid_argument (Printf.sprintf "eval_sym: symbol %s has no value in current scope" s))
  | e :: rest ->
    match Hashtbl.find_opt e s with
    | None -> eval_sym rest s
    | Some v -> v

let rec eval_one env v = 
  match v with
  | LInt x -> LInt x
  | LDouble x -> LDouble x
  | LString s -> LString s
  | LSymbol s -> eval_sym env s
  | LNil -> LNil
  | LCons (func, args) -> eval_call env func args
  | LBuiltinFunction (n, f) -> LBuiltinFunction (n, f)
  | LFunction (n, l, f) -> LFunction (n, l, f)
  | LMacro (n, l, f) -> LMacro (n, l, f)
  | LQuoted v -> v

(* Evaluate a list of values, without evaluating the resulting
function or macro call. Since macros and functions inherently
look similar, they share a lot of code, which is extracted here *)
and eval_list env l =
  match l with
  | LNil -> LNil
  | LCons (a, b) -> LCons (eval_one env a, eval_list env b)
  | _ -> raise (Invalid_argument "eval_list: cannot process non-list")

and eval_body env body =
  match body with
  | LNil -> LNil
  | LCons (form, LNil) -> eval_one env form
  | LCons (form, next) -> 
    ignore (eval_one env form); eval_body env next
  | _ -> LNil

and eval_apply env func args =
  let lexical_env = env_new_lexical env in
  let rec bind_one s a =
    match a with
    | LNil -> raise (Invalid_argument "not enough arguments supplied to function")
    | LCons (value, resta) ->
      env_add lexical_env s value; resta
    | _ -> raise (Invalid_argument "invalid argument list")
  and bind_args l a =
    match l with
    | LNil -> (match a with
      | LNil -> ()
      | _ -> raise (Invalid_argument "too many arguments supplied to function"))
    | LCons (LSymbol sym, LSymbol restl)->
      env_add lexical_env restl (bind_one sym a)
    | LCons (LSymbol sym, restl) ->
      bind_args restl (bind_one sym a)
    | _ -> raise (Invalid_argument "Failure while binding arguments")
  in match func with
  | LFunction (_, l, b) ->
    bind_args l args;
    eval_body lexical_env b
  | LMacro (_, l, b) ->
    bind_args l args;
    eval_body lexical_env b
  | _ -> LNil
    


and eval_call env func args =
  match eval_one env func with
  | LBuiltinFunction (_, f) -> f env (eval_list env args)
  | LFunction (n, l, b) -> eval_apply env (LFunction (n, l, b)) (eval_list env args)
  | LMacro (n, l, b) -> eval_apply env (LMacro (n, l, b)) args
  | v -> raise (Invalid_argument 
        (Printf.sprintf "eval_apply: cannot call non-function object %s" (dbg_print_one v)));;

let eval_all env vs =
  let ev v = eval_one env v in
  List.map ev vs

open Ast;;
open InterpreterStdlib;;


let default_env: environment = [Hashtbl.create 1024];;

let add_builtin s f =
  env_set_global default_env s (LBuiltinFunction (s, f))
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

let rec eval_one env = function
  | LSymbol s -> eval_sym env s
  | LCons (func, args) -> eval_call env (eval_one env func) args
  | LQuoted v -> v
  | v -> v (* All other forms are self-evaluating *)

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
  | LCons (form, next) -> ignore (eval_one env form); eval_body env next
  | _ -> LNil

and err s = raise (Invalid_argument s)
and bind_args env = function
  | LNil -> (function
      | LNil -> () | _ -> err "bind_args")
  | LCons (LSymbol hl, tl) -> (function
      | LCons (ha, ta) -> 
        env_set_local env hl ha;
        bind_args env tl ta;
      | _ -> err "bind_args")
  | _ -> fun _ -> err "bind_args"
  
and eval_apply args = function
  | LFunction (_, e, l, b) ->
    let lexical_env = env_new_lexical e in
    bind_args lexical_env l args;
    eval_body lexical_env b
  | LMacro (_, e, l, b) ->
    let lexical_env = env_new_lexical e in
    bind_args lexical_env l args;
    eval_body lexical_env b
  | v ->
    err ("Non-macro non-function value passed to eval_apply "
        ^ dbg_print_one v); LNil
    


and eval_call env func args =
  match func with
  | LBuiltinFunction (_, f) -> f env (eval_list env args)
  (* The function calls don't happen in the calling environment,
  so it makes no sense to pass env to a call. *)
  | LFunction _ -> eval_apply (eval_list env args) func
  (* Macros are the same, they just return code that *will* be evaluated
  in the calling environment *)
  | LMacro _ -> eval_apply args func
  | v -> raise (Invalid_argument 
        (Printf.sprintf "eval_apply: cannot call non-function object %s" (dbg_print_one v)));;

let eval_all env vs =
  let ev v = eval_one env v in
  List.map ev vs

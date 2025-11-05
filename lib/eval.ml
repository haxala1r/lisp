open Ast;;


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

and bind_args env = function
  | (LNil, LNil) -> ()
  | (LSymbol s, v) -> Env.set_local env s v
  | (LCons (LSymbol hl, tl), LCons (ha, ta)) -> Env.set_local env hl ha; bind_args env (tl, ta)
  | _ -> invalid_arg "cannot bind argument list for function"

and eval_apply args = function
  | LLambda (e, l, b)
  | LFunction (_, e, l, b) ->
    let lexical_env = Env.new_lexical e in
    bind_args lexical_env (l, args);
    eval_body lexical_env b
  | LUnnamedMacro (e, l, b)
  | LMacro (_, e, l, b) ->
    let lexical_env = Env.new_lexical e in
    bind_args lexical_env (l, args);
    eval_body lexical_env b
  | v ->
    invalid_arg ("Non-macro non-function value passed to eval_apply "
        ^ dbg_print_one v)

and eval_call env func args =
  match func with
  | LBuiltinSpecial (_, f) -> f env args
  | LBuiltinFunction (_, f) -> f env (eval_list env args)
  (* The function calls don't happen in the calling environment,
     so it makes no sense to pass env to a call. *)
  | LLambda _
  | LFunction _ -> eval_apply (eval_list env args) func
  (* Macros are the same, they just return code that *will* be evaluated
  in the calling environment *)
  | LUnnamedMacro _
  | LMacro _ -> eval_one env (eval_apply args func)
  | v -> raise (Invalid_argument 
                  (Printf.sprintf "eval_apply: cannot call non-function object %s" (dbg_print_one v)))

let eval_all env vs =
  let ev v = eval_one env v in
  List.map ev vs;;



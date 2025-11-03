open Ast;;
open InterpreterStdlib;;


let default_env: environment = [Hashtbl.create 1024];;

let add_builtin s f =
  Env.set_global default_env s (LBuiltinFunction (s, f))
let add_special s f =
  Env.set_global default_env s (LBuiltinSpecial (s, f))

let make_env () = Env.copy default_env

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
      | LNil -> ()
      | _ -> err "cannot bind arguments")
  | LSymbol s ->
    (function
      | v -> Env.set_local env s v; ())
  | LCons (LSymbol hl, tl) -> (function
      | LCons (ha, ta) -> 
        Env.set_local env hl ha;
        bind_args env tl ta;
      | _ -> err "cannot bind arguments")
  | _ -> fun _ -> err "bind_args"
  
and eval_apply args = function
  | LLambda (e, l, b)
  | LFunction (_, e, l, b) ->
    let lexical_env = Env.new_lexical e in
    bind_args lexical_env l args;
    eval_body lexical_env b
  | LUnnamedMacro (e, l, b)
  | LMacro (_, e, l, b) ->
    let lexical_env = Env.new_lexical e in
    bind_args lexical_env l args;
    eval_body lexical_env b
  | v ->
    err ("Non-macro non-function value passed to eval_apply "
        ^ dbg_print_one v); LNil
    


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

and (* This only creates a *local* binding, contained to the body given. *)
  bind_local env = function
  | LCons (LSymbol s, LCons (v, body)) ->
    let e = Env.new_lexical env in
    Env.set_local e s v;
    eval_body e body
  | _ -> invalid_arg "invalid argument to bind-local"

(* special form that creates a global binding *)
and lisp_define env = function
  | LCons (LSymbol s, LCons (v, LNil)) ->
     let evaluated = eval_one env v in
     Env.set_global env s evaluated;
     evaluated
  | _ -> invalid_arg "invalid args to def"

and lisp_if env = function
  | LCons (cond, LCons (if_true, LNil)) ->
    (match eval_one env cond with
     | LNil -> LNil
     | _ -> eval_one env if_true)
  | LCons (cond, LCons (if_true, LCons (if_false, LNil))) ->
    (match eval_one env cond with
     | LNil -> eval_one env if_false
     | _ -> eval_one env if_true)
  | _ -> invalid_arg "invalid argument list passed to if!"

let eval_all env vs =
  let ev v = eval_one env v in
  List.map ev vs

let () = add_builtin "+" add
let () = add_builtin "-" sub
let () = add_builtin "car" car
let () = add_builtin "cdr" cdr
let () = add_builtin "cons" cons
let () = add_special "def" lisp_define
let () = add_builtin "set" lisp_set
let () = add_builtin "list" lisp_list
let () = add_special "fn" lambda
let () = add_special "fn-macro" lambda_macro
let () = add_special "let-one" bind_local
let () = add_special "quote" (fun _ -> function
             | LCons (x, LNil) -> x
             | _ -> invalid_arg "hmm")
let () = add_special "if" lisp_if
let () = add_builtin "nil?" lisp_not
let () = add_builtin "not" lisp_not (* Yes, these are the same thing *)
(*let () = add_builtin "print" lisp_prin *)

(* I know this looks insane. please trust me.
   Idea: maybe put this in a file instead of putting
   literally the entire standard library in a constant string
 *)
let _ = eval_all default_env (Read.parse_str
"
(def defn
  (fn-macro (name lm . body)
    (list 'def name (cons 'fn (cons lm body)))))
(def defmacro
  (fn-macro (name lm . body)
    (list 'def name (cons 'fn-macro (cons lm body)))))

(defmacro setq (sym val)
 (list 'set (list 'quote sym) val))
(defmacro letfn (sym fun . body)
 (cons 'let-one (cons sym (cons '() (cons (list 'setq sym fun) body)))))


(defn filter (f l)
   (letfn helper
       (fn (l acc)
         (if (nil? l) acc (helper (cdr l) (if (f (car l)) (cons (car l) acc) acc))))
     (helper l '())))
 
")


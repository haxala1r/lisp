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

(* builtin function that updates an existing binding *)
let lisp_set env = function
  | LCons (LSymbol s, LCons (v, LNil)) ->
     Env.update env s v;
     v
  | _ -> invalid_arg "invalid args to set"

let lambda env = function
  | LCons (l, body) ->
    LLambda (env, l, body)
  | _ -> raise (Invalid_argument "invalid args to lambda!")

let lambda_macro env = function
  | LCons (l, body) -> LUnnamedMacro (env, l, body)
  | _ -> invalid_arg "invalid args to lambda-macro";;


let lisp_not _ = function
  | LCons (LNil, LNil) -> LSymbol "t"
  | _ -> LNil;;

(* This only creates a *local* binding, contained to the body given. *)
let bind_local env = function
  | LCons (LSymbol s, LCons (v, body)) ->
    let e = Env.new_lexical env in
    Env.set_local e s v;
    Eval.eval_body e body
  | _ -> invalid_arg "invalid argument to bind-local"

(* special form that creates a global binding *)
let lisp_define env = function
  | LCons (LSymbol s, LCons (v, LNil)) ->
     let evaluated = Eval.eval_one env v in
     Env.set_global env s evaluated;
     evaluated
  | _ -> invalid_arg "invalid args to def"

let lisp_if env = function
  | LCons (cond, LCons (if_true, LNil)) ->
    (match Eval.eval_one env cond with
     | LNil -> LNil
     | _ -> Eval.eval_one env if_true)
  | LCons (cond, LCons (if_true, LCons (if_false, LNil))) ->
    (match Eval.eval_one env cond with
     | LNil -> Eval.eval_one env if_false
     | _ -> Eval.eval_one env if_true)
  | _ -> invalid_arg "invalid argument list passed to if!"



open Env;;

let init_script = "
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
";;

let init_default_env () =
  add_builtin "+" add;
  add_builtin "-" sub;
  add_builtin "car" car;
  add_builtin "cdr" cdr;
  add_builtin "cons" cons;
  add_special "def" lisp_define;
  add_builtin "set" lisp_set;
  add_builtin "list" lisp_list;
  add_special "fn" lambda;
  add_special "fn-macro" lambda_macro;
  add_special "let-one" bind_local;
  add_special "quote" (fun _ -> function
      | LCons (x, LNil) -> x
      | _ -> invalid_arg "hmm");
  add_special "if" lisp_if;
  add_builtin "nil?" lisp_not;
  add_builtin "not" lisp_not; (* Yes, these are the same thing *)

  (*let () = add_builtin "print" lisp_prin *)

  (* I know this looks insane. please trust me.
     Idea: maybe put this in a file instead of putting
     literally the entire standard library in a constant string
   *)
  ignore (Eval.eval_all default_env (Read.parse_str init_script));
  ()

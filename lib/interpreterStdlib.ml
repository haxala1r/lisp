open Ast;;

(* I feel like the more I get into functional programming, the more insane my code
   becomes. What the fuck is this? why do I have a set of functions that combine
   binary operators over an arbitrarily long list? I have like. 4 operators. None
   of this matters.

   But it's just so... beautiful.
 *)
let mathop_do_once int_op float_op = function
  | (LDouble v1, LDouble v2) -> LDouble (float_op v1 v2)
  | (LDouble v1, LInt v2) -> LDouble (float_op v1 (float_of_int v2))
  | (LInt v1, LDouble v2) -> LDouble (float_op (float_of_int v1) v2)
  | (LInt v1, LInt v2) -> LInt (int_op v1 v2)
  | _ -> invalid_arg "invalid arguments to mathematical operator"

let mathop_do_once_curried int_op float_op =
  let f = mathop_do_once int_op float_op in
  fun x -> fun y -> f (x, y)

let mathop_reduce fi ff init vs =
  let curried = mathop_do_once_curried fi ff in
  reduce init curried vs

let cast_int_to_double = function
  | LInt x -> LDouble (float x)
  | LDouble x -> LDouble x
  | _ -> invalid_arg "can't cast_int_to_double!"

let add _ vs =
  mathop_reduce (+) (+.) (LInt 0) vs
let sub _ = function
  | LCons (x, LNil) -> ((mathop_do_once (-) (-.)) (LInt 0, x))
  | LCons (x, rest) -> mathop_reduce (-) (-.) x rest
  | _ -> invalid_arg "invalid argument list passed to (-)"
let mul _ vs =
  mathop_reduce ( * ) ( *. ) (LInt 1) vs
let div _ vs =
  let div_one = mathop_do_once ( / ) ( /. ) in
  match vs with
    (* (/ x) is equal to 1 / x  *)
  | LCons (x, LNil) -> div_one (LDouble 1., cast_int_to_double x)
  | LCons (x, LCons (y, LNil)) -> div_one (cast_int_to_double x, y)
  | _ -> invalid_arg "invalid argument list passed to (/)"

let rem _ = function
  | LCons (x, LCons (y, LNil)) ->
     mathop_do_once (mod) (mod_float) (cast_int_to_double x, cast_int_to_double y)
  | _ -> invalid_arg "invalid argument list passed to (rem)"


let car _ = function
  | LCons (a, _) -> a
  | _ -> invalid_arg "car: non-cons"
let cdr _ = function
  | LCons (_, d) -> d
  | _ -> invalid_arg "cdr: non-cons"
let cons _ a b = LCons (a, b)
let lisp_list _ vs = vs

(* builtin function that updates an existing binding *)
let lisp_set env sym v =
  match sym with
  | LSymbol s -> Env.update env s v; v
  | _ -> invalid_arg ("cannot set non-symbol " ^ dbg_print_one sym)
let lambda env = function
  | LCons (l, body) ->
    LLambda (env, l, body)
  | args -> invalid_arg ("invalid args to fn! " ^ (dbg_print_one args)) 
let defn env = function
  | LCons (LSymbol s, LCons (l, body)) ->
     let f = LFunction (s, env, l, body) in
     Env.set_global env s f; f
  | args -> invalid_arg ("cannot define function! " ^ (dbg_print_one args))

let lambda_macro env = function
  | LCons (l, body) -> LUnnamedMacro (env, l, body)
  | args -> invalid_arg ("invalid args to fn-macro! " ^ (dbg_print_one args)) 
let defmacro env = function
  | LCons (LSymbol s, LCons (l, body)) ->
     let f = LMacro (s, env, l, body) in
     Env.set_global env s f; f
  | args -> invalid_arg ("cannot define macro! " ^ (dbg_print_one args))


let lisp_not _ = function
  | LCons (LNil, LNil) -> LSymbol "t"
  | _ -> LNil;;

(* This only creates a *local* binding, contained to the body given. *)
let bind_local env = function
  | LCons (LSymbol s, LCons (v, body)) ->
    let e = Env.new_lexical env in
    Env.set_local e s (Eval.eval_one env v);
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


let bf s f = s, LBuiltinFunction (s, f)
let bf1 s f =
  let aux e = function
    | LCons (v, LNil) -> f e v
    | _ -> invalid_arg ("invalid argument to " ^ s)
  in bf s aux
let bf2 s f =
  let aux e = function
    | LCons (v1, LCons (v2, LNil)) -> f e v1 v2
    | _ -> invalid_arg ("invalid argument to " ^ s)
  in bf s aux

let sp s f = s, LBuiltinSpecial (s, f)
let sp1 s f =
  let aux e = function
    | LCons (v, LNil) -> f e v
    | _ -> invalid_arg ("invalid argument to " ^ s)
  in sp s aux
let sp2 s f =
  let aux e = function
    | LCons (v1, LCons (v2, LNil)) -> f e v1 v2
    | _ -> invalid_arg ("invalid argument to " ^ s)
  in sp s aux
 

let add_builtins bs =
  List.iter (fun (s, f) -> set_default s f) bs

(*
(def defn
  (fn-macro (name lm . body)
    (list 'def name (cons 'fn (cons lm body)))))
(def defmacro
  (fn-macro (name lm . body)
    (list 'def name (cons 'fn-macro (cons lm body)))))
 *)

let init_script =
"
(defmacro setq (sym val)
 (list 'set (list 'quote sym) val))
(defmacro letfn (sym fun . body)
 (cons 'let-one (cons sym (cons '() (cons (list 'setq sym fun) body)))))

(defn mapcar (f l)
 (if l))
(defn filter (f l)
   (letfn helper
       (fn (l acc)
         (if (nil? l) acc (helper (cdr l) (if (f (car l)) (cons (car l) acc) acc))))
     (helper l '())))
";;

let init_default_env () =
  add_builtins [
      bf "+" add; bf "-" sub;
      bf "*" mul; bf "/" div;
      bf1 "car" car;
      bf1 "cdr" cdr;
      bf2 "cons" cons;
      bf "rem" rem;
      bf2 "set" lisp_set;
      bf "list" lisp_list;
      bf "nil?" lisp_not;
      bf "not" lisp_not;
      
      sp "fn" lambda;
      sp "defn" defn;
      sp "fn-macro" lambda_macro;
      sp "defmacro" defmacro;
      sp "let-one" bind_local;
      sp "def" lisp_define;
      sp1 "quote" (fun _ x -> x);
      sp "if" lisp_if;
    ];

  (*let () = add_builtin "print" lisp_prin *)

  (* I know this looks insane. please trust me.
     Idea: maybe put this in a file instead of putting
     literally the entire standard library in a constant string
   *)
  ignore (Eval.eval_all default_env (Read.parse_str init_script));
  ()

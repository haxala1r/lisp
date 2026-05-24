

type literal = Core_ast.literal

(* primitive operations *)
type primop =
  | Add
  | Sub
  | Mul
  | Div

(* Trivial values *)
type value =
  | Literal of literal
  | Var of string
  (* (lambda (args... k) e)*)
  | Lambda of string list * string * expr
  (* (lambda (v) e), reified continuation *)
  | Cont of string * expr

(* Complex expressions.
   The current continuation and meta-continuation are threaded explicitly
   The equivalents of each node in scheme syntax are written for clarity
 *)
and expr =
  (* (let ((arg v)) e) *)
  | LetVar of string * value * expr
  | Set of string * value * expr
  (* (f args... k) *)
  | App of value * value list * value
  (* (k v) *)
  | CApp of value * value
  | If of value * expr * expr
  | Halt of value

(* debug prints *)
let p = Printf.sprintf
let rec print_literal = function
  | Core_ast.Nil -> p "()"
  | (Int x) -> p "<int %d>" x
  | (Double x) -> p "<double %f>" x
  | (String s) -> p "<string \"%s\">" s
  | (Symbol s) -> p "<symbol %s>" s
  | (Cons (a, b)) -> p "(%s . %s)" (print_literal a) (print_literal b)

let rec print_value = function
  | Literal l -> print_literal l
  | Var s -> p "%s" s
  | Lambda (args, k, b) -> p "<lambda (%s) %s %s>" (List.fold_left (fun x y -> x ^ " " ^ y) "" args) k (print_expr b)
  | Cont (k, e) -> p "<cont %s %s>" k (print_expr e)

and print_expr = function
  | LetVar (s, v, e) -> p "let %s = %s in\n%s" s (print_value v) (print_expr e)
  | Set (s, v, e) -> p "set %s = %s in\n%s" s (print_value v) (print_expr e)
  | App (f, args, k) -> p "(%s %s %s)" (print_value f) (List.fold_left (fun x y -> x ^ " " ^ (print_value y)) "" args) (print_value k)
  | CApp (k, v) -> p "(%s %s)" (print_value k) (print_value v)
  | If (t, th, el) -> p "if %s then %s else %s" (print_value t) (print_expr th) (print_expr el)
  | Halt v -> print_value v

let gensym = Gensym.gensym

let rec cps (e : Core_ast.expression) (k : value -> expr) : expr =
  Core_ast.(
    match e with
    | Literal l -> k (Literal l)
    | Var s -> k (Var s)
    | Lambda (args, e) ->
       let kvar = gensym "k" in
       let e = cps e (fun v -> CApp (Var kvar, v)) in
       k (Lambda (args, kvar, e))
    | Apply (f, args) ->
       let rec aux g acc = function
         | [] -> g acc
         | a :: rest -> cps a (fun a -> aux g (List.append acc [a]) rest) in
       cps f (fun f ->
         aux (fun vs ->
             let res = gensym "result" in
             let cont = Cont (res, k (Var res)) in
             App (f, vs, cont)) [] args)
    | Let (s, e, b) ->
       let c v = LetVar (s, v, cps b k) in
       cps e c
    | If (e1, e2, e3) ->
       let kargsym = gensym "karg" in
       let ksym = gensym "k" in
       let k = Cont (kargsym, k (Var kargsym)) in
       let c v = CApp (Var ksym, v) in
       LetVar (ksym, k, cps e1 (fun v ->
                         If (v, (cps e2 c), (cps e3 c))))
    | Set (s, e) ->
       cps e (fun v -> Set (s, v, k v))
    | Begin [] -> k (Literal Nil)
    | Begin (e :: []) ->
       cps e k
    | Begin (e :: rest) ->
       cps e (fun _ -> cps (Begin rest) k)
  )

let top_level e =
  cps e (fun v -> Halt v)

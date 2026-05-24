

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

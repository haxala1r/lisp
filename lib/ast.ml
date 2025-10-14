type lisp_val = 
  | LInt of int 
  | LDouble of float 
  | LCons of lisp_val * lisp_val
  | LNil
  | LSymbol of string
  | LString of string
  
  (* a builtin function is expressed as a name and the ocaml function
  that performs the operation. The function should take a list of arguments.
  generally, builtin functions should handle their arguments directly,
  and eval forms in the environment as necessary. *)
  | LBuiltinFunction of string * (environment -> lisp_val -> lisp_val)
  | LBuiltinSpecial of string * (environment -> lisp_val -> lisp_val)
  (* a function is a name, captured environment, a parameter list, and function body. *)
  | LFunction of string * environment * lisp_val * lisp_val
  | LLambda of environment * lisp_val * lisp_val
  (* a macro is exactly the same as a function, with the distinction
  that it receives all of its arguments completely unevaluated
  in a compiled lisp this would probably make more of a difference *)
  | LMacro of string * environment * lisp_val * lisp_val
  | LUnnamedMacro of environment * lisp_val * lisp_val
  | LQuoted of lisp_val
and environment = (string, lisp_val) Hashtbl.t list


let env_set_local env s v =
  match env with
  | [] -> ()
  | e1 :: _ ->  Hashtbl.replace e1 s v

let env_new_lexical env = 
  let h = Hashtbl.create 16 in
  h :: env

let rec env_root (env : environment) =
  match env with
  | [] -> raise (Invalid_argument "Empty environment passed to env_root!")
  | e :: [] -> e
  | _ :: t -> env_root t

let env_set_global env s v =
  Hashtbl.replace (env_root env) s v

let env_copy env =
  List.map Hashtbl.copy env

let rec dbg_print_one v =
  let pf = Printf.sprintf in
  match v with
  | LInt x ->  pf "<int: %d>" x
  | LSymbol s -> pf "<symbol: '%s'>" s
  | LString s -> pf "<string: '%s'>" s
  | LNil -> pf "()"
  | LCons (a, b) -> pf "(%s . %s)" (dbg_print_one a) (dbg_print_one b)
  | LDouble d -> pf "<double: %f>" d
  | LBuiltinSpecial (name, _)
  | LBuiltinFunction (name, _) -> pf "<builtin: %s>" name
  | LLambda (_, args, _) -> pf "<unnamed function, lambda-list: %s>"
    (dbg_print_one args)
  | LFunction (name, _, args, _) -> pf "<function: '%s' lambda-list: %s>" 
    name (dbg_print_one args)
  | LUnnamedMacro (_, args, _) -> pf "<unnamed macro, lambda-list: %s>"
    (dbg_print_one args)
  | LMacro (name, _, args, _) -> pf "<macro '%s' lambda-list: %s>"
    name (dbg_print_one args)
  | LQuoted v -> pf "<quote: %s>" (dbg_print_one v)
  (*| _ -> "<Something else>"*)

let dbg_print_all vs =
  let pr v = Printf.printf "%s\n" (dbg_print_one v) in
  List.iter pr vs

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
  (* a function is a name, a parameter list, and function body. *)
  | LFunction of string * lisp_val * lisp_val
  | LQuoted of lisp_val
and environment = (string, lisp_val) Hashtbl.t list


let rec dbg_print_one v =
  let pf = Printf.sprintf in
  match v with
  | LInt x ->  pf "<int: %d>" x
  | LSymbol s -> pf "<symbol: '%s'>" s
  | LString s -> pf "<string: '%s'>" s
  | LNil -> pf "()"
  | LCons (a, b) -> pf "(%s . %s)" (dbg_print_one a) (dbg_print_one b)
  | LDouble d -> pf "<double: %f>" d
  | LBuiltinFunction (name, _) -> pf "<builtin: %s>" name
  | LFunction (name, args, _) -> pf "<function: '%s' lambda-list: %s>" 
    name ((dbg_print_one args))
  | LQuoted v -> pf "<quote: %s>" (dbg_print_one v)
  (*| _ -> "<Something else>"*)

let dbg_print_all vs =
  let pr v = Printf.printf "%s\n" (dbg_print_one v) in
  List.iter pr vs

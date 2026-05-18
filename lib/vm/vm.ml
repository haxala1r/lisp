
module Types = Types
open Types


let do_local state i f =
  match List.nth_opt state.env i with
  | None -> failwith "Invalid index for local access"
  | Some x -> f x
let load_local state i =
  do_local state i (!)
let set_local state i v =
  do_local state i (fun r -> r := v)

let pop_one state =
  match state.stack with
  | v :: rest -> state.stack <- rest; v
  | [] -> failwith ("VM error: cannot pop from empty stack! " )
let pop_args state count =
  let rec aux acc i =
    if i <= 0 then acc
    else aux ((ref (pop_one state)) :: acc) (i - 1)
  in aux [] count
let peek_one state =
  match state.stack with
  | v :: _ -> v
  | [] -> failwith ("VM error: cannot peek on empty stack! " )

let push state v =
  state.stack <- (v :: state.stack)

let trace state =
  let stack () = List.fold_left (fun acc x -> acc ^ " " ^ (Types.print_value x)) "" state.stack in
  let env () = List.fold_left (fun acc x -> acc ^ " " ^ (Types.print_value !x)) "" state.env in
  Printf.printf "%d: \n\tstack: [%s ]\n\tenv:[%s]\n" state.i (stack ()) (env ())

let rec do_apply state arg_count =
  let cur_env = state.env in
  let cur_i = state.i in
  let args = pop_args state arg_count in
  let f = pop_one state in
  match f with
  | Closure (a, _, _) when a != arg_count -> failwith "Wrong argument count to function"
  | Closure (_, x, e) ->
     state.call_stack <- (cur_i, cur_env) :: state.call_stack;
     state.i <- x;
     state.env <- List.append args e;
     interpret state
  | Native x ->
     push state (Native.table.(x) args);
     interpret state
  | _ -> failwith "Cannot apply non-closure object"

and interpret state =
  (*trace state; (*For debug use only*)*)
  let i = state.i in
  state.i <- i + 1;
  (match state.instrs.(i) with
  | Constant x -> push state state.constants.(x) ; interpret state
  | LoadLocal x -> push state (load_local state x) ; interpret state
  | LoadGlobal x -> push state state.globals.(x) ; interpret state
  | StoreLocal x -> set_local state x (peek_one state) ; interpret state
  | StoreGlobal x -> Array.set state.globals x (peek_one state) ; interpret state
  | MakeCons ->
     let cdr = pop_one state in
     let car = pop_one state in
     push state (Cons (car, cdr))
  | Pop -> ignore (pop_one state) ; interpret state
  | Apply a -> do_apply state a
  | MakeClosure (args, x) -> push state (Closure (args, x, state.env)); interpret state
  | Jump target -> state.i <- target ; interpret state
  | JumpF target ->
     (match (pop_one state) with
     | Nil -> state.i <- target
     | _ -> ()); interpret state 
  | End ->
     (match state.call_stack with
     | [] -> ()
     | (old_i, old_env) :: rest ->
        state.call_stack <- rest;
        state.env <- old_env;
        state.i <- old_i;
        interpret state)
  | NOOP -> interpret state
  | Add count ->
     let rec aux sum = function
       | 0 -> sum
       | x ->
          let one = pop_one state in
          aux (Native.numeric_add sum (Native.to_numeric one)) (x-1) in
     push state (Native.of_numeric (aux (Native.NInt 0) count)); interpret state
  | Sub 0 -> failwith ("instruction at index "^(string_of_int state.i)^ ": cannot call '-' on zero args")
  | Sub 1 ->
     let one = pop_one state in
     let one = (match one with
               | Int x -> Int (Int.neg x)
               | Double x -> Double (Float.neg x)
               | _ -> failwith ("cannot subtract non-numeric value: " ^ (print_value one))) in
     push state one; interpret state
  | Sub x ->
     let one = Native.to_numeric (pop_one state) in
     let rec aux res = function
       | 0 -> res
       | x ->
          let one = (Native.to_numeric (pop_one state)) in
          aux (Native.numeric_sub res one) (x-1) in
     push state (Native.of_numeric (aux one (x-1))); interpret state
  | Mul count ->
     let rec aux sum = function
       | 0 -> sum
       | x ->
          let one = pop_one state in
          aux (Native.numeric_mul sum (Native.to_numeric one)) (x-1) in
     push state (Native.of_numeric (aux (Native.NInt 1) count)); interpret state
  | Div 0 -> failwith ("instruction at index "^(string_of_int state.i)^ ": cannot call '/' on zero args")
  | Div 1 ->
     let one = pop_one state in
     let one = (match one with
               | Int x -> Double (1. /. (float_of_int x))
               | Double x -> Double (1. /. x)
               | _ -> failwith ("cannot divide non-numeric value: " ^ (print_value one))) in
     push state one; interpret state
  | Div count ->
     let one = Native.to_numeric (pop_one state) in
     let rec aux res = function
       | 0 -> res
       | x ->
          let one = (Native.to_numeric (pop_one state)) in
          aux (Native.numeric_div res one) (x-1) in
     push state (Native.of_numeric (aux one (count-1))); interpret state)

let make_vm instrs constants globals syms =
  (*let globals = Array.init global_count (fun x -> if x < (Array.length Native.table) then Native x else Nil) in*)
  {
    i = 0;
    instrs = instrs;
    globals = globals;
    constants = constants;
    env = [];
    stack = [];
    call_stack = [];
    symbols = syms
  }

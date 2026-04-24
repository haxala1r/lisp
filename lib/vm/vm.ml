
type value =
  | Int of int
  | Double of float
  | String of string
  | Nil
  | Cons of value * value
  | Symbol of string
  | Closure of int * value ref list
  | NativeClosure of (value -> value)

type instr =  
  | Constant of int
  | LoadLocal of int
  | LoadGlobal of int
  | StoreLocal of int
  | StoreGlobal of int
  | Pop (* discards top of stack *)
  | Apply
  | MakeClosure of int
  | Jump of int
  | JumpF of int (* jump if false. *)
  | End

type vm_state = {
    mutable i : int;
    instrs : instr array;
    globals : value array;
    constants : value array;
    mutable env : value ref list;
    mutable stack : value list
  }

(* TODO: add facilities to print the VM state in case of errors. *)

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
  | [] -> failwith "VM error: cannot pop from empty stack!"

let push state v =
  state.stack <- v :: state.stack

let rec do_apply state =
  let cur_env = state.env in
  let cur_i = state.i in
  let f = pop_one state in
  let arg = pop_one state in
  match f with
  | Closure (x, e) ->
     state.env <- e;
     state.i <- x;
     interpret state;
     state.env <- cur_env;
     state.i <- cur_i
  | NativeClosure f ->
     push state (f arg)
  | _ -> failwith "Cannot apply non-closure object"

and interpret state =
  let i = state.i in
  state.i <- i + 1;
  (match state.instrs.(state.i) with
  | Constant x -> push state state.constants.(x) ; interpret state
  | LoadLocal x -> push state (load_local state x) ; interpret state
  | LoadGlobal x -> push state state.globals.(x) ; interpret state
  | StoreLocal x -> set_local state x (pop_one state) ; interpret state
  | StoreGlobal x -> Array.set state.globals x (pop_one state) ; interpret state
  | Pop -> ignore (pop_one state) ; interpret state
  | Apply -> do_apply state ; interpret state
  | MakeClosure x -> push state (Closure (x, state.env)); interpret state
  | Jump target -> state.i <- target ; interpret state
  | JumpF target ->
     (match (pop_one state) with
     | Nil -> state.i <- target
     | _ -> ()); interpret state 
  | End -> ())


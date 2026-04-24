
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
  | [] -> failwith "VM error: cannot pop from empty stack!"

let push state v =
  state.stack <- v :: state.stack

let rec do_apply state =
  let cur_env = state.env in
  let cur_i = state.i in
  let arg = pop_one state in
  let f = pop_one state in
  match f with
  | Closure (x, e) ->
     state.env <- ref arg :: e;
     state.i <- x;
     interpret state;
     state.env <- cur_env;
     state.i <- cur_i
  | Native x ->
     push state (Native.table.(x) arg)
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
  | MakeCons ->
     let cdr = pop_one state in
     let car = pop_one state in
     push state (Cons (car, cdr))
  | Pop -> ignore (pop_one state) ; interpret state
  | Apply -> do_apply state ; interpret state
  | MakeClosure x -> push state (Closure (x, state.env)); interpret state
  | Jump target -> state.i <- target ; interpret state
  | JumpF target ->
     (match (pop_one state) with
     | Nil -> state.i <- target
     | _ -> ()); interpret state 
  | End -> ())


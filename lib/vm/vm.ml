
type value =
  | Int of int
  | Double of float
  | String of string
  | Symbol of string
  | Nil
  | Cons of value ref * value ref
  | Box of value ref
  | Closure of int * value iarray
  | Continuation of int * value iarray

type access =
  | Global of int
  | Arg of int
  | Env of int

type instr =
  | MkClosure of access * int
  | MkCont of access * int
  | Invoke of access
  | LoadInto of access * access
  | If of access * int * int
  | Add of access list

type vm = {
    mutable i : int;
    instrs : instr iarray;
    mutable globals : value iarray;
    mutable args : value iarray;
    mutable next_args : value array;
    mutable env : value iarray;
    mutable next_env : value array;

    (* A "stack" of continuations.
       This is used exclusively to implement delimited continuations.
     *)
    mutable meta_stack : value list
  }

let do_access vm = function
  | Global i -> Iarray.get vm.globals i
  | Arg i -> Iarray.get vm.args i
  | Env i -> Iarray.get vm.env i

let put vm target v =
  match target with
  | Global _ -> failwith "Cannot modify globals."
  | Arg i -> Array.set vm.next_args i v
  | Env i -> Array.set vm.next_env i v

let is_truthy = function
  | Nil -> false
  | _ -> true

let interpret_one vm =
  let i = vm.i in
  vm.i <- i + 1;
  match Iarray.get vm.instrs i with
  | MkClosure (target, index) ->
     let new_env = Iarray.of_array vm.next_env in
     vm.next_env <- Array.make 32 Nil;
     put vm target (Closure (index, new_env))
  | MkCont (target, index) ->
     let new_env = Iarray.of_array vm.next_env in
     vm.next_env <- Array.make 32 Nil;
     put vm target (Continuation (index, new_env))
  | Invoke acc ->
     (match do_access vm acc with
     | Closure (i, env)
       | Continuation (i, env) ->
        vm.i <- i;
        vm.env <- env
     | _ -> failwith "Cannot invoke non-closure object!")
  | LoadInto (target, src) ->
     put vm target (do_access vm src)
  | If (cond, t, e) ->
     if (is_truthy (do_access vm cond)) then
       vm.i <- t
     else
       vm.i <- e
  | Add _ -> failwith "ADD TRIGGERED"
     

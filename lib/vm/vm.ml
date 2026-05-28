
type value =
  | Int of int
  | Double of float
  | String of string
  | Symbol of string
  | Nil
  | Cons of value ref * value ref
  | Box of value ref
  | Closure of int * value array
  | Continuation of int * value array

let rec print_val = function
  | Int  x -> string_of_int x
  | Double  x -> string_of_float x
  | String  s -> "\"" ^ s ^ "\""
  | Symbol  s -> s
  | Nil -> "nil"
  | Cons (a,b) -> "(" ^ (print_val !a) ^" . " ^ (print_val !b) ^ ")"
  | Box b -> "(box"^ (print_val !b) ^")"
  | Closure (i, _) -> "<closure"^(string_of_int i)^">"
  | Continuation (i, _) -> "<continuation"^(string_of_int i)^">"



type access =
  | Tmp (* loads into/from the singular temporary register *)
  | Global of int
  | Arg of int
  | Env of int

type instr =
  | MkClosure of access * int
  | MkCont of access * int
  | Invoke of access
  | LoadInto of access * access
  | Const of access * int
  | If of access * int * int
  | Halt of access
  | Print
  | Add
  | Sub
  | Mul
  | Div
  | PushMeta of access
  | MetaReturn of access



type vm = {
    mutable i : int;
    instrs : instr iarray;
    globals : value array;
    mutable constants : value iarray;
    mutable args : value array;
    mutable next_args : value Dynarray.t;
    mutable env : value array;
    mutable next_env : value Dynarray.t;

    mutable tmp : value;

    (* A "stack" of continuations.
       This is used exclusively to implement delimited continuations.
     *)
    mutable meta_stack : value list
  }

let string_of_acc = function
  | Tmp -> "(Tmp)"
  | Global i -> "(Global "^string_of_int i^")"
  | Arg i -> "(Arg "^string_of_int i ^")"
  | Env i -> "(Env "^string_of_int i ^")"

let string_of_instr = function
  | MkClosure (a, i) -> "MkClosure "^string_of_acc a^", "^string_of_int i
  | MkCont (a, i) -> "MkCont "^string_of_acc a^", "^ string_of_int i
  | Invoke a -> "Invoke "^string_of_acc a
  | LoadInto (a1, a2) -> "LoadInto "^string_of_acc a1^", "^string_of_acc a2
  | Const (a, i) -> "Const "^string_of_acc a^", "^string_of_int i
  | If (a, i1, i2) -> "If "^string_of_acc a^", "^string_of_int i1^", "^string_of_int i2
  | Halt a -> "Halt "^string_of_acc a
  | Print -> "Print"
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | PushMeta a -> "PushMeta "^string_of_acc a
  | MetaReturn a -> "MetaReturn "^string_of_acc a

let print_instrs state =
  Iarray.iteri (fun i ins -> print_endline (string_of_int i^": "^string_of_instr ins)) state.instrs

let print_vm vm =
  print_endline "constants:";
  Iarray.iteri (fun i v -> print_string (string_of_int i ^": "^print_val v^" | " )) vm.constants;
  print_endline "globals:";
  Array.iteri (fun i v -> print_string (string_of_int i ^": "^print_val v^" | " )) vm.globals;
  print_endline "args:";
  Array.iteri (fun i v -> print_string (string_of_int i ^": "^print_val v^" | " )) vm.args;
  print_endline "env:";
  Array.iteri (fun i v -> print_string (string_of_int i ^": "^print_val v^" | " )) vm.env;
  print_endline ""


let rec add_dynarray arr i v =
  if (Dynarray.length arr) <= i then
    (Dynarray.add_last arr Nil; add_dynarray arr i v)
  else Dynarray.set arr i v
let do_access vm = function
  | Tmp -> vm.tmp
  | Global i -> vm.globals.(i)
  | Arg i -> Array.get vm.args i
  | Env i -> Array.get vm.env i
let put vm target v =
  match target with
  | Tmp -> vm.tmp <- v
  | Global i -> Array.set vm.globals i v
  | Arg i -> add_dynarray vm.next_args i v
  | Env i -> add_dynarray vm.next_env i v

let pop_meta vm =
  match vm.meta_stack with
  | [] -> failwith "Tried to pop from empty meta-stack"
  | one :: rest ->
     vm.meta_stack <- rest;
     one

let is_truthy = function
  | Nil -> false
  | _ -> true

let rec interpret vm =
  let i = vm.i in
  print_vm vm;
  print_endline (string_of_int i);
  vm.i <- i + 1;
  if (i >= Iarray.length vm.instrs) then ()
  else
  match Iarray.get vm.instrs i with
  | MkClosure (target, index) ->
     let new_env = Dynarray.to_array vm.next_env in
     vm.next_env <- Dynarray.create ();
     put vm target (Closure (index, new_env));
     interpret vm
  | MkCont (target, index) ->
     let new_env = Dynarray.to_array vm.next_env in
     vm.next_env <- Dynarray.create ();
     put vm target (Continuation (index, new_env));
     interpret vm
  | Invoke acc ->
     (match do_access vm acc with
     | Closure (i, env)
       | Continuation (i, env) ->
        vm.args <- Dynarray.to_array vm.next_args;
        vm.next_args <- Dynarray.create ();
        vm.i <- i;
        vm.env <- env
     | v -> failwith ("Cannot invoke non-closure object: " ^ print_val v));
     interpret vm
  | LoadInto (target, src) ->
     put vm target (do_access vm src); interpret vm
  | Const (dest, i) ->
     put vm dest (Iarray.get vm.constants i); interpret vm
  | If (cond, t, e) ->
     (if (is_truthy (do_access vm cond)) then
       vm.i <- t
     else
       vm.i <- e); interpret vm
  | Halt acc ->
     let v = do_access vm acc in
     print_endline (print_val v)
  | Print -> failwith "PRINT TRIGGERED"
  | Add -> failwith "ADD TRIGGERED"
  | Sub -> failwith "SUB TRIGGERED"
  | Mul -> failwith "MUL"
  | Div -> failwith "DIV"
  | PushMeta acc ->
     vm.meta_stack <- ((do_access vm acc) :: vm.meta_stack);
     interpret vm
  | MetaReturn acc ->
     let cont = pop_meta vm in
     let arg = do_access vm acc in
     put vm (Arg 0) arg;
     (match cont with
     | Closure (i, env)
       | Continuation (i, env) ->
        vm.args <- Dynarray.to_array vm.next_args;
        vm.next_args <- Dynarray.create ();
        vm.i <- i;
        vm.env <- env;
     | v -> failwith ("cannot metareturn on "^print_val v));
     interpret vm

  

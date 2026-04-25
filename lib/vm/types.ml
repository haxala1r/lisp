
type value =
  | Int of int
  | Double of float
  | String of string
  | Nil
  | Cons of value * value
  | Symbol of string
  | Closure of int * value ref list
  | Native of int (* This is basically a syscall, each ID represents a primitive operation
                     that should have a well-defined effect. These will be further detailed
                     in the language documentation
                   *)

type instr =  
  | Constant of int
  | LoadLocal of int
  | LoadGlobal of int
  | StoreLocal of int
  | StoreGlobal of int
  | MakeCons
  | Pop (* discards top of stack *)
  | Apply
  | MakeClosure of int
  | Jump of int
  | JumpF of int (* jump if false. *)
  | End
  | NOOP

type vm_state = {
    mutable i : int;
    instrs : instr array;
    globals : value array;
    constants : value array;
    mutable env : value ref list;
    mutable stack : value list;
    mutable call_stack : (int * (value ref list)) list;
  }


let p = Printf.sprintf 
let print_one = function
    | Constant i -> p "CONSTANT %d\n" i
    | LoadLocal i -> p "LOCAL %d\n" i
    | LoadGlobal i -> p "GLOBAL %d\n" i
    | StoreLocal i -> p "STORE_LOCAL %d\n" i
    | StoreGlobal i -> p "STORE_GLOBAL %d\n" i
    | MakeCons -> p "CONS\n"
    | Pop -> p "POP\n"
    | Apply -> p "APPLY\n"
    | MakeClosure i -> p "MKCLOSURE %d\n" i
    | Jump i -> p "JMP %d\n" i
    | JumpF i -> p "JMPF %d\n" i
    | End -> p "END\n"
    | NOOP -> p "NOOP\n"

let print_instrs instrs =  
  Array.mapi_inplace
    (fun i ins ->
      print_string (p "%d: %s" i (print_one ins));
      ins)
    instrs

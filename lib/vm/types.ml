
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

type vm_state = {
    mutable i : int;
    instrs : instr array;
    globals : value array;
    constants : value array;
    mutable env : value ref list;
    mutable stack : value list
  }

(* TODO: add facilities to print the VM state in case of errors. *)

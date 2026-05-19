module SymbolTable = Map.Make(String)

type value =
  | Int of int
  | Double of float
  | String of string ref
  | Nil
  | ConsCell of value ref * value ref
  | Symbol of string
  | Closure of { fixed_arity : int; is_variadic : bool; code_index : int; env : value ref list }
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
  | Pop (* discards top of stack *)
  | Apply of int (* arg count *)
  | MakeClosure of int * int (* arg count, code pointer *)
  | Jump of int
  | JumpF of int (* jump if false. *)
  | End
  | NOOP
  (* Arithmetic... and other math. *)
  | Add
  | Sub
  | Negate
  | Mul
  | Div
  | Absolute
  | Modulo
  | Remainder
  (* List operations *)
  | Cons
  | Car
  | Cdr
  | SetCar
  | SetCdr
  (* Predicates *)
  | IsNil
  | IsCons
  | IsSymbol

type vm_state = {
    mutable i : int;
    mutable instrs : instr array;
    mutable globals : value array;
    mutable constants : value array;
    mutable env : value ref list;
    mutable stack : value list;
    mutable call_stack : (int * (value ref list)) list;
    mutable symbols : int SymbolTable.t
  }


let make_global_closure arity code =
  Closure {fixed_arity = arity; is_variadic=false; code_index=code; env=[]}

let p = Printf.sprintf 

let rec print_value = function
    | Int x -> p "%d" x
    | Double x -> p "%f" x
    | String x -> p "\"%s\"" !x
    | Nil -> p "'()"
    | ConsCell (a, b) -> p "(%s . %s)" (print_value !a) (print_value !b)
    | Symbol x -> p "'%s" x
    | Closure c ->
       p "<closure of %d%s args at %d>"
         c.fixed_arity
         (if c.is_variadic then " (or more)" else "")
         c.code_index
    | Native i -> p "<native %d>" i


let print_one = function
    | Constant i -> p "CONSTANT %d\n" i
    | LoadLocal i -> p "LOCAL %d\n" i
    | LoadGlobal i -> p "GLOBAL %d\n" i
    | StoreLocal i -> p "STORE_LOCAL %d\n" i
    | StoreGlobal i -> p "STORE_GLOBAL %d\n" i
    | Pop -> p "POP\n"
    | Apply i -> p "APPLY %d\n" i
    | MakeClosure (a, i) -> p "MKCLOSURE %d, %d\n" a i
    | Jump i -> p "JMP %d\n" i
    | JumpF i -> p "JMPF %d\n" i
    | End -> p "END\n"
    | NOOP -> p "NOOP\n"
    (* Math *)
    | Add -> p "ADD\n"
    | Sub -> p "SUB\n"
    | Negate -> p "NEGATE\n"
    | Mul -> p "MUL\n"
    | Div -> p "DIV\n"
    | Absolute -> p "ABS\n"
    | Modulo -> p "MOD\n"
    | Remainder -> p "REM\n"
    (* List *)
    | Cons -> p "CONS\n"
    | Car -> p "CAR\n"
    | Cdr -> p "CDR\n"
    | SetCar -> p "SET-CAR!\n"
    | SetCdr -> p "SET-CDR!\n"
    (* Predicates *)
    | IsNil -> p "NIL?\n"
    | IsCons -> p "CONS?\n"
    | IsSymbol -> p "SYMBOL?\n"


let print_instrs instrs =  
  Array.mapi_inplace
    (fun i ins ->
      print_string (p "%d: %s" i (print_one ins));
      ins)
    instrs

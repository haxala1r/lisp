
let rec make_vm_val = function
  | Core_ast.Nil -> Vm.Nil
  | Core_ast.Int x -> Vm.Int x
  | Core_ast.Double x -> Vm.Double x
  | Core_ast.String s -> Vm.String s
  | Core_ast.Symbol s -> Vm.Symbol s
  | Core_ast.Cons (a, b) -> Vm.Cons (ref (make_vm_val a), ref (make_vm_val b))

let make_vm_access = function
  | Cps.Global i -> Vm.Global i
  | Cps.Arg i -> Vm.Arg i
  | Cps.Env i -> Vm.Env i

let add (d : 'a Dynarray.t) (i : 'a) =
  let index = Dynarray.length d in
  Dynarray.add_last d i; index


type state = {
    instrs : Vm.instr Dynarray.t;
    consts : Vm.value Dynarray.t;
    labels : (string, int) Hashtbl.t;
    globals : Vm.value Array.t;
  }

let current_i state = Dynarray.length state.instrs

let emit_instr state i =
  ignore (add state.instrs i)

let compile_access state into = function
  | Cps.Global i -> emit_instr state (Vm.LoadInto (into, Vm.Global i))
  | Cps.Arg i -> emit_instr state (Vm.LoadInto (into, Vm.Arg i))
  | Cps.Env i -> emit_instr state (Vm.LoadInto (into, Vm.Env i))

let emit_primop state =
  let e = emit_instr state in
  function
  | Cps.Add -> e Vm.Add
  | _ -> failwith "unknown primitive"

let compile_value state into = function
  | Cps.FLiteral l -> emit_instr state (Vm.Const (into, add state.consts (make_vm_val l)))
  | Cps.FVar a -> emit_instr state (Vm.LoadInto (into, make_vm_access a))
  | Cps.FLambda (s, _, accs) ->
     let i = Hashtbl.find state.labels s in
     List.iteri (fun i a -> compile_access state (Vm.Env i) a) accs;
     emit_instr state (Vm.MkClosure (into, i))
  | Cps.FCont (s, accs) ->
     let i = Hashtbl.find state.labels s in
     List.iteri (fun i a -> compile_access state (Vm.Env i) a) accs;
     emit_instr state (Vm.MkClosure (into, i))
     
let rec compile_one state = function
  | Cps.FApp (f, args, k) ->
     compile_value state (Vm.Tmp) f;
     List.iteri (fun i v -> compile_value state (Vm.Arg i) v) args;
     compile_value state (Vm.Arg (List.length args)) k;
     emit_instr state (Vm.Invoke Vm.Tmp)
  | Cps.FCApp (k, v) ->
     compile_value state (Vm.Tmp) k;
     compile_value state (Vm.Arg 0) v;
     emit_instr state (Vm.Invoke Vm.Tmp)
  | Cps.FHalt v ->
     compile_value state (Vm.Tmp) v;
     emit_instr state (Vm.Halt Vm.Tmp)
  | Cps.FIf (v, e1, e2) ->
     
     compile_value state (Vm.Tmp) v;
     let test_i = current_i state in
     emit_instr state (Vm.Halt Vm.Tmp);
     let then_i = current_i state in
     compile_one state e1;
     let else_i = current_i state in
     compile_one state e2;
     Dynarray.set state.instrs test_i (Vm.If (Vm.Tmp, then_i, else_i))
  | Cps.FPrimitive (prim, args, k) ->
     List.iteri (fun i v -> compile_value state (Vm.Arg i) v) args;
     compile_value state (Vm.Arg (List.length args)) k;
     emit_primop state prim
  | Cps.FHaltIntoGlobal (v, i) ->
     compile_value state (Vm.Global i) v
     (*emit_instr state (Vm.Halt (Vm.Global i))*)

let compile_func state label body =
  let i = current_i state in
  Hashtbl.add state.labels label i;
  compile_one state body

type vm = Vm.vm
let emit (info : Cps.info) : Vm.vm =
  let state = {
      instrs = (Dynarray.create ());
      consts = Dynarray.create();
      labels = Hashtbl.create 256;
      globals = Array.make (Hashtbl.length info.defs) Vm.Nil;
    } in
  Queue.iter (fun (l, _, b) -> compile_func state l b) info.funs;
  let i = current_i state in
  Hashtbl.(Seq.iter (fun (_, e) -> compile_one state e) (to_seq info.defs));
  List.iter (compile_one state) info.toplevel;
  let vm : Vm.vm = {
      i;
      instrs = Iarray.of_array (Dynarray.to_array state.instrs);
      globals = state.globals;
      constants = Iarray.of_array (Dynarray.to_array state.consts);
      args = Iarray.of_list [];
      next_args =Array.make 32 Vm.Nil;
      env = Iarray.of_list [];
      next_env = Array.make 32 Vm.Nil;
      tmp = Vm.Nil;
      meta_stack = [];
    } in vm

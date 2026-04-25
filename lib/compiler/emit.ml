
type literal = Core_ast.literal
type expression = Scope_analysis.expression
module SymbolTable = Scope_analysis.SymbolTable

type instr = Vm.Types.instr

type pre_instr =
  | Instr of instr
  | BackPatchMkClosure
  | BackPatchJumpF

type program = {
    instrs : pre_instr Dynarray.t;
    constants : Vm.Types.value Dynarray.t;
    sym_table : int SymbolTable.t;
    (* This array holds the lambda bodies that we have to compiler later, and
       the index we have to patch the address back into.
     *)
    backpatch : (int * expression) Queue.t;
  }

let ( let* ) = Result.bind

let current_index p =
  Dynarray.length p.instrs
let set_instr p i ins =
  Dynarray.set p.instrs i (Instr ins)

let emit_mkclosure p =
  Ok (Dynarray.add_last p.instrs BackPatchMkClosure)
let emit_jumpf p =
  Ok (Dynarray.add_last p.instrs BackPatchJumpF)

let emit_instr p i =
  Ok (Dynarray.add_last p.instrs (Instr i))

let emit_constant p c =
  Dynarray.add_last p.constants c;
  emit_instr p (Constant ((Dynarray.length p.constants) - 1))
(* evaluating an expression ALWAYS has the effect of pushing exactly
   one element to the stack. For top-level items, this element is
   silently popped.
 *)
let rec compile_one p = function
  | Scope_analysis.Literal (Int x) -> emit_constant p (Vm.Types.Int x)
  | Literal Nil -> emit_constant p (Vm.Types.Nil)
  | Literal (Double x) -> emit_constant p (Vm.Types.Double x)
  | Literal (String s) -> emit_constant p (Vm.Types.String s)
  | Literal (Cons (a, b)) ->
     let* _ = compile_one p (Literal a) in
     let* _ = compile_one p (Literal b) in
     emit_instr p (Vm.Types.MakeCons)
  | Var (Scope_analysis.Local i) ->
     emit_instr p (Vm.Types.LoadLocal i)
  | Var (Global i) ->
     emit_instr p (Vm.Types.LoadGlobal i)
  | Set (Local i, expr) ->
     let* _ = compile_one p expr in
     emit_instr p (Vm.Types.StoreLocal i)
  | Set (Global i, expr) ->
     let* _ = compile_one p expr in
     emit_instr p (Vm.Types.StoreGlobal i)
  | Apply (f, arg) ->
     let* _ = compile_one p f in
     let* _ = compile_one p arg in
     emit_instr p Vm.Types.Apply
  | Lambda body ->
     let* _ = emit_mkclosure p in
     Ok (Queue.push ((Dynarray.length p.instrs) - 1, body) p.backpatch)
  | If (test, t, f) ->
(* *)
     let* _ = compile_one p test in (* compile the expression to be tested *)
     let jumpf_index = current_index p in
     let* _ = emit_jumpf p in (* jump if false, to the false branch*)
     let* _ = compile_one p t in (* true branch *)
     let jump_index = current_index p in
     let* _ = emit_jumpf p in (* jump unconditionally to the common point*)
     let false_index = current_index p in
     let* _ = compile_one p f in (* false branch *)
     let reunite_index = current_index p in
     let* _ = emit_instr p NOOP in
(* Now we can immediately backpatch the dummy instructions we put in place *)
     set_instr p jumpf_index (JumpF false_index);
     set_instr p jump_index (Jump reunite_index);
     Ok ()
  | Begin [] ->
     Error "Cannot compile empty begin "
  | Begin (e1 :: []) ->
     compile_one p e1
  | Begin (e1 :: e2 :: rest) ->
     let* _ = compile_one p e1 in
     compile_one p (Begin (e2 :: rest))

and compile_all p exprs =
  Util.traverse (compile_one p) exprs

(* Once we have compiled the top-level expressions, we must now compile
   all of the lambdas we held off on. Some of these will hold more
   lambdas - that should be fine, they'll just get added to the end
   of the backpatch queue. 
 *)
let backpatch_one p (i, b) =
  Dynarray.set p.instrs i (Instr (MakeClosure (current_index p)));
  let* _ = compile_one p b in
  emit_instr p End
let backpatch p =
  if Queue.is_empty p.backpatch then
    Ok ()
  else backpatch_one p (Queue.pop p.backpatch)

let smooth_one = function
  | Instr i -> i
  | _ -> failwith "backpatching process was not complete!"
let smooth_instrs p =
  Dynarray.to_array (Dynarray.map smooth_one p.instrs)

let compile (exprs : expression list) (tbl : int SymbolTable.t) =
  let program = {
      instrs=Dynarray.create ();
      constants=Dynarray.create ();
      sym_table=tbl;
      backpatch=Queue.create ();
    } in
  let* _ = compile_all program exprs in
  let* _ = backpatch program in
  let final_instrs = smooth_instrs program in
  Ok (Vm.Types.make_vm final_instrs (Dynarray.to_array program.constants) (SymbolTable.cardinal tbl))

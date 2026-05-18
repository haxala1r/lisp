
type literal = Core_ast.literal
type expression = Scope_analysis.expression
module SymbolTable = Scope_analysis.SymbolTable

type instr = Vm.Types.instr

type pre_global =
  | Global of Vm.Types.value
  | DontModify
  | BackPatchClosure
type pre_instr =
  | Instr of instr
  | BackPatchMkClosure of int
  | BackPatchJumpF

type program = {
    instrs : pre_instr Dynarray.t;
    constants : Vm.Types.value Dynarray.t;
    globals : pre_global Dynarray.t;
    sym_table : int SymbolTable.t;
    (* This array holds the lambda bodies that we have to compiler later, and
       the index we have to patch the address back into.
     *)
    backpatch : (int * expression) Queue.t;
    backpatch_const_q : (int * int * expression) Queue.t;
  }

let ( let* ) = Result.bind

let current_index p =
  Dynarray.length p.instrs
let set_instr p i ins =
  Dynarray.set p.instrs i (Instr ins)

let emit_mkclosure p i =
  Ok (Dynarray.add_last p.instrs  (BackPatchMkClosure i))
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
  | Literal (Symbol s) -> emit_constant p (Vm.Types.Symbol s)
  | Literal (Cons (a, b)) ->
     let* _ = compile_one p (Literal a) in
     let* _ = compile_one p (Literal b) in
     emit_instr p (Vm.Types.MakeCons)
  | Var (Scope_analysis.Local i) ->
     emit_instr p (Vm.Types.LoadLocal i)
  | Var (Global i) ->
     emit_instr p (Vm.Types.LoadGlobal i)
  | Var (Intrinsic i) ->
     emit_constant p (Vm.Types.Native i)
  | Set (Local i, expr) ->
     let* _ = compile_one p expr in
     emit_instr p (Vm.Types.StoreLocal i)
  | Set (Global i, expr) ->
     let* _ = compile_one p expr in
     emit_instr p (Vm.Types.StoreGlobal i)
  | Set (Intrinsic _, _) -> failwith "emit: Cannot set! intrinsics!"
  | Apply (Var (Intrinsic 1), args) ->
     let* _ = compile_all_no_pop p args in
     emit_instr p (Vm.Types.Add (List.length args))
  | Apply (Var (Intrinsic 2), args) ->
     let* _ = compile_all_no_pop p (List.rev args) in
     emit_instr p (Vm.Types.Sub (List.length args))
  | Apply (Var (Intrinsic 3), args) ->
     let* _ = compile_all_no_pop p args in
     emit_instr p (Vm.Types.Mul (List.length args))
  | Apply (Var (Intrinsic 4), args) ->
     let* _ = compile_all_no_pop p (List.rev args) in
     emit_instr p (Vm.Types.Div (List.length args))
  | Apply (Var (Intrinsic 5), x :: []) ->
     let* _ = compile_one p x in
     emit_instr p Vm.Types.Absolute
  | Apply (Var (Intrinsic 5), xs) -> failwith ("invalid number of args for abs: " ^(string_of_int (List.length xs)))
  | Apply (Var (Intrinsic 6), x :: y :: []) ->
     let* _ = compile_one p x in
     let* _ = compile_one p y in
     emit_instr p Vm.Types.Modulo
  | Apply (Var (Intrinsic 6), xs) -> failwith ("invalid number of args for mod: " ^(string_of_int (List.length xs)))
  | Apply (Var (Intrinsic 7), x :: y :: []) ->
     let* _ = compile_one p x in
     let* _ = compile_one p y in
     emit_instr p Vm.Types.Remainder
  | Apply (Var (Intrinsic 7), xs) -> failwith ("invalid number of args for rem: " ^(string_of_int (List.length xs)))
  | Apply (Var (Intrinsic x), _) -> failwith ("unknown intrinsic: " ^ (string_of_int x))
  | Apply (f, args) ->
     let* _ = compile_one p f in
     let* _ = compile_all_no_pop p args in
     emit_instr p (Vm.Types.Apply (List.length args))
  | Lambda (arg_count, body) ->
     let* _ = emit_mkclosure p arg_count in
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
     emit_constant p Vm.Types.Nil
  | Begin (e1 :: []) ->
     compile_one p e1
  | Begin (e1 :: e2 :: rest) ->
     let* _ = compile_one p e1 in
     let* _ = emit_instr p Vm.Types.Pop in
     compile_one p (Begin (e2 :: rest))
  | Native i ->
     emit_constant p (Vm.Types.Native i)

and compile_all p exprs =
  Util.traverse
    (fun e ->
      let* _ = compile_one p e in
      emit_instr p Pop) exprs
and compile_all_no_pop p exprs =
  Util.traverse
    (fun e ->
      let* _ = compile_one p e in Ok ()) exprs

(* Once we have compiled the top-level expressions, we must now compile
   all of the lambdas we held off on. Some of these will hold more
   lambdas - that should be fine, they'll just get added to the end
   of the backpatch queue. 
 *)
let backpatch_one_instr p (i, b) =
  match Dynarray.get p.instrs i with
  | BackPatchMkClosure arg_count ->
     Dynarray.set p.instrs i (Instr (MakeClosure (arg_count, current_index p)));
     let* _ = compile_one p b in
     emit_instr p End
  | _ -> failwith "Can't backpatch anything other than a MakeClosure after compilation"
let rec backpatch_instrs p =
  if Queue.is_empty p.backpatch then
    Ok ()
  else
    (let* _ = backpatch_one_instr p (Queue.pop p.backpatch) in
     backpatch_instrs p)
let backpatch_one_const p (i, arg_count, b) =
  let instr_loc = Dynarray.length p.instrs in
  let* _ = compile_one p b in
  let* _ = emit_instr p End in
  Ok (Dynarray.set p.globals i (Global (Vm.Types.Closure (arg_count, instr_loc, []))))
let rec backpatch_consts p =
  if Queue.is_empty p.backpatch_const_q then
    Ok ()
  else
    (let* _ = backpatch_one_const p (Queue.pop p.backpatch_const_q) in
    backpatch_consts p)
let backpatch p =
  let* () = backpatch_instrs p in
  backpatch_consts p


let print_instr = function
  | Instr i -> Vm.Types.print_one i
  | BackPatchJumpF ->  "BACKPATCH JUMPF\n"
  | BackPatchMkClosure i -> "BACKPATCH CLOSURE \n" ^ (string_of_int i)
let print_instrs =
  Array.mapi_inplace (fun i ins ->
    print_endline (Printf.sprintf "%d: %s" i (print_instr ins)); ins)
let smooth_one_instr = function
  | Instr i -> i
  | _ -> failwith "backpatching process was not complete! (instrs)"
let smooth_instrs p =
  Dynarray.to_array (Dynarray.map smooth_one_instr p.instrs)
let smooth_one_global (prev_vm : Vm.Types.vm_state option) i = function
  | Global c -> c
  | DontModify ->
     (match prev_vm with
     | Some prev_vm -> prev_vm.globals.(i)
     | None -> Nil)
  | _ -> failwith "backpatching process was not complete! (consts)"
let smooth_globals prev_vm p =
  Dynarray.to_array (Dynarray.mapi (smooth_one_global prev_vm) p.globals)

let rec constantify = function
  | Core_ast.Nil  -> Vm.Types.Nil
  | Core_ast.Int x -> Vm.Types.Int x
  | Core_ast.String s -> Vm.Types.String s
  | Core_ast.Double x -> Vm.Types.Double x
  | Core_ast.Cons (a, b) -> Vm.Types.Cons (constantify a, constantify b)
  | Core_ast.Symbol s -> Vm.Types.Symbol s
let make_globals (_prev_vm : Vm.Types.vm_state option) (tbl : (int * expression option) SymbolTable.t) =
  let global_count = ((SymbolTable.cardinal tbl)) in 
  let globals = Dynarray.make global_count DontModify in
  let to_backpatch = Queue.create () in
  let () = SymbolTable.iter
             (fun _ (i, v) ->
               match v with
               | Some v ->
                  Dynarray.set globals i (match v with
                    | Scope_analysis.Lambda (a, b) -> Queue.add (i, a, b) to_backpatch; BackPatchClosure
                    | Scope_analysis.Literal l -> Global (constantify l)
                    | Native i -> Global (Vm.Types.Native i)
                    | _ -> Global Vm.Types.Nil)
               | None -> ()) tbl in
  (globals, to_backpatch)
  
let make_instrs (prev_vm : Vm.Types.vm_state option) =
  match prev_vm with
  | Some v -> Dynarray.of_array (Array.map (fun i -> Instr i) v.instrs)
  | None -> Dynarray.create ()
let make_consts (prev_vm : Vm.Types.vm_state option) =
  match prev_vm with
  | Some v -> Dynarray.of_array (v.constants)
  | None -> Dynarray.create ()

let compile (prev_vm : Vm.Types.vm_state option) (exprs : expression list) (tbl : (int * expression option) SymbolTable.t) =
  let (globals, backpatch_const_q) = make_globals prev_vm tbl in
  let program = {
      instrs=make_instrs prev_vm;
      constants=make_consts prev_vm;
      globals=globals;
      sym_table=SymbolTable.map (fun (a, _) -> a) tbl;
      backpatch=Queue.create ();
      backpatch_const_q=backpatch_const_q;
    } in
  let* _ = compile_one program (Begin exprs) in
  let* _ = emit_instr program End in
  let* _ = backpatch program in
  let final_instrs = smooth_instrs program in
  let final_globals = smooth_globals prev_vm program in
  (*let () = print_endline "constants:"; Array.iter (fun v -> print_endline(Vm.Types.print_value v)) final_globals in*)
  Ok (final_instrs, (Dynarray.to_array program.constants), final_globals) (*((SymbolTable.cardinal tbl) + 1))*)

let compile_src src =
  let* (exprs, tbl) = Scope_analysis.of_src src in
  let* (is, cs, gs) = (compile None exprs tbl) in
  Ok (Vm.make_vm is cs gs (SymbolTable.map (fun (i, _) -> i) tbl))

let compile_src_into_vm vm src =
  let* (exprs, tbl) = Scope_analysis.of_src_into_vm vm src in
  let i = Array.length vm.instrs in
  let* (is, cs, gs) = compile (Some vm) exprs tbl in
  vm.i <- i; vm.instrs <- is; vm.constants <- cs ; vm.globals <- gs;
  vm.symbols <- (SymbolTable.map (fun (i, _) -> i) tbl);
  Ok (vm)

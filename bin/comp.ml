


(* I don't have any built-in functions at all rn, so we just use a dummy function *)

let rec interpret_cont vm =
  let l = read_line () in
  let vm = Compiler.Emit.compile_src_into_vm vm l in
  match vm with
  | Ok vm ->
     print_endline "=== PROGRAM DISASSEMBLY";
     Vm.Types.print_instrs vm.instrs;
     print_endline "=== PROGRAM OUTPUT";
     Vm.interpret vm; interpret_cont vm
  | Error s -> print_endline s

let interpret_loop () =
  let l  = read_line () in
  let vm = Compiler.Emit.compile_src l in
  match vm with
  | Ok vm ->
     print_endline "=== PROGRAM DISASSEMBLY";
     Vm.Types.print_instrs vm.instrs;
     print_endline "=== PROGRAM OUTPUT";
     Vm.interpret vm; interpret_cont vm
  | Error s -> print_endline s
let _ = interpret_loop ()

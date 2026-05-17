let ( let* ) = Result.bind;;



(* I don't have any built-in functions at all rn, so we just use a dummy function *)


let rec interpret_loop () =
  let l  = read_line () in
  let* vm = Compiler.Emit.compile_src l in
  print_endline "=== PROGRAM DISASSEMBLY";
  Vm.Types.print_instrs vm.instrs;
  print_endline "=== PROGRAM OUTPUT";
  Vm.interpret vm;
  interpret_loop ()
let _ = interpret_loop ()

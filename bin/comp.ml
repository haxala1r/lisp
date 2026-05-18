
let debug = Array.exists (String.equal "-d") Sys.argv

let maybe_debug (vm : Vm.Types.vm_state) = (if debug then
                      (print_endline "=== DISASSEMBLY";
                       Vm.Types.print_instrs vm.instrs;
                       print_endline "=== END DISASSEMBLY"; ())
                    else ())
  

let rec interpret_cont vm =
  print_string ">>> ";
  let l = read_line () in
  let vm = Compiler.Emit.compile_src_into_vm vm l in
  match vm with
  | Ok vm ->
     Vm.interpret vm; print_endline (Vm.Types.print_value (Vm.pop_one vm));
     maybe_debug vm;
     interpret_cont vm
  | Error s -> print_endline s

let interpret_loop () =
  print_string ">>> ";
  let l  = read_line () in
  let vm = Compiler.Emit.compile_src l in
  match vm with
  | Ok vm ->
     Vm.interpret vm; print_endline (Vm.Types.print_value (Vm.pop_one vm));
     maybe_debug vm;
     interpret_cont vm
  | Error s -> print_endline s
let _ = interpret_loop ()

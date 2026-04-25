let ( let* ) = Result.bind;;



(* Try to interpret some test source code. *)
let some_source = "(define (print-three a b c)
                    (print a)
                    (print b)
                    (print c))
                   (print-three 'hello 'world 'there)
                   (print 'fuck)";;
(* I don't have any built-in functions at all rn, so we just use a dummy function *)

let bruh =
  let* vm = Compiler.Emit.compile_src some_source in
  print_endline "=== PROGRAM DISASSEMBLY";
  Vm.Types.print_instrs vm.instrs;
  print_endline "=== PROGRAM OUTPUT";
  Vm.interpret vm;
  Ok ()

let _ = match bruh with
  | Ok _ -> ()
  | Error s -> print_endline s

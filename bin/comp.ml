let ( let* ) = Result.bind;;



(* Try to interpret some test source code. *)
let some_source = "(define (+ a b) b)
                   (print 1)";;
(* I don't have any built-in functions at all rn, so we just use a dummy function *)

let bruh =
  let* vm = Compiler.Emit.compile_src some_source in
  Vm.Types.print_instrs vm.instrs;
  Vm.interpret vm;
  Ok (print_endline "hello")

let _ = match bruh with
  | Ok _ -> ()
  | Error s -> print_endline s

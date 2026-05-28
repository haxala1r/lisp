
let ( let* ) = Result.bind
let src = "
           (let ((x (reset (shift k (print \"exception\") k) (print 'notexecuted))))
            (print 'back)
            (x 15))"
let c = Compiler.Core_ast.of_src src
let _ = (match (
          let* c = c in
          let* i = Compiler.Cps.top_level c in
          let v = Compiler.Emit.emit i in
          Vm.print_instrs v;
          Ok (Vm.interpret v)) with
        | Ok () -> () 
        | Error s -> failwith ("noo: " ^ s))

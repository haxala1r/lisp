
let ( let* ) = Result.bind


let src = "(print (/ 1 2.0))"
let c = Compiler.Core_ast.of_src src
let _ = (match (
          let* c = c in
          let* i = Compiler.Cps.top_level c in
          let v = Compiler.Emit.emit i in
          Vm.print_instrs v;
          Ok (Vm.interpret v)) with
        | Ok () -> () 
        | Error s -> failwith ("noo: " ^ s))

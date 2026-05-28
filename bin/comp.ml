
let ( let* ) = Result.bind
let src = "
           (define (try thunk)
  (reset
    (thunk)))
(define (throw ex)
  (shift k ex))

(define (db-access)
   \"assume that this is some abstract access that may throw\"
   (throw 'fuck))
(try (lambda ()
  (let ((x (db-access)))
     (print x))))
"
let c = Compiler.Core_ast.of_src src
let _ = (match (
          let* c = c in
          let* i = Compiler.Cps.top_level c in
          let v = Compiler.Emit.emit i in
          Vm.print_instrs v;
          Ok (Vm.interpret v)) with
        | Ok () -> () 
        | Error s -> failwith ("noo: " ^ s))

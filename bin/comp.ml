
let ( let* ) = Result.bind
let src = "(define (f x) x) (define (g x) x)
           (if 5 (f 5) (g 6))"
let c = Compiler.Core_ast.of_src src
let _ = (match (
          let* c = c in
          let* i = Compiler.Cps.top_level c in
          Ok (Compiler.Cps.print_info i)) with
        | Ok () -> () 
        | Error s -> failwith ("noo: " ^ s))
let _ = print_endline "hi"

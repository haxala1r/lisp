
let def = Parser.parse_str "(define (f)
                             (let ((x 5))
                              (if t (set! x (+ x 1)))))
                            (define (f)
                              (define (g y) (* y 2))
                              (or (g 5) (g 6)))
                            (cond
                             ((> 1 2) 0)
                             ((> 3 2) 3)
                             (t -1))";;

let ( let* ) = Result.bind;;
let e =
  (*let def = Parser.parse_str "(lambda () (+ x 1) (+ x 1))" in
   *)
  let* top = Compiler.Syntactic_ast.make (List.hd def) in
  Ok (Printf.printf "%s\n" (Compiler.Syntactic_ast.print top))

let _ = match e with
  | Error s -> Printf.printf "%s\n" s
  | _ -> ()


let src = "
           (f (g 5))"
let c = Compiler.Core_ast.of_src src
let _ = print_endline (match c with
  | Ok (Compiler.Core_ast.Expr e :: _) -> Compiler.Cps.print_expr (Compiler.Cps.top_level e)
  | _ -> "fuck")
let _ = print_endline "hi"

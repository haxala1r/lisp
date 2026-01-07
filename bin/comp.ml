
open Parser.Ast;;

let p = Printf.sprintf

let rec dbg_print = function
  | LSymbol s -> p "%s" s
  | LCons (a, LNil) -> p "%s)" (dbg_print_start a)
  | LCons (a, b) -> p "%s %s" (dbg_print_start a) (dbg_print b)
  | LNil -> p "()"
  | LInt i -> p "%d" i
  | LDouble d -> p "%f" d
  | LString s -> p "%s" s

and dbg_print_start = function
  | LCons (_, _) as l -> p "(%s" (dbg_print l)
  | _ as x -> dbg_print x



let def = Parser.parse_str "(define (f)
                             (let ((x 5))
                              (if t (+ x 1))))
                            (define (f)
                              (define (g y) (* y 2))
                              (or (g 5) (g 6)))
                            (cond
                             ((> 1 2) 0)
                             ((> 3 2) 3)
                             (t -1))";;
let desugared = List.map Compiler.Sugar.desugar def
let () = List.iter (fun x -> Printf.printf "%s\n" (dbg_print_start x) ) desugared
let () = print_newline ()

let ( let* ) = Result.bind;;
let e =
  (*let def = Parser.parse_str "(lambda () (+ x 1) (+ x 1))" in
   *)
  let* top = Compiler.Syntactic_ast.make (List.hd def) in
  Ok (Printf.printf "%s\n" (Compiler.Syntactic_ast.print top))

let _ = match e with
  | Error s -> Printf.printf "%s\n" s
  | _ -> ()

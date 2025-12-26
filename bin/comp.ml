
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



let def = Parser.parse_str "(define (f x) (+ x 1))
                            (define (f)
                              (define (g y) (* y 2))
                              (g 5) (g 6))";;
let desugared = List.map Compiler.Sugar.desugar def
let () = List.iter (fun x -> Printf.printf "%s\n" (dbg_print_start x) ) desugared
let () = print_newline ()

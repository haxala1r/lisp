
(* This file implements native functions of the VM runtime.
   Stuff like printing to the screen, file I/O etc will be implemented
   here.
 *)
open Types
let builtin_print (v : Types.value) =
  let p = Printf.sprintf in
  let rec aux_print = function
    | Int x -> p "%d" x
    | Double x -> p "%f" x
    | String x -> p "\"%s\"" x
    | Nil -> p "'()"
    | Cons (a, b) -> p "(%s . %s)" (aux_print a) (aux_print b)
    | Symbol x -> p "'%s" x
    | Closure (i, _) -> p "<closure %d>" i
    | Native i -> p "<native %d>" i in
  print_endline (aux_print v);
  Types.Nil

let table = [|
    builtin_print
  |]


  

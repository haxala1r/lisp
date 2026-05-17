
(* This file implements native functions of the VM runtime.
   Stuff like printing to the screen, file I/O etc will be implemented
   here.
 *)
open Types

type numeric_val =
  | NInt of int
  | NDouble of float

let to_numeric = function
  | Int x -> NInt x
  | Double x -> NDouble x
  | v -> failwith ((print_value v) ^ " is not a numeric value")
let of_numeric = function
  | NInt x -> Int x
  | NDouble x -> Double x
let numeric_add = function
  | NInt x -> (function
               | NInt y -> NInt (x+y)
               | NDouble y -> NDouble ((float_of_int x) +. y))
  | NDouble x -> (function
                  | NInt y -> NDouble (x+.(float_of_int y))
                  | NDouble y -> NDouble (x+.y))


let builtin_print (v : Types.value ref list) =
  List.iter (fun r -> print_endline (print_value !r)) v;
  Types.Nil



let builtin_add (vs : Types.value ref list) =
  of_numeric (List.fold_left numeric_add (NInt 0) (List.map (fun r -> to_numeric !r) vs))

let table = [|
    builtin_print;
    builtin_add
  |]


  

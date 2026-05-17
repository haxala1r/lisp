
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

let numeric_generic fi fd = function
  | NInt x -> (function
               | NInt y -> NInt (fi x y)
               | NDouble y -> NDouble (fd (float_of_int x) y))
  | NDouble x -> (function
                  | NInt y -> NDouble (fd x (float_of_int y))
                  | NDouble y -> NDouble (fd x y)) 
let numeric_add = numeric_generic (+) (+.)
let numeric_sub = numeric_generic (-) (-.)
let numeric_mul = numeric_generic ( * ) ( *. )
let numeric_div = numeric_generic ( / ) ( /. )

let builtin_print (v : Types.value ref list) =
  List.iter (fun r -> print_endline (print_value !r)) v;
  Types.Nil

let builtin_add (vs : Types.value ref list) =
  of_numeric (List.fold_left numeric_add (NInt 0) (List.map (fun r -> to_numeric !r) vs))
let builtin_sub (vs : Types.value ref list) =
  match vs with
  | f :: [] -> of_numeric (match (to_numeric !f) with
               | NInt x -> NInt (Int.neg x)
               | NDouble x -> NDouble (Float.neg x))
  | f :: rest -> of_numeric (List.fold_left numeric_sub (to_numeric !f) (List.map (fun r -> to_numeric !r) rest))
  | [] -> failwith "invalid number of arguments for subtraction"

let builtin_mul vs =
  of_numeric (List.fold_left numeric_mul (NInt 1) (List.map (fun r -> to_numeric !r) vs))



let table = [|
    builtin_print;
    builtin_add;
    builtin_sub;
    builtin_mul
  |]


  


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

let float_of_numeric = function
  | NInt x -> float_of_int x
  | NDouble x -> x

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
let numeric_div x y =
  NDouble ((float_of_numeric x) /. (float_of_numeric y))

module type Num = sig
  type t
  val add : t -> t -> t
  val zero : t
  val rem : t -> t -> t
end

let aux_mod (type a) (module M : Num with type t = a) (x:a) (y:a) =
  let z = M.rem x y in
  if z < M.zero then M.add z y else z
let numeric_mod = numeric_generic (aux_mod (module Int)) (aux_mod (module Float))
let numeric_rem = numeric_generic (Int.rem) (Float.rem)


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
  | [] -> failwith "invalid number of arguments for subtraction: 0"

let builtin_mul vs =
  of_numeric (List.fold_left numeric_mul (NInt 1) (List.map (fun r -> to_numeric !r) vs))

let builtin_div vs =
  match vs with
  | f :: [] -> of_numeric (numeric_div (NDouble 1.0) (to_numeric !f))
  | f :: rest -> of_numeric (List.fold_left numeric_div (to_numeric !f) (List.map (fun r -> to_numeric !r) rest))
  | [] -> failwith "invalid number of arguments for division: 0"

let make_single_func s f = function
  | first :: [] -> f first 
  | v -> failwith ("invalid number of arguments for " ^s ^ ": " ^ (string_of_int (List.length v)))

let make_two_func s f = function
  | first :: second :: [] -> f first second 
  | v -> failwith ("invalid number of arguments for " ^ s ^ ": " ^ (string_of_int (List.length v)))


let builtin_abs = make_single_func "ABS" (fun f -> of_numeric (match to_numeric !f with
                                                     | NInt x -> NInt (Int.abs x)
                                                     | NDouble x -> NDouble (Float.abs x)))
let builtin_mod =
  make_two_func "MOD" (fun x y -> of_numeric (numeric_mod (to_numeric !x) (to_numeric !y)))
let builtin_rem =
  make_two_func "REM" (fun x y -> of_numeric (numeric_rem (to_numeric !x) (to_numeric !y)))

let table = [|
    builtin_print;
    builtin_add;
    builtin_sub;
    builtin_mul;
    builtin_div;
    builtin_abs;
    builtin_mod;
    builtin_rem
  |]


  

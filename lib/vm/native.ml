
(* This file implements native functions of the VM runtime.
   Stuff like printing to the screen, file I/O etc will be implemented
   here.
 *)

open Types

let builtin_print (v : value) =
  ignore v;
  Nil

let table = [|
    builtin_print
  |]



  

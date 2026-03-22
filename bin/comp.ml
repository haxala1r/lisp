let ( let* ) = Result.bind;;

(* Try to interpret some test source code. *)
let some_source = "(define (+ a b) b)
                   (+ 1 2)";;
(* I don't have any built-in functions at all rn, so we just use a dummy function *)

let bruh =
  let* res = Interpreter.Main.interpret_src some_source in
  match res with
  | Int x -> Printf.printf "got %d as result\n" x; Ok ()
  | _ -> Printf.printf "got something else\n" ; Ok ()
let _ =
  match bruh with
  | Error s -> Printf.printf "%s" s
  | _ -> ()

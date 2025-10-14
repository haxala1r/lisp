open Lisp.Ast;;
open Printf;;
open Lisp;;
open Eval;;
open Read;;

let rec repl env c =
  let () = printf ">>> "; Out_channel.flush Out_channel.stdout; in
  match In_channel.input_line c with
  | None -> ()
  | Some l -> 
    let vals = (parse_str l) in
    (* dbg_print_all vals; *)
    dbg_print_all (eval_all env vals);
    Out_channel.flush Out_channel.stdout;
    repl env c;;


let _ = repl (make_env ()) (In_channel.stdin)

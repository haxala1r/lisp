open Lisp.Ast;;
open Printf;;
open Lisp;;
open Env;;
open Eval;;
open Read;;

let () = InterpreterStdlib.init_default_env ()

let rec repl env c =
  let () = printf ">>> "; Out_channel.flush Out_channel.stdout; in
  match In_channel.input_line c with
  | None -> ()
  | Some "exit" -> ()
  | Some l ->
    try
      let vals = (parse_str l) in
      (* dbg_print_all vals; *)
      dbg_print_all (eval_all env vals);
      Out_channel.flush Out_channel.stdout;
      repl env c
    with
    | Invalid_argument s ->
      printf "%s\nResuming repl\n" s;
      repl env c
    | Parser.Error ->
      printf "Expression '%s' couldn't be parsed, try again\n" l;
      repl env c
;;


let () = repl (make_env ()) (In_channel.stdin)


let ( let* ) = Result.bind

let rec loop () =
  match In_channel.input_line stdin with
  | Some src ->
     let* c = Compiler.Core_ast.of_src src in
     let* i = Compiler.Cps.top_level c in
     let v = Compiler.Emit.emit i in
     let () = Vm.interpret v in
     loop ()
  | None ->
     Ok ()

let _ = match loop () with
  | Error s -> print_endline ("error: " ^ s)
  | Ok () -> ()

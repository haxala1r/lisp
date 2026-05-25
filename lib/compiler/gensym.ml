

let counter = ref 0

let reset () = counter := 0
let gensym base =
  incr counter;
  Printf.sprintf "~%s_%d" base !counter

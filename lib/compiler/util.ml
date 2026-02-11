let ( let* ) = Result.bind

let traverse f l =
  let rec aux acc = function
    | x :: xs ->
       let* result = f x in
       aux (result :: acc) xs
    | [] -> Ok (List.rev acc) in
  aux [] l

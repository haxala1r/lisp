let parse_one lb = Parser.prog (Lexer.read) lb

let parse lb = 
  let rec helper () =
    match parse_one lb with
    | None -> []
    | Some (t) -> t :: helper ()
  in
  helper ()

let parse_str s =
  parse (Lexing.from_string s)


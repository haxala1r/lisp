{
open Lexing
open Parse
exception SyntaxError of string

let strip_quotes s = String.sub s 1 (String.length s - 2);;
}

let digit = ['0'-'9']
let number_sign = '-' | '+'
let int = number_sign? digit+
let double = digit* '.' digit+ | digit+ '.' digit*

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

let sym_char = ['a'-'z' 'A'-'Z' '!' '\\' '+' '-' '*' '/' '_' '?']
let sym = sym_char sym_char*

let str = '"' [^'"']* '"'

rule read =
    parse
    | white { read lexbuf }
    | newline { new_line lexbuf; read lexbuf}
    | int { INT (int_of_string (Lexing.lexeme lexbuf))}
    | double { DOUBLE (float_of_string (Lexing.lexeme lexbuf))}
    | sym { SYM (Lexing.lexeme lexbuf)}
    | str { STR (strip_quotes (Lexing.lexeme lexbuf))}
    | '(' { LPAREN }
    | ')' { RPAREN }
    | '\'' { QUOTE }
    | '.' { DOT }
    | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf))}
    | eof { EOF }

%{
    open Ast
%}

%token <int> INT
%token <float> DOUBLE
%token <string> SYM
%token <string> STR
%token LPAREN
%token RPAREN
%token QUOTE
%token EOF

%start <Ast.lisp_val option> prog
%%

prog:
  | EOF { None }
  | e = expr { Some e }
;

expr:
  | i = INT { LInt i }
  | d = DOUBLE {LDouble d}
  | s = SYM { LSymbol s }
  | s = STR { LString (String.uppercase_ascii s) }
  | LPAREN; l = lisp_list_rest { l }
  | QUOTE; e = expr { LQuoted e}
;

lisp_list_rest:
  | RPAREN { LNil }
  | e = expr; lr = lisp_list_rest { LCons (e, lr) }
;

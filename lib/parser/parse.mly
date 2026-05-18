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
%token DOT
%token EOF

%start <lisp_ast option> prog
%%

prog:
  | EOF { None }
  | e = expr { Some e }
;

expr:
  | i = INT { LInt i }
  | d = DOUBLE { LDouble d}
  | s = SYM { LSymbol (String.uppercase_ascii s) }
  | s = STR { LString s}
  | LPAREN; l = lisp_list_rest { l }
  | QUOTE; e = expr { LCons (LSymbol "QUOTE", LCons (e, LNil)) }
;

lisp_list_rest:
  | RPAREN { LNil }
  | DOT; e = expr; RPAREN { e }
  | e = expr; lr = lisp_list_rest { LCons (e, lr) }
;

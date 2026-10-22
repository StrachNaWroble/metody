%token <int> INT
%token <string> IDENT
%token MULT DIV ADD SUB EQ
%token LPAR RPAR
%token LET IN
%token COMMA FST SND
%token EOF

%start <Ast.expr> main

%left ADD SUB
%left MULT DIV

%{
open Ast
%}

%%

main:
  | e = expr; EOF { e }
  ;

expr:
  | LET; x = IDENT; EQ; e1 = expr; IN; e2 = expr
    { Let(x, e1, e2) }
  | e = opexpr { e }
  ;

opexpr:
  | i = INT { Int i }
  | l = opexpr; MULT; r = opexpr { Binop (Mult, l, r) }
  | l = opexpr; DIV; r = opexpr { Binop (Div, l, r) }
  | l = opexpr; ADD; r = opexpr { Binop (Add, l, r) }
  | l = opexpr; SUB; r = opexpr { Binop (Sub, l, r) } 
  | FST; LPAR; l = expr; COMMA; r = expr; RPAR; { Fst ( Pair(l, r) ) }
  | SND; LPAR; l = expr; COMMA; r = expr; RPAR; { Snd ( Pair(l, r) ) }
  | LPAR; l = expr;  COMMA; r = expr; RPAR { Pair (l, r) } (* Zadanie 4. *)
  | LPAR; e = expr; RPAR { e }
  | x = IDENT { Var x }
  ;

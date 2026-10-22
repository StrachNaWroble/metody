%token <int> INT
%token <string> IDENT
%token MULT DIV ADD SUB EQ LT GT LEQ GEQ NEQ EQEQ
%token ARROW
%token LPAR RPAR
%token LET IN IF THEN ELSE TRUE FALSE FUN AND NOT
%token EOF

%start <Ast.expr> main

%left AND
%right NOT
%nonassoc LT GT LEQ GEQ NEQ EQEQ
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
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr
    { If(e1, e2, e3) }
  | LET; x = IDENT; EQ; e1 = expr; IN; e2 = expr
    { Let(x, e1, e2) }
  | FUN x = IDENT; ARROW; e = expr
    { Fun(x, e) }
  | e = opexpr { e }
  ;

opexpr:
  | NOT; e = opexpr { Not e }
  | l = opexpr; AND; r = opexpr { And(l, r) }
  | l = opexpr; MULT; r = opexpr { Binop (Mult, l, r) }
  | l = opexpr; DIV; r = opexpr { Binop (Div, l, r) }
  | l = opexpr; ADD; r = opexpr { Binop (Add, l, r) }
  | l = opexpr; SUB; r = opexpr { Binop (Sub, l, r) }
  | l = opexpr; LT; r = opexpr { Binop (Lt, l, r) }
  | l = opexpr; GT; r = opexpr { Binop (Gt, l, r) }
  | l = opexpr; LEQ; r = opexpr { Binop (Leq, l, r) }
  | l = opexpr; GEQ; r = opexpr { Binop (Geq, l, r) }
  | l = opexpr; NEQ; r = opexpr { Binop (Neq, l, r) }
  | l = opexpr; EQEQ; r = opexpr { Binop (Eq, l, r) }
  | e = app_expr { e }
  ;

app_expr:
  | e1 = app_expr; e2 = simple_expr { App(e1, e2) }
  | e = simple_expr { e }
  ;

simple_expr:
  | LPAR; e = expr; RPAR { e }
  | x = IDENT { Var x }
  | i = INT { Int i }
  | TRUE  { Bool true  }
  | FALSE { Bool false }
  ;

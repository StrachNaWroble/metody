%{
  open Ast
%}

%token <Bigint.t> INT (* Zadanie 2. *)
%token <string> IDENT
%token <string> STRING
%token MULT DIV ADD SUB LT LEQ GT GEQ NEQ EQEQ EQ
%token PLUSPLUS
%token LPAR RPAR LBRACE RBRACE SC
%token TRUE FALSE IF ELSE WHILE PRINT SKIP
%token FOR (* Zadanie 1. *)
%token RBRACK LBRACK
%token HALT
%token EOF

%start <Ast.stmt> main

%nonassoc LT LEQ GT GEQ NEQ EQEQ
%left ADD SUB
%left MULT DIV

%%

main:
  | e = stmts; EOF { e }
  ;

expr:
  | l = expr; MULT; r = expr { Binop (Mult, l, r) }
  | l = expr; DIV; r = expr { Binop (Div, l, r) }
  | l = expr; ADD; r = expr { Binop (Add, l, r) }
  | l = expr; SUB; r = expr { Binop (Sub, l, r) }
  | l = expr; LT; r = expr { Binop (Lt, l, r) }
  | l = expr; LEQ; r = expr { Binop (Leq, l, r) }
  | l = expr; GT; r = expr { Binop (Gt, l, r) }
  | l = expr; GEQ; r = expr { Binop (Geq, l, r) }
  | l = expr; NEQ; r = expr { Binop (Neq, l, r) }
  | l = expr; EQEQ; r = expr { Binop (Eq, l, r) }
  | i = INT { Int i }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | s = STRING { Str s }
  | i = IDENT { Var i }
  | LPAR; e = expr; RPAR { e }
  | PLUSPLUS; x = IDENT { Preincr x }
  | LBRACE; a = expr_list; RBRACE { Array a } (* Zadanie 3. *)
  | x = IDENT; LBRACK; i = expr; RBRACK { ArrayRead (x, i) } (* Zadanie 4. *)
  ;

expr_list: (* Zadanie 3. *)
  | { [] }
  | e = expr { [e] }
  | e = expr; SC; es = expr_list { e :: es }
  ;

stmt:
  | x = IDENT; EQ; e = expr; SC { Assign (x, e) }
  | IF; LPAR; p = expr; RPAR; t = stmt; ELSE; e = stmt { If (p, t, e) }
  | WHILE; LPAR; p = expr; RPAR; b = stmt { While (p, b) }
  | PRINT; LPAR; e = expr; RPAR; SC { Print e }
  | FOR; LPAR; init = for_init; SC; cond = for_cond; SC; step = for_step; RPAR; b = stmt; { Cmp (init, While (cond, Cmp (b, step) ) ) } (* Zadanie 1. *)
  | x = IDENT; LBRACK; i = expr; RBRACK; EQ; e = expr; SC { AssignArray (x, i, e) } (* Zadanie 4. *)
  | HALT; SC { Halt }
  | LBRACE; s = stmts; RBRACE { s }
  | SKIP; SC { Skip }
  ;

for_init: (* Zadanie 1. *)
  | { Skip }
  | i = IDENT; EQ; e = expr { Assign (i, e) }
  ;

for_cond: (* Zadanie 1. *) 
  | { Bool true }
  | e = expr { e }
  ;

for_step: (* Zadanie 1. *)
  | { Skip }
  | i = IDENT; EQ; e = expr { Assign (i, e) }
  ;

stmts:
  | { Skip }
  | s = stmt; ss = stmts { Cmp(s, ss) }

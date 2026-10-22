%{
  open Ast
%}

%token <int> INT
%token MULT DIV ADD SUB
%token LPAR RPAR
%token TRUE FALSE IF THEN ELSE
%token <string> IDENT
%token LET EQ IN
%token EOF
%token COMMA FST SND (* zadanie 4.*)
%token MATCH WITH ARROW (* zadanie 5. *)
%token SUM TO (* zadanie 6. *)

%start <Ast.expr> main

%left ADD SUB
%left MULT DIV

%%

main:
  | e = mixfix; EOF { e }
  ;

mixfix:
  | IF; e1 = mixfix; THEN; e2 = mixfix; ELSE; e3 = mixfix { If (e1,e2,e3) }
  | LET; i = IDENT; EQ; e1 = mixfix; IN; e2 = mixfix { Let (i, e1, e2) }
  | MATCH; p = mixfix; WITH; LPAR; x = IDENT; COMMA; y = IDENT; RPAR; ARROW; e = mixfix { MatchPair (p, x, y, e) } (* zadanie 5. *)
  | SUM; x = IDENT; EQ; n = mixfix; TO; m = mixfix; IN; k = mixfix { Sum(x, n, m, k)} (* zadanei 6. *)
  | e = expr { e }
  ;

expr:
  | i = INT { Int i }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | i = IDENT { Var i }
  | l = expr; MULT; r = expr { Binop (Mult, l, r) }
  | l = expr; DIV; r = expr { Binop (Div, l, r) }
  | l = expr; ADD; r = expr { Binop (Add, l, r) }
  | l = expr; SUB; r = expr { Binop (Sub, l, r) }
  | LPAR; e = mixfix; RPAR { e }
  | LPAR; RPAR { Unit } (* zadanie 3.*)
  | LPAR; e1 = mixfix; COMMA; e2 = mixfix; RPAR { Pair (e1, e2) } (* zadanie 4. *)
  | FST; p = expr { Fst p } (* zadanie 4. *)
  | SND; p = expr { Snd p } (* zadanie 4. *)
  ;

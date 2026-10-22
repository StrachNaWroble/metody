%token <int> INT
%token <string> IDENT
%token MULT DIV ADD SUB EQ LT GT LEQ GEQ NEQ EQEQ
%token ARROW COMMA COLON UNDERSCORE
%token LPAR RPAR
%token LET IN IF THEN ELSE TRUE FALSE FUN AND NOT FST SND
%token KW_INT KW_BOOL
%token RBRACKET LBRACKET LIST
%token EOF

%start <Ast.expr> main

%right ARROW
%left COMMA
%left AND
%right NOT
%nonassoc LT GT LEQ GEQ NEQ EQEQ
%left ADD SUB
%left MULT DIV

%{
open Ast

let make loc data = { loc; data }
%}

%%

main:
  | e = expr; EOF { e }
  ;

type_expr:
  | tp1=type_expr; ARROW; tp2=type_expr { TArrow(tp1, tp2) }
  | tp1=type_expr; MULT; tp2=type_expr { TPair(tp1, tp2) }
  | tp=simple_type_expr { tp }
  ;

simple_type_expr:
  | LPAR; tp=type_expr; RPAR { tp }
  | KW_INT  { TInt  }
  | KW_BOOL { TBool }
  | UNDERSCORE { TWildcard }
  | tp=simple_type_expr; LIST { TList tp }
  ;

type_annot_opt:
  | COLON; tp=simple_type_expr { tp }
  | /* empty */         { TWildcard }
  ;

expr:
  | IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr
    { make $loc (If(e1, e2, e3)) }
  | LET; x = IDENT; EQ; e1 = expr; IN; e2 = expr
    { make $loc (Let(x, e1, e2)) }
  | FUN x = IDENT; tp = type_annot_opt; ARROW; e = expr
    { make $loc (Fun(x, tp, e)) }
  | e = opexpr { e }
  ;

opexpr:
  | NOT; e = opexpr { make $loc (Not e) }
  | l = opexpr; AND;   r = opexpr { make $loc (And(l, r)) }
  | l = opexpr; MULT;  r = opexpr { make $loc (Binop (Mult, l, r)) }
  | l = opexpr; DIV;   r = opexpr { make $loc (Binop (Div,  l, r)) }
  | l = opexpr; ADD;   r = opexpr { make $loc (Binop (Add,  l, r)) }
  | l = opexpr; SUB;   r = opexpr { make $loc (Binop (Sub,  l, r)) }
  | l = opexpr; LT;    r = opexpr { make $loc (Binop (Lt,   l, r)) }
  | l = opexpr; GT;    r = opexpr { make $loc (Binop (Gt,   l, r)) }
  | l = opexpr; LEQ;   r = opexpr { make $loc (Binop (Leq,  l, r)) }
  | l = opexpr; GEQ;   r = opexpr { make $loc (Binop (Geq,  l, r)) }
  | l = opexpr; NEQ;   r = opexpr { make $loc (Binop (Neq,  l, r)) }
  | l = opexpr; EQEQ;  r = opexpr { make $loc (Binop (Eq,   l, r)) }
  | l = opexpr; COMMA; r = opexpr { make $loc (Pair(l, r)) }
  | l = opexpr; COLON; COLON; r = opexpr { make $loc (Cons(l, r)) }
  | e = app_expr { e }
  ;

app_expr:
  | e1 = app_expr; e2 = simple_expr { make $loc (App(e1, e2)) }
  | FST; e = simple_expr { make $loc (Fst e) }
  | SND; e = simple_expr { make $loc (Snd e) }
  | e = simple_expr { e }
  ;

simple_expr:
  | LPAR; e = expr; RPAR { make $loc e.data }
  | x = IDENT { make $loc (Var x) }
  | i = INT { make $loc (Int i) }
  | TRUE  { make $loc (Bool true)  }
  | FALSE { make $loc (Bool false) }
  | LBRACKET; RBRACKET { make $loc Empty_list }
  ;

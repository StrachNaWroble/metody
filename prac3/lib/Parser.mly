%token<string> IDENT
%token<int> NUM
%token LPAR RPAR LBRK RBRK LBRC RBRC
%token ARROW ASTER COLON COMMA EQ GEQ GT HASH LEQ LT MINUS NEQ PLUS SEMI
%token BOOL ELSE FALSE FUN IF IN INT LET READ RETURN THEN TRUE WRITE
%token EOF

%start main
%type<Ast.expr> main

%nonassoc EQ NEQ LT GT LEQ GEQ
%left  PLUS MINUS
%left  ASTER

%%

main:
  | e=expr EOF { e }
  ;

type_expr:
  | INT  { Ast.TInt  }
  | BOOL { Ast.TBool }
  | LBRC; tps=separated_list(COMMA, type_expr); RBRC { Ast.TTuple tps }
  | LPAR; tps=separated_list(COMMA, type_expr); RPAR;
    ARROW; tp=type_expr
      { Ast.TArrow (tps, tp) }
  ;

ident_opt:
  | x=IDENT     { x }
  | /* empty */ { "<anon>" }
  ;

level_opt:
  | LBRK; n=NUM; RBRK { n }
  | /* empty */       { 0 }
  ;

sep:
  | SEMI { () }
  | IN   { () }
  ;

formal_arg:
  | x=IDENT; COLON; tp=type_expr { (x, tp) }
  ;

formal_args:
  | LPAR; args=separated_list(COMMA, formal_arg); RPAR { args }
  ;

expr:
  | IF; e1=expr; THEN; e2=expr; ELSE; e3=expr
      { Ast.If (e1, e2, e3) }
  | LET; x=IDENT; EQ; e1=expr; sep; e2=expr
      { Ast.Let (x, e1, e2) }
  | LET; f=IDENT; args=formal_args; COLON; tp=type_expr; EQ;
    e1=expr; sep; e2=expr
      { Ast.Let (f, Ast.FunRec (f, args, tp, e1), e2) }
  | FUN; f=ident_opt; args=formal_args; ARROW; tp=type_expr; EQ; e=expr
      { Ast.FunRec (f, args, tp, e) }
  | READ; x=IDENT; sep; e=expr   { Ast.Read (x, e) }
  | WRITE; e1=expr; sep; e2=expr { Ast.Write (e1, e2) }
  | RETURN; l=level_opt; e=expr; COLON; tp=type_expr { Ast.Return (l, e, tp) }
  | e=op_expr { e }
  ;

op_expr:
  | e1=op_expr; PLUS;  e2=op_expr { Ast.Binop (Add, e1, e2) }
  | e1=op_expr; MINUS; e2=op_expr { Ast.Binop (Sub, e1, e2) }
  | e1=op_expr; ASTER; e2=op_expr { Ast.Binop (Mul, e1, e2) }
  | e1=op_expr; EQ;    e2=op_expr { Ast.Cmpop (Eq,  e1, e2) }
  | e1=op_expr; NEQ;   e2=op_expr { Ast.Cmpop (Neq, e1, e2) }
  | e1=op_expr; LT;    e2=op_expr { Ast.Cmpop (Lt,  e1, e2) }
  | e1=op_expr; GT;    e2=op_expr { Ast.Cmpop (Gt,  e1, e2) }
  | e1=op_expr; LEQ;   e2=op_expr { Ast.Cmpop (Leq, e1, e2) }
  | e1=op_expr; GEQ;   e2=op_expr { Ast.Cmpop (Geq, e1, e2) }
  | e=app_expr { e }
  ;

app_expr:
  | e=app_expr; LPAR; args=separated_list(COMMA, expr); RPAR
    { Ast.App(e, args) }
  | e=app_expr; HASH; n=NUM { Ast.Select(e, n) }
  | e=expr_simple { e }
  ;

expr_simple:
  | LPAR; e=expr; RPAR { e }
  | LBRC; es=separated_list(COMMA, expr); RBRC { Ast.Tuple es }
  | IDENT              { Ast.Var $1 }
  | NUM                { Ast.Int $1 }
  | TRUE               { Ast.Bool true }
  | FALSE              { Ast.Bool false }
  ; 

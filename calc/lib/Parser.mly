%token EOF
%token <int> INT
%start <Ast.expr> prog

%%

prog:
  | e = expr; EOF { e } 
  ;

expr:
  | i = INT { Int i }
  ;

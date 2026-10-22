%{
  open Ast

  let rec fold_fun (args : ident list) (body : expr) : expr = (* zadanie 1. *)
    match args with 
    | [] -> body
    | x :: xs -> Fun (x, fold_fun xs body)

  let fold_fun_rec (f : ident) (args : ident list) (body : expr) : expr = (* zadanie 1. *)
    match args with 
    | [] -> failwith "no arguments in rec funcion"
    | x :: xs -> Funrec (f, x, fold_fun xs body) 

  let rec fold_tuple (es : expr list) : expr = (* zadanie 2. *)
    match es with 
    | [] | [_] -> failwith ""
    | [e1; e2] -> Pair (e1, e2)
    | e :: es -> Pair (e, fold_tuple es)

  let rec fold_match (e1 : expr) (tuple : ident list) (e2 : expr) : expr = (* zadanie 2. *)
    match tuple with 
    | [] | [_] -> failwith ""
    | [i1; i2] -> Match (e1, i1, i2, e2)
    | i :: is -> Match (e1, i, "temporary", fold_match (Var "temporary") is e2)

%}

%token <int> INT
%token MULT DIV ADD SUB
%token LT LEQ GT GEQ NEQ EQEQ
%token LPAR RPAR COMMA
%token TRUE FALSE IF THEN ELSE
%token <string> IDENT
%token LET EQ IN
%token MATCH WITH ARR
%token FUN FUNREC
%token DEF REC
%token EOF

%start <Ast.expr> main
%start <Ast.def list> program

%nonassoc LT LEQ GT GEQ NEQ EQEQ
%left ADD SUB
%left MULT DIV

%%

idents:
  | i = IDENT; { [i] } (* zadanie 1. *)
  | i = IDENT; is = idents { i :: is } (* zadanie 1. *)
  ;

ident_list: (* zadanie 1. *)
  | { [] }
  | is = idents {is}

tuple: (* zadanie 2. *)
  | e1 = mixfix; COMMA; e2 = mixfix { [e1; e2] }
  | e = mixfix; COMMA; es = tuple { e :: es }

tuple_idents: (* zadanie 2. *)
  | i1 = IDENT; COMMA; i2 = IDENT { [i1; i2] }
  | i = IDENT; COMMA; is = tuple_idents { i :: is }

main:
  | e = mixfix; EOF { e }
  ;

program:
  | EOF { [] }
  | DEF; i = IDENT; args = ident_list; EQ; e = mixfix; p = program { (i, fold_fun args e) :: p } (* zadanie 1. *)
  | DEF; REC; i = IDENT; args = idents; EQ; e = mixfix; p = program { (i, fold_fun_rec i args e) :: p } (* zadanie 1. *)
  ;

mixfix:
  | IF; e1 = mixfix; THEN; e2 = mixfix; ELSE; e3 = mixfix { If (e1,e2,e3) }
  | LET; i = IDENT; EQ; e1 = mixfix; IN; e2 = mixfix { Let (i, e1, e2) }
  | MATCH; e1 = mixfix; WITH; LPAR; t = tuple_idents; RPAR; ARR; e2 = mixfix { fold_match e1 t e2 } (* zadanie 2. *)
  | FUN; args = idents; ARR; e = mixfix { fold_fun args e } (* zadanie 1. *)
  | FUNREC; f = IDENT; args = idents; ARR; e = mixfix { fold_fun_rec f args e } (* zadanie 1. *)
  | e = expr { e }
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
  | e = app { e }
  ;

app:
  | e1 = app; e2 = base { App (e1, e2) }
  | e = base { e }
  ;

base:
  | i = INT { Int i }
  | TRUE { Bool true }
  | FALSE { Bool false }
  | i = IDENT { Var i }
  | LPAR; RPAR { Unit }
  | LPAR; es = tuple; RPAR { fold_tuple es } (* zadanie 2. *)
  | LPAR; e = mixfix; RPAR { e }
  ;


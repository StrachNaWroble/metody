type bop = Mult | Div | Add | Sub 
         | Eq | Neq | Gt | Geq | Lt | Leq

type ident = string

type expr =
  | Int of int
  | Bool of bool
  | Str of string
  | Binop of bop * expr * expr
  | Var of ident

type stmt =
  | Assign of ident * expr
  | Skip
  | Cmp of stmt * stmt
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Print of expr
  | Halt

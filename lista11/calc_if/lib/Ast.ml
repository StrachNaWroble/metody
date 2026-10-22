type bop = Mult | Div | Add | Sub
         | Eq | Neq | Gt | Geq | Lt | Leq

type var = string

type expr =
  | Var   of var
  | Int   of int
  | Bool  of bool
  | If    of expr * expr * expr
  | Binop of bop * expr * expr
  | Let   of var * expr * expr

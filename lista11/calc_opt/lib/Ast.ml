type bop = Mult | Div | Add | Sub
         | Eq | Neq | Gt | Geq | Lt | Leq

type var = string

type expr =
  | Var   of var
  | Int   of int
  | Bool  of bool
  | And   of expr * expr
  | Or of expr * expr
  | While of expr * expr
  | Not   of expr
  | If    of expr * expr * expr
  | Binop of bop * expr * expr
  | Let   of var * expr * expr
  | SetVar of var * expr

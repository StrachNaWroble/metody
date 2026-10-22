type bop = Mult | Div | Add | Sub
         | Eq | Neq | Gt | Geq | Lt | Leq

type ident = string

type expr =
  | Int of int
  | Bool of bool
  | If of expr * expr * expr
  | Binop of bop * expr * expr
  | Unit
  | Pair of expr * expr
  | Match of expr * ident * ident * expr
  | Let of ident * expr * expr
  | Var of ident
  | Fun of ident * expr
  | Funrec of ident * ident * expr
  | App of expr * expr

type def = ident * expr

type bop = Mult | Div | Add | Sub

type var = string

type expr =
  | Var   of var
  | Int   of int
  | Binop of bop * expr * expr
  | Let   of var * expr * expr
  | Pair of expr * expr (* Zadanie 4. *)
  | Fst of expr
  | Snd of expr

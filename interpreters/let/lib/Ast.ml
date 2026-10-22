type bop = Mult | Div | Add | Sub

type ident = string

type expr =
  | Int of int
  | Bool of bool
  | If of expr * expr * expr
  | Binop of bop * expr * expr
  | Let of ident * expr * expr
  | Var of ident
  | Unit (* zadanie 3.*)
  | Pair of expr * expr (* zadanie 4.*)
  | Fst of expr (* zadanie 4.*)
  | Snd of expr (* zadanie 4.*)
  | MatchPair of expr * ident * ident * expr (* zadanie 5. *)
  | Sum of ident * expr * expr * expr (* zadanie 6. *) 
  


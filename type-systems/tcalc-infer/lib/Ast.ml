type bop = Mult | Div | Add | Sub
         | Eq | Neq | Gt | Geq | Lt | Leq

type var = string

type 'a node =
  { loc  : Lexing.position * Lexing.position;
    data : 'a
  }

type tp =
  | TWildcard
  | TInt
  | TBool
  | TArrow of tp * tp
  | TPair  of tp * tp
  | TList of tp

type expr = expr_data node
and expr_data =
  | Var   of var
  | Int   of int
  | Bool  of bool
  | And   of expr * expr
  | Not   of expr
  | If    of expr * expr * expr
  | Binop of bop * expr * expr
  | Let   of var * expr * expr
  | Fun   of var * tp * expr
  | App   of expr * expr
  | Pair  of expr * expr
  | Fst   of expr
  | Snd   of expr
  | Empty_list
  | Cons of expr * expr

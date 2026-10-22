type tp =
  | TInt
  | TBool
  | TTuple  of tp list
  | TArrow  of tp list * tp

type binop = Add | Sub | Mul
type cmpop = Eq | Neq | Lt | Gt | Leq | Geq

type var = string

type expr =
  | Var    of var
  | Int    of int
  | Bool   of bool
  | Binop  of binop * expr * expr
  | Cmpop  of cmpop * expr * expr
  | Let    of var * expr * expr
  | If     of expr * expr * expr
  | Tuple  of expr list
  | Select of expr * int
  | FunRec of var * (var * tp) list * tp * expr
  | App    of expr * expr list
  | Read   of var * expr
  | Write  of expr * expr
  | Return of int * expr * tp

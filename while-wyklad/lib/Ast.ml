type bop = Mult | Div | Add | Sub
         | Eq | Neq | Gt | Geq | Lt | Leq

type ident = string

type expr =
  | Int of Bigint.t (* Zadanie 2. *)
  | Bool of bool
  | Str of string
  | Binop of bop * expr * expr
  | Var of ident
  | Preincr of ident
  | Array of expr list (* Zadanie 3. *)
  | ArrayRead of ident * expr (* Zadanie 4. *)

type stmt =
  | Assign of ident * expr
  | Skip
  | Cmp of stmt * stmt
  | If of expr * stmt * stmt
  | While of expr * stmt
  | Print of expr
  | AssignArray of ident * expr * expr (* Zadanie 4. *)
  | Halt

open VM

module Env : sig
  type t

  val empty    : t
  val add      : t -> Ast.var -> t
  val push_tmp : t -> t
  val lookup   : t -> Ast.var -> int
end = struct
  type t = Ast.var option list

  let empty     = []
  let add env x = Some x :: env
  let push_tmp env = None :: env

  let lookup env x =
    match List.find_index (fun y -> y = Some x) env with
    | Some i -> i
    | None   -> failwith ("Unbound variable " ^ x)
end

let compile_op (op : Ast.bop) =
  match op with
  | Add  -> [ MAdd ]
  | Sub  -> [ MSub ]
  | Div  -> [ MDiv ]
  | Mult -> [ MMul ]

let rec compile env (e : Ast.expr) =
  match e with
  | Var x -> [ MGetLocal (Env.lookup env x) ]
  | Int n -> [ MConst n ]
  | Binop(op, e1, e2) ->
    compile env e1 @
    [ MPush ] @
    compile (Env.push_tmp env) e2 @
    compile_op op
  | Let(x, e1, e2) ->
    compile env e1 @
    [ MPush ] @
    compile (Env.add env x) e2 @
    [ MPopN 1 ]
  | Fst(e) -> compile env e @ [ MGet 0 ] (* Zadanie 4. *)
  | Snd(e) -> compile env e @ [ MGet 1 ]
  | Pair (l, r) -> [ MAlloc 2 ] @  [ MPush ] @ compile (Env.push_tmp env) l @ [ MSet 0 ]
                   @ compile (Env.push_tmp env) r  @ [ MSet 1 ] @ [ MPopAcc ]

let compile_program e =
  compile Env.empty e

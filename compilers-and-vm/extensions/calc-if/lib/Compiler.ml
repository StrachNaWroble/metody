open VM

module Env : sig
  type t

  val empty    : t
  val add      : t -> Ast.var -> t
  val push_tmp : t -> t
  val lookup   : t -> Ast.var -> int
end = struct
  module StrMap = Map.Make(String)
  type t =
    { var_map    : int StrMap.t;
      stack_size : int
    }

  let empty     =
    { var_map    = StrMap.empty;
      stack_size = 0
    }

  let add env x =
    { var_map    = StrMap.add x (env.stack_size + 1) env.var_map;
      stack_size = env.stack_size + 1
    }

  let push_tmp env =
    { env with stack_size = env.stack_size + 1 }

  let lookup env x =
    match StrMap.find_opt x env.var_map with
    | Some i -> env.stack_size - i
    | None   -> failwith ("Unbound variable " ^ x)
end

let next_id = ref 0
let fresh_label () =
  let id = !next_id in
  next_id := id + 1;
  "$L_" ^ string_of_int id

let compile_cmp_op (op : Ast.bop) =
  match op with
  | Lt  -> MC_LT
  | Gt  -> MC_GT
  | Leq -> MC_LE
  | Geq -> MC_GE
  | Neq -> MC_NE
  | Eq  -> MC_EQ
  | _   -> failwith "Not a comparison operator"

let compile_op (op : Ast.bop) =
  match op with
  | Add  -> [ MAdd ]
  | Sub  -> [ MSub ]
  | Div  -> [ MDiv ]
  | Mult -> [ MMul ]
  | Lt | Gt | Leq | Geq | Neq | Eq  ->
    let l1 = fresh_label () in
    let l2 = fresh_label () in
    [ MBranch(compile_cmp_op op, l1);
      MConst 0;
      MJump l2;
      MLabel l1;
      MConst 1;
      MLabel l2
    ]

let rec compile env (e : Ast.expr) =
  match e with
  | Var x -> [ MGetLocal (Env.lookup env x) ]
  | Int n -> [ MConst n ]
  | Bool b -> [ MConst (if b then 1 else 0) ]
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
  | If(e1, e2, e3) ->
    let e3_label = fresh_label () in
    let end_label = fresh_label () in
    compile env e1 @
    [ MBranch(MC_Z, e3_label) ] @
    compile env e2 @
    [ MJump end_label ] @
    [ MLabel e3_label ] @
    compile env e3 @
    [ MLabel end_label ]
(*
  | If(e1, e2, e3) ->
    compile env (Let("$cnd", e1,
      Binop(Add,
        Binop(Mult, Var "$cnd", e2),
        Binop(Mult, Binop(Sub, Var "$cnd", Int 1), e3))))
*)

let compile_program e =
  compile Env.empty e

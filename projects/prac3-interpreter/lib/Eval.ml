open Ast

module Cont : sig
  type 'a t
  type 'a cont

  val return : 'a -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t

  val callcc : ('a cont -> 'a t) -> 'a t
  val throw  : 'a cont -> 'a t -> 'b t

  val run : 'a t -> ('a -> unit) -> unit
end = struct
  type 'a cont = 'a -> unit
  type 'a t = 'a cont -> unit

  let return x cont = cont x
  let bind m f cont = m (fun x -> f x cont)

  let callcc f cont =
    f cont cont

  let throw cont m _ = m cont

  let run m = m
end

let ( let* ) = Cont.bind

type value =
  | VInt   of int
  | VBool  of bool
  | VTuple of value list
  | VFun   of (value list -> value Cont.t)

module Env : sig
  type t
  val empty    : t
  val add      : t -> var -> value -> t
  val enter    : t -> value Cont.cont -> t
  val lookup   : t -> var -> value
  val ret_cont : t -> int -> value Cont.cont
end = struct
  module StrMap = Map.Make(String)
  type t =
    { variables : value StrMap.t;
      ret_conts : value Cont.cont list
    }

  let empty = { variables = StrMap.empty; ret_conts = [] }

  let add env x v =
    { env with variables = StrMap.add x v env.variables }

  let enter env cont =
    { env with ret_conts = cont :: env.ret_conts }

  let lookup env x =
    match StrMap.find_opt x env.variables with
    | Some v -> v
    | None   -> failwith ("Unbound variable: " ^ x)

  let ret_cont env lvl =
    match List.nth_opt env.ret_conts lvl with
    | Some cont -> cont
    | None      -> failwith "Invalid return level"
end

let to_int v =
  match v with
  | VInt n -> n
  | _      -> failwith "Runtime error"

let rec compare_values v1 v2 =
  match v1, v2 with
  | VInt n1,    VInt n2    -> compare n1 n2
  | VInt _,     _          -> failwith "Runtime error"
  | VBool b1,   VBool b2   -> compare b1 b2
  | VBool _,    _          -> failwith "Runtime error"
  | VTuple vs1, VTuple vs2 -> compare_tuples vs1 vs2
  | VTuple _,   _          -> failwith "Runtime error"
  | VFun   _,   _          -> failwith "Runtime error"

and compare_tuples vs1 vs2 =
  match vs1, vs2 with
  | [], [] -> 0
  | v1 :: vs1, v2 :: vs2 ->
    let cmp = compare_values v1 v2 in
    if cmp <> 0 then cmp else compare_tuples vs1 vs2
  | [], _ :: _ | _ :: _, [] -> failwith "Runtime error"

let eval_binop op =
  match op with
  | Add -> ( + )
  | Sub -> ( - )
  | Mul -> ( * )

let eval_cmpop op =
  match op with
  | Eq  -> ( = )
  | Neq -> ( <> )
  | Lt  -> ( < )
  | Gt  -> ( > )
  | Leq -> ( <= )
  | Geq -> ( >= )

let rec eval env prog =
  match prog with
  | Var  x -> Cont.return (Env.lookup env x)
  | Int  n -> Cont.return (VInt  n)
  | Bool b -> Cont.return (VBool b)
  | Binop(op, e1, e2) ->
    let* v1 = eval env e1 in
    let* v2 = eval env e2 in
    Cont.return (VInt (eval_binop op (to_int v1) (to_int v2)))
  | Cmpop(op, e1, e2) ->
    let* v1 = eval env e1 in
    let* v2 = eval env e2 in
    Cont.return (VBool (eval_cmpop op (compare_values v1 v2) 0))
  | Let(x, e1, e2) ->
    let* v1 = eval env e1 in
    eval (Env.add env x v1) e2
  | If(cond, e1, e2) ->
    let* v = eval env cond in
    begin match v with
    | VBool true  -> eval env e1
    | VBool false -> eval env e2
    | _           -> failwith "Runtime error"
    end
  | Tuple es ->
    let* vs = eval_exprs env es in
    Cont.return (VTuple vs)
  | Select(e, n) ->
    let* v = eval env e in
    begin match v with
    | VTuple vs when n < List.length vs ->
      Cont.return (List.nth vs n)
    | _ -> failwith "Runtime error"
    end
  | FunRec(f, args, _, body) ->
    let rec func vs =
      let env = List.fold_left2 Env.add
        (Env.add env f (VFun func))
        (List.map fst args)
        vs
      in
      enter env body
    in
    Cont.return (VFun func)
  | App(e, es) ->
    let* v  = eval env e in
    let* vs = eval_exprs env es in
    begin match v with
    | VFun func -> func vs
    | _         -> failwith "Runtime error"
    end
  | Read(x, e) ->
    let n = read_line () |> int_of_string in
    eval (Env.add env x (VInt n)) e
  | Write(e1, e2) ->
    let* v = eval env e1 in
    begin match v with
    | VInt n -> Printf.printf "%d\n%!" n
    | _      -> failwith "Runtime error"
    end;
    eval env e2
  | Return(lvl, e, _) ->
    Cont.throw (Env.ret_cont env lvl) (eval env e)

and eval_exprs env es =
  match es with
  | [] -> Cont.return []
  | e :: es ->
    let* v  = eval env e in
    let* vs = eval_exprs env es in
    Cont.return (v :: vs)

and enter env prog =
  Cont.callcc (fun cont -> eval (Env.enter env cont) prog)

let initial_cont v =
  match v with
  | VInt n -> Printf.printf "%d\n" n
  | _      -> failwith "Invalid result"

let run_program prog =
  Cont.run (enter Env.empty prog) initial_cont

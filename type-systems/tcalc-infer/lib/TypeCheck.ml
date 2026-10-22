module Type : sig
  module UVar : sig
    type t

    val compare : t -> t -> int
    val equal   : t -> t -> bool
  end

  type t

  type view =
    | TUVar of UVar.t
    | TInt
    | TBool
    | TArrow of t * t
    | TPair  of t * t
    | TList of t (*zadanie 5*)

  val view : t -> view

  val fresh_uvar : unit -> t
  val tint   : t
  val tbool  : t
  val tarrow : t -> t -> t
  val tpair  : t -> t -> t
  val tlist : t -> t

  val set_uvar : UVar.t -> t -> unit
end = struct
  type view =
    | TUVar  of uvar
    | TInt
    | TBool
    | TArrow of t * t
    | TPair  of t * t
    | TList of t

  and t = view

  and uvar = uvar_state ref

  and uvar_state =
    | SUVar of int
    | SType of t

  module UVar = struct
    type t = uvar

    let compare u1 u2 =
      match !u1, !u2 with
      | SUVar n1, SUVar n2 -> compare n1 n2
      | _ -> assert false

    let equal u1 u2 = compare u1 u2 = 0
  end

  let rec view tp =
    match tp with
    | TUVar u ->
      begin match !u with
      | SUVar _  -> tp
      | SType tp -> view tp
      end
    | _ -> tp

  let next = ref 0

  let fresh_uvar () =
    let id = !next in
    next := id + 1;
    TUVar (ref (SUVar id))

  let tint  = TInt
  let tbool = TBool
  let tarrow tp1 tp2 = TArrow(tp1, tp2)
  let tpair  tp1 tp2 = TPair(tp1, tp2)
  let tlist tp = TList tp

  let set_uvar u tp =
    assert (UVar.equal u u);
    u := SType tp
end

let paren prec p str =
  if prec > p then "(" ^ str ^ ")"
  else str

let rec pretty_tp prec tp =
  match Type.view tp with
  | TUVar _ -> "?"
  | TInt  -> "int"
  | TBool -> "bool"
  | TArrow(tp1, tp2) ->
    paren prec 0 (pretty_tp 1 tp1 ^ " -> " ^ pretty_tp 0 tp2)
  | TPair(tp1, tp2) ->
    paren prec 10
      (pretty_tp 10 tp1 ^ " * " ^ pretty_tp 11 tp2)
  | TList tp -> pretty_tp 2 tp ^ " list"

let string_of_tp tp =
  pretty_tp 0 tp

module Env : sig
  type t

  val empty : t

  val add_var : t -> Ast.var -> Type.t -> t
  val add_vars : t -> (Ast.var * Type.t) list -> t

  val lookup_var : t -> Ast.var -> Type.t option
end = struct
  module StrMap = Map.Make(String)

  type t = Type.t StrMap.t

  let empty = StrMap.empty

  let add_var env x tp =
    StrMap.add x tp env

  let add_vars =
    List.fold_left (fun env (x, tp) -> add_var env x tp)

  let lookup_var env x =
    StrMap.find_opt x env
end

type loc = Lexing.position * Lexing.position
exception Type_error of loc * string

let rec tr_type (tp : Ast.tp) =
  match tp with
  | TWildcard -> Type.fresh_uvar ()
  | TInt  -> Type.tint
  | TBool -> Type.tbool
  | TArrow(tp1, tp2) ->
    Type.tarrow (tr_type tp1) (tr_type tp2)
  | TPair(tp1, tp2) ->
    Type.tpair (tr_type tp1) (tr_type tp2)
  | TList tp -> Type.tlist (tr_type tp) (*zadanie 5*)

let as_arrow tp =
  match Type.view tp with
  | TArrow(tp1, tp2) -> Some(tp1, tp2)
  | TUVar u ->
    let tp1 = Type.fresh_uvar () in
    let tp2 = Type.fresh_uvar () in
    Type.set_uvar u (Type.tarrow tp1 tp2);
    Some(tp1, tp2)
  | _ -> None

let as_pair tp =
  match Type.view tp with
  | TPair(tp1, tp2) -> Some(tp1, tp2)
  | TUVar u ->
    let tp1 = Type.fresh_uvar () in
    let tp2 = Type.fresh_uvar () in
    Type.set_uvar u (Type.tpair tp1 tp2);
    Some(tp1, tp2)
  | _ -> None

exception Cannot_unify

let rec occurs u tp =
  match Type.view tp with
  | TUVar u' -> Type.UVar.equal u u'
  | TInt | TBool -> false
  | TArrow(t1, t2) | TPair(t1, t2) ->
    occurs u t1 || occurs u t2
  | TList t -> occurs u t (*zadanie 5*)

let unify_with_var u tp =
  if occurs u tp then raise Cannot_unify;
  Type.set_uvar u tp

let rec unify tp1 tp2 =
  match Type.view tp1, Type.view tp2 with
  | TUVar u1, TUVar u2 when Type.UVar.equal u1 u2 -> ()
  | TUVar u, _ -> unify_with_var u tp2
  | _, TUVar u -> unify_with_var u tp1
  | TInt, TInt -> ()
  | TInt, _    -> raise Cannot_unify
  | TBool, TBool -> ()
  | TBool, _     -> raise Cannot_unify
  | TArrow(ta1, tb1), TArrow(ta2, tb2) ->
    unify ta1 ta2;
    unify tb1 tb2
  | TArrow _, _ -> raise Cannot_unify
  | TPair(ta1, tb1), TPair(ta2, tb2) ->
    unify ta1 ta2;
    unify tb1 tb2
  | TPair _, _ -> raise Cannot_unify
  | TList t1, TList t2 -> unify t1 t2 (*zadanie 5*)
  | TList _, _ -> raise Cannot_unify (*zadanie 5*)

let op_type (op : Ast.bop) =
  match op with
  | Mult | Div | Add | Sub ->
    (Type.tint, Type.tint, Type.tint)
  | Eq | Neq | Lt | Leq | Gt | Geq ->
    (Type.tint, Type.tint, Type.tbool)

let rec infer_type env (e : Ast.expr) =
  let loc = e.loc in
  match e.data with
  | Var  x ->
    begin match Env.lookup_var env x with
    | Some tp -> tp
    | None    ->
      raise (Type_error(loc, "Unbound variable " ^ x))
    end
  | Int  _ -> Type.tint
  | Bool _ -> Type.tbool
  | And(e1, e2) ->
    check_type env e1 Type.tbool;
    check_type env e2 Type.tbool;
    Type.tbool
  | Not  e ->
    check_type env e Type.tbool;
    Type.tbool
  | If(e1, e2, e3) ->
    check_type env e1 Type.tbool;
    let tp = infer_type env e2 in
    check_type env e3 tp;
    tp
  | Binop(op, e1, e2) ->
    let (tp1, tp2, tp) = op_type op in
    check_type env e1 tp1;
    check_type env e2 tp2;
    tp
  | Let(x, e1, e2) ->
    let tp1 = infer_type env e1 in
    infer_type (Env.add_var env x tp1) e2
  | Fun(x, tp, e) ->
    let tp = tr_type tp in
    Type.tarrow tp
      (infer_type (Env.add_var env x tp) e)
  | App(e1, e2) ->
    let tp = infer_type env e1 in
    begin match as_arrow tp with
    | Some(tp2, tp1) ->
      check_type env e2 tp2;
      tp1
    | None ->
      raise (Type_error(e1.loc,
        "not a function: " ^ string_of_tp tp))
    end
  | Pair(e1, e2) ->
    Type.tpair (infer_type env e1) (infer_type env e2)
  | Fst e ->
    let tp = infer_type env e in
    begin match as_pair tp with
    | Some(tp1, _) -> tp1
    | None ->
      raise (Type_error(e.loc,
        "not a pair: " ^ string_of_tp tp))
    end
  | Snd e ->
    let tp = infer_type env e in
    begin match as_pair tp with
    | Some(_, tp2) -> tp2
    | None ->
      raise (Type_error(e.loc,
        "not a pair: " ^ string_of_tp tp))
    end
  | Empty_list -> Type.tlist (Type.fresh_uvar ()) (*zadanie 5*)
  | Cons (e1, e2) -> 
    let tp1 = infer_type env e1 in 
    check_type env e2 (Type.tlist tp1); 
    Type.tlist tp1     (*zadanie 5*)

and check_type env e tp =
  let tp' = infer_type env e in
  try unify tp tp' with
  | Cannot_unify ->
    raise (Type_error(e.loc,
      ("type error: got: " ^ string_of_tp tp' ^
      " expected: " ^ string_of_tp tp)))

let check_program prog =
  let _ = infer_type Env.empty prog in
  prog

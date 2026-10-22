open Ast

exception Type_error

module Env : sig
  type t
  val empty : t
  val add_var : t -> Ast.var -> tp -> t
  val add_vars : t -> (Ast.var * tp) list -> t
  val lookup_var : t -> Ast.var -> tp option
end = struct
  module StrMap = Map.Make(String)

  type t = tp StrMap.t
  let empty = StrMap.empty
  let add_var env x tp =
    StrMap.add x tp env
  let add_vars =
    List.fold_left (fun env (x, tp) -> add_var env x tp)
  let lookup_var env x =
    StrMap.find_opt x env
end

let rec lookup_arrow e =
  match e with
  | TArrow _ -> true
  | TTuple es -> List.exists lookup_arrow es
  | _ -> false

let rec are_uniqe vs =
  match vs with
  | [] -> true
  | v :: vs' -> 
    if (List.exists (fun x -> x = v) vs' = true) then false else are_uniqe vs'

let rec infer_type (env : Env.t) (returns : tp list) (e : expr) : tp =
  match e with
  | Var x -> 
    (match Env.lookup_var env x with
    | Some t -> t
    | None -> raise Type_error
    )
  | Int _ -> TInt
  | Bool _ -> TBool
  | Binop (_, e1, e2) -> 
    (match infer_type env returns e1, infer_type env returns e2 with
    | TInt, TInt -> TInt
    | _ -> raise Type_error
    )
  | Let (x, e1, e2) ->
    let env' = Env.add_var env x (infer_type env returns e1) in
    infer_type env' returns e2
  | Cmpop (_, e1, e2) ->
    (match infer_type env returns e1, infer_type env returns e2 with
    | TInt, TInt | TBool, TBool -> TBool
    | TTuple tp1, TTuple tp2 -> 
      if tp1 = tp2 && lookup_arrow (TTuple tp1) = false then TBool else raise Type_error
    | _ -> raise Type_error 
    )
  | If (cond, e1, e2) -> 
    if infer_type env returns cond <> TBool then raise Type_error else
    let tp1 = infer_type env returns e1 in 
    let tp2 = infer_type env returns e2 in
    if tp1 <> tp2 then raise Type_error else
    tp1
  | Read (x, e) ->
    let env' = Env.add_var env x TInt in
      infer_type env' returns e
  | Write (e1, e2) -> 
    if infer_type env returns e1 <> TInt then raise Type_error else
    infer_type env returns e2
  | Tuple es ->
    TTuple (List.map (fun x -> infer_type env returns x) es)
  | Select (es, n) ->
    if n < 0 then raise Type_error else
    (match infer_type env returns es with 
    | TTuple tps -> 
      (match List.nth_opt tps n with
      | Some tp -> tp
      | None -> raise Type_error 
      )
    | _ -> raise Type_error
    )
  | FunRec (f, args, tp, body) -> 
    if lookup_arrow tp = true then raise Type_error else
    if are_uniqe (f :: (List.map fst args)) = false then raise Type_error else
    let tp_f = TArrow (List.map snd args, tp) in 
    let env' = Env.add_vars env args in
    let env'' = if f = "<anon>" then env' else (Env.add_var env' f tp_f) in 
    let tp_body = infer_type env'' (tp :: returns) body in
    if tp_body <> tp then raise Type_error else
    tp_f
  | App (f, es) -> 
    (match infer_type env returns f with
    | TArrow (tp1, tp2) -> 
      let tp_args = List.map (fun x -> infer_type env returns x) es in
      if tp1 = tp_args then tp2 else raise Type_error
    | _ -> raise Type_error
    )
  | Return (lvl, e, tp) -> 
    if lvl < 0 then raise Type_error else
    let tp_e = infer_type env returns e in 
    match List.nth_opt returns lvl with
    | Some tp_ret -> if tp_ret = tp_e then tp else raise Type_error
    | None -> raise Type_error

let check_program prog : unit =
  if infer_type Env.empty [TInt] prog <> TInt then raise Type_error else ()

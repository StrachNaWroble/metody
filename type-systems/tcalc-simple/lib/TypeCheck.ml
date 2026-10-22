type tp = Ast.tp =
  | TInt
  | TBool
  | TArrow of tp * tp
  | TPair  of tp * tp
  | TUnit
  | TRef of tp (* Zadanie 2. *)
  | TList of tp 

let paren prec p str =
  if prec > p then "(" ^ str ^ ")"
  else str

let rec pretty_tp prec tp =
  match tp with
  | TInt  -> "int"
  | TBool -> "bool"
  | TArrow(tp1, tp2) ->
    paren prec 0 (pretty_tp 1 tp1 ^ " -> " ^ pretty_tp 0 tp2)
  | TPair(tp1, tp2) ->
    paren prec 10
      (pretty_tp 10 tp1 ^ " * " ^ pretty_tp 11 tp2)
  | TUnit -> "()"
  | TRef tp -> pretty_tp prec tp ^ "ref"
  | TList tp -> pretty_tp prec tp ^ "list"

let string_of_tp tp =
  pretty_tp 0 tp

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

type loc = Lexing.position * Lexing.position
exception Type_error of loc * string

let op_type (op : Ast.bop) =
  match op with
  | Mult | Div | Add | Sub ->
    (TInt, TInt, TInt)
  | Eq | Neq | Lt | Leq | Gt | Geq ->
    (TInt, TInt, TBool)

let rec infer_type env (e : Ast.expr) =
  let loc = e.loc in
  match e.data with
  | Var  x ->
    begin match Env.lookup_var env x with
    | Some tp -> tp
    | None    ->
      raise (Type_error(loc, "Unbound variable " ^ x))
    end
  | Int  _ -> TInt
  | Bool _ -> TBool
  | And(e1, e2) ->
    check_type env e1 TBool;
    check_type env e2 TBool;
    TBool
  | Not  e ->
    check_type env e TBool;
    TBool
  | If(e1, e2, e3) ->
    check_type env e1 TBool;
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
    TArrow(tp, infer_type (Env.add_var env x tp) e)
  | Funrec(f, x, tp1, e, tp2) ->  (* Zadanie 3. *)
    let env' = Env.add_var env x tp1 in (*trzeba dodać typ x*)
    let env'' = Env.add_var env' f (TArrow(tp1, tp2)) in (*i typ f bo bedzie wywołany pózniej w ciele*)
    check_type env'' e tp2; (*sprawdzic czy cialo jest dobrze otypowane, typ argumentu sprawdzi app*)
    TArrow(tp1, tp2)
  | App(e1, e2) ->
    begin match infer_type env e1 with
    | TArrow(tp2, tp1) ->
      check_type env e2 tp2;
      tp1
    | tp ->
      raise (Type_error(e1.loc,
        "not a function: " ^ string_of_tp tp))
    end
  | Pair(e1, e2) ->
    TPair(infer_type env e1, infer_type env e2)
  | Fst e ->
    begin match infer_type env e with
    | TPair(tp1, _) -> tp1
    | tp ->
      raise (Type_error(e.loc,
        "not a pair: " ^ string_of_tp tp))
    end
  | Snd e ->
    begin match infer_type env e with
    | TPair(_, tp2) -> tp2
    | tp ->
      raise (Type_error(e.loc,
        "not a pair: " ^ string_of_tp tp))
    end
  | Unit -> TUnit
  | Reref(e1, e2) ->  (*e1 musi byc Tref of tp i potem e2 musi byc tp *)
    begin match infer_type env e1 with
    | TRef tp -> check_type env e2 tp; TUnit
    | _ -> failwith "not a referention"
    end 
  | Deref(e) -> (*aby odwrocic referencje musi byc Tref*)
    begin match infer_type env e with 
    | TRef tp -> tp
    | _ -> failwith "not a referention"
    end
  | Ref(e) ->
    TRef(infer_type env e)
  | Empty_list(tp) -> TList(tp) (* Zadanie 4 *)
  | List(ls) ->
    let tp = infer_type env (List.hd ls) (* ustalam tp na typ pierwszsego elemtu *)
    in let rec check ls =  (* Iteruję się po reszcie elementow sprawdzajac czy sa tego samego typu tp*)
      begin match ls with 
      | [] -> TList(tp) (*Zwracam litę typu tp*)
      | l :: ls' -> check_type env l tp; check ls'; (*sprawdzam i redukuje do []*)
      end
    in check ls
  | Cons(hd, tl) -> (* Zadanie 4. *)
    let tp = infer_type env hd in 
    check_type env tl (TList(tp)); (*tl musi byc listą typu tp, tp - takiego jak hd*)
    TList(tp)
  | Match (list, e1, x, xs, e2) ->
    let tp_e1 = infer_type env e1 in 
    begin match infer_type env list with
    | TList tp -> 
      let env' = Env.add_var env x tp in
      let env'' = Env.add_var env' xs (TList tp)
      in check_type env'' e2 tp_e1; tp;
    | _ -> failwith "list in match is not a TLits type"
    end


and check_type env e tp =
  let tp' = infer_type env e in
  if tp' <> tp then
    raise (Type_error(e.loc,
      ("type error: got: " ^ string_of_tp tp' ^
      " expected: " ^ string_of_tp tp)))

let check_program prog =
  let _ = infer_type Env.empty prog in
  prog



(* testowanie : 
zad 3 - dune exec tcalc -- -eval -input "let siln = funrec siln x:int -> (if x == 0 then 1 else x * siln(x-1)) : int in siln 3"
zad 4 - dune exec tcalc -- -eval -input "true::[false;false;]"
                                        "true::[]:int"

*)

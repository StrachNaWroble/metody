type var = string
type sym = string

type term =
  | Var of var
  | Sym of sym * term list

module VarMap = Map.Make(String)

module Term : sig
  type t
  type tvar
  type view =
    | TVar of tvar
    | TSym of sym * t list

  val view : t -> view

  val var_equal : tvar -> tvar -> bool
  val set_var : tvar -> t -> unit

  val fresh_var : var -> t
  val sym : sym -> t list -> t
  val to_term : t -> term
end = struct
  type view =
    | TVar of tvar
    | TSym of sym * t list

  and t = view

  and tvar = var_state ref

  and var_state =
    | SVar  of var
    | STerm of t

  let var_equal x y = x == y

  let set_var x t =
    match !x with
    | SVar  _ -> x := STerm t
    | STerm _ -> assert false

  let rec view t =
    match t with
    | TVar x ->
      begin match !x with
      | SVar  _  -> t
      | STerm t' -> view t'
      end
    | TSym _ -> t

  let fresh_var x = TVar (ref (SVar x))
  let sym f ts = TSym(f, ts)

  let rec to_term (t : t) : term =   (*Zadanie 3.*)
  match view t with
  | TVar v ->
    begin match !v with
    | SVar  v  -> Var v
    | STerm _ -> failwith "error"
    end
  | TSym(s, ts) -> Sym (s, List.map to_term ts)
end

let rec tr_term var_map (t : term) : Term.t =
  match t with
  | Var x ->
    begin match Hashtbl.find_opt var_map x with
    | Some t -> t
    | None ->
      let t = Term.fresh_var x in
      Hashtbl.add var_map x t;
      t
    end
  | Sym(f, ts) -> Term.sym f (List.map (tr_term var_map) ts)

exception Cannot_unify

let rec occurs (x : Term.tvar) (t : Term.t) : bool =
  match Term.view t with
  | TVar y      -> Term.var_equal x y
  | TSym(_, ts) -> List.exists (occurs x) ts

let unify_with_var x t =
  if occurs x t then raise Cannot_unify;
  Term.set_var x t

let rec unify (t1 : Term.t) (t2 : Term.t) : unit =
  match Term.view t1, Term.view t2 with
  | TVar x, TVar y when Term.var_equal x y -> ()
  | TVar x, _ -> unify_with_var x t2
  | _, TVar x -> unify_with_var x t1
  | TSym(f1, ts1), TSym(f2, ts2)
    when f1 = f2 && List.length ts1 = List.length ts2 ->
    List.iter2 unify ts1 ts2
  | TSym(_, _), TSym(_, _) -> raise Cannot_unify

let unify (t1 : term) (t2 : term) : (var * term) list option =
  (* Mapa ze zmiennych w wewnętrzną reprezentację termów *)
  let var_map = Hashtbl.create 32 in
  match unify (tr_term var_map t1) (tr_term var_map t2) with
  | () ->
    let lst = Hashtbl.to_seq var_map |> List.of_seq in
    let ans = List.map (fun (v, t) -> (v, Term.to_term t)) lst in Some ans (*Zadanie 3.*)
  | exception Cannot_unify -> None

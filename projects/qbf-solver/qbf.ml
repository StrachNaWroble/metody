type ident = string
type qbf =
| Top (* ⊤ *)
| Bot (* ⊥ *)
| Var of ident (* x *)
| Disj of qbf * qbf (* ∨ *)
| Conj of qbf * qbf (* ∧ *)
| Not of qbf (* ¬ *)
| Forall of ident * qbf (* ∀ *)
| Exists of ident * qbf (* ∃ *)


let rec replace (x : ident) (v : qbf) (formula : qbf) : qbf = 
  match formula with 
  | Top -> Top 
  | Bot -> Bot
  | Var y -> if y = x then v else Var y
  | Disj (p, q) -> Disj (replace x v p, replace x v q)
  | Conj (p, q) -> Conj (replace x v p, replace x v q)
  | Not p -> Not (replace x v p)
  | Forall (y, p) -> 
      if y = x then Forall (x, p) 
      else Forall (y, replace x v p)
  | Exists (y, p) ->
      if y = x then Exists (x, p)
      else Exists (y, replace x v p)
  
let rec eval_repl (formula : qbf) : bool = 
  match formula with
  | Top -> true
  | Bot -> false
  | Var x -> failwith "unbound variable"
  | Not p -> not (eval_repl p)
  | Disj (p, q) -> eval_repl p || eval_repl q
  | Conj (p, q) -> eval_repl p && eval_repl q
  | Forall (x, p) -> eval_repl (replace x Top p) && eval_repl (replace x Bot p)
  | Exists (x, p) -> eval_repl (replace x Top p) || eval_repl (replace x Bot p)

module Env : sig
  type 'a t
  val empty : 'a t
  val add : string -> 'a -> 'a t -> 'a t
  val find_opt : string -> 'a t -> 'a option
end = Map.Make(String)

type env = bool Env.t

let rec eval_env (formula : qbf) (env : env) : bool = 
  match formula with
  | Top -> true
  | Bot -> false
  | Not p -> not (eval_env p env)
  | Disj (p, q) -> eval_env p env || eval_env q env
  | Conj (p, q) -> eval_env p env && eval_env q env
  | Var x -> 
    (match Env.find_opt x env with
    | Some x_val -> x_val
    | None -> failwith "unbound variable")
  | Forall (x, p) -> (eval_env p (Env.add x false env)) && (eval_env p (Env.add x true env))
  | Exists (x, p) -> (eval_env p (Env.add x false env)) || (eval_env p (Env.add x true env))
  
let rec eval (formula : qbf) : bool =
  eval_env formula Env.empty

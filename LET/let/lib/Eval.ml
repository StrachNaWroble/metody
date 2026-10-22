open Ast

type value =
  | VInt of int
  | VBool of bool
  | VUnit (* zadanie 3.*)
  | VPair of value * value (* zadanie 4.*)

let rec show_value (v : value) : string =
  match v with
  | VInt a -> string_of_int a
  | VBool b -> string_of_bool b
  | VUnit -> "()" (* zadanie 3.*)
  | VPair (v1, v2) -> "(" ^ show_value v1 ^ ", " ^ show_value v2 ^ ")" (* zadanie 4.*) 

let print_value (v : value) : unit =
  v |> show_value |> print_string

let eval_op (op : bop) (l : value) (r : value) : value =
  match op, l, r with
  | Mult, VInt l, VInt r -> VInt (l * r)
  | Div, VInt l, VInt r  -> VInt (l / r)
  | Add, VInt l, VInt r  -> VInt (l + r)
  | Sub, VInt l, VInt r  -> VInt (l - r)
  | _ -> failwith "type error"

module Env : sig
  type 'a t
  val empty : 'a t
  val add : string -> 'a -> 'a t -> 'a t
  val find_opt : string -> 'a t -> 'a option
end = Map.Make(String)

type env = value Env.t

let rec eval (env : env) (e : expr) : value =
  match e with
  | Int a -> VInt a
  | Binop (op, l, r) -> eval_op op (eval env l) (eval env r)
  | Bool b -> VBool b
  | If (b, t, e) ->
      (match eval env b with
        | VBool true -> eval env t
        | VBool false -> eval env e
        | _ -> failwith "type error")
  | Let (x, e1, e2) ->
      let v1 = eval env e1 in
      let env' = Env.add x v1 env in
      let v2 = eval env' e2 in
      v2
  | Var y ->
      (match Env.find_opt y env with
       | Some v -> v
       | None -> failwith ("unknown var " ^ y))
  | Unit -> VUnit (* zadanie 3.*)
  | Pair (e1, e2) -> VPair (eval env e1, eval env e2) (* zadanie 4.*) 
  | Fst p -> 
      (match eval env p with (* zadanie 4. *)
      | VPair (v1, _) -> v1
      | _ -> failwith "type error")
  | Snd p -> 
      (match eval env p with (* zadanie 4. *)
      | VPair (_, v2) -> v2
      | _ -> failwith "type error")
  | MatchPair (p, x, y, e) -> (* zadanie 5. *)
      (match eval env p with 
      | VPair (v1, v2) -> 
          let env' = Env.add x v1 env 
          in let env'' = Env.add y v2 env' 
          in eval env'' e
      | _ -> failwith "type error"
      )
  | Sum (x, n, m, k) -> (* zadanie 7. *)
      let v_n = eval env n in 
      let v_m = eval env m in 
      (match v_n, v_m with
      |  VInt v_start, VInt v_end -> 
          let rec it i acc = 
              if i > v_end then VInt acc
              else 
                  let v_i = VInt i in
                  let env' = Env.add x v_i env in
                  let v_k = eval env' k in 
                  match v_k with 
                    | VInt v_k -> it (i + 1) (acc + v_k)
                    | _ -> failwith "type error"
          in it v_start 0
      | _ -> failwith "type error")
  
let interp (s : string) : value =
  let ast =
    Parser.main Lexer.read (Lexing.from_string s)
  in
  eval Env.empty ast

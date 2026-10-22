open Ast

module Env : sig
  type 'a t
  val empty : 'a t
  val add : string -> 'a -> 'a t -> 'a t
  val find_opt : string -> 'a t -> 'a option
end = Map.Make(String)

type env = value Env.t

and value =
  | VInt of int
  | VBool of bool
  | VUnit
  | VPair of value * value
  | VClosure of env * ident * expr
  | VRecClosure of env * ident * ident * expr

let rec show_value (v : value) : string =
  match v with
  | VInt a -> string_of_int a
  | VBool b -> string_of_bool b
  | VUnit -> "()"
  | VPair (v1, v2) -> Printf.sprintf "(%s,%s)" (show_value v1) (show_value v2)
  | VClosure _ | VRecClosure _ -> "<fun>"

let print_value (v : value) : unit =
  v |> show_value |> print_endline

let rec equal_value (v1 : value) (v2 : value) : bool =
  match v1, v2 with
  | VInt a1, VInt a2 -> Int.equal a1 a2
  | VBool a1, VBool a2 -> Bool.equal a1 a2
  | VUnit, VUnit -> true
  | VPair (a1, a2), VPair (b1, b2) -> equal_value a1 b1 && equal_value a2 b2
  | _ -> false

let eval_op (op : bop) (l : value) (r : value) : value =
  match op, l, r with
  | Mult, VInt l, VInt r -> VInt (l * r)
  | Div, VInt l, VInt r  -> VInt (l / r)
  | Add, VInt l, VInt r  -> VInt (l + r)
  | Sub, VInt l, VInt r  -> VInt (l - r)
  | Eq, l, r -> VBool (equal_value l r)
  | Neq, l, r -> VBool (equal_value l r |> not)
  | Gt, VInt l, VInt r -> VBool (l > r)
  | Geq, VInt l, VInt r -> VBool (l >= r)
  | Lt, VInt l, VInt r -> VBool (l < r)
  | Leq, VInt l, VInt r -> VBool (l <= r)
  | _ -> failwith "type error"

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
  | Unit -> VUnit
  | Pair (e1, e2) -> VPair (eval env e1, eval env e2)
  | Match (e1, x, y, e2) ->
      (match eval env e1 with
        | VPair (v1, v2) ->
            let env' = env |> Env.add x v1 |> Env.add y v2 in
            eval env' e2
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
  | Fun (x, e) -> VClosure (env, x, e)
  | Funrec (f, x, e) -> VRecClosure (env, f, x, e)
  | App (e1, e2) ->
      (match eval env e1 with
        | VClosure (env', x, body) ->
            let v_arg = eval env e2 in
            eval (Env.add x v_arg env') body
        | VRecClosure (env', f, x, body) as c ->
            let v_arg = eval env e2 in
            let env'' = env' |> Env.add x v_arg |> Env.add f c in
            eval env'' body
        | _ -> failwith "type error")

let interp (env : env) (s : string) : value = 
  let ast =
    Parser.main Lexer.read (Lexing.from_string s)
  in
    Check.unused_vars ast; (* zadanie 6. *)
    let free = Check.free_vars ast in (* zadanie 5. *)
    let free_corrected = List.filter (fun v -> Env.find_opt v env = None) free in (* zadanie 5. *)
    if free_corrected <> [] then 
      failwith "Error: unbound value(s)"
    else
      eval env ast

let eval_def (env : env) ((x, e) : def) : env =
  let v = eval env e in
  print_endline x;
  Env.add x v env

let eval_prog (env : env) (s : def list) : env =
  List.fold_left eval_def env s

let interp_prog (env : env) (s : string) : env =
  let defs =
    Parser.program Lexer.read (Lexing.from_string s)
  in
  eval_prog env defs

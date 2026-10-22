type value =
  | VInt  of int
  | VBool of bool
  | VFun  of (value -> value)
  | VPair of value * value
  | VUnit 
  | VRef of value ref
  | VList of value list

let rec string_of_value v =
  match v with
  | VInt  n -> string_of_int n
  | VBool b -> string_of_bool b
  | VFun  _ -> "<fun>"
  | VPair(v1, v2) ->
    "(" ^ string_of_value v1 ^ ", " ^ string_of_value v2 ^ ")"
  | VUnit -> "unit"
  | VRef r -> string_of_value !r ^ " ref"
  | VList ls -> "[" ^ print_list ls ^ "]" 

and print_list ls = 
    match ls with
    | [] -> ""
    | hd::tl -> string_of_value hd ^ ";" ^ print_list tl
    

module Env : sig
  type t

  val empty  : t
  val add    : t -> Ast.var -> value ->t
  val lookup : t -> Ast.var -> value
end = struct
  module StrMap = Map.Make(String)
  type t = value StrMap.t

  let empty = StrMap.empty
  let add env x v = StrMap.add x v env
  let lookup env x = StrMap.find x env
end

let int_op op v1 v2 =
  match v1, v2 with
  | VInt n1, VInt n2 -> VInt (op n1 n2)
  | _ -> failwith "Runtime error"

let cmp_op op v1 v2 =
  match v1, v2 with
  | VInt n1, VInt n2 -> VBool (op n1 n2)
  | _ -> failwith "Runtime error"

let eval_op (op : Ast.bop) =
  match op with
  | Mult -> int_op ( * )
  | Div  -> int_op ( / )
  | Add  -> int_op ( + )
  | Sub  -> int_op ( - )
  | Eq   -> cmp_op ( = )
  | Neq  -> cmp_op ( <> )
  | Gt   -> cmp_op ( > )
  | Geq  -> cmp_op ( >= )
  | Lt   -> cmp_op ( < )
  | Leq  -> cmp_op ( <= )

let rec eval env (e : Ast.expr) =
  match e.data with
  | Var  x -> Env.lookup env x
  | Int  n -> VInt  n
  | Bool b -> VBool b
  | And(e1, e2) ->
    begin match eval env e1 with
    | VBool true  -> eval env e2
    | VBool false -> VBool false
    | _           -> failwith "runtime error"
    end
  | Not e ->
    begin match eval env e with
    | VBool b -> VBool (not b)
    | _       -> failwith "runtime error"
    end
  | If(e1, e2, e3) ->
    begin match eval env e1 with
    | VBool true  -> eval env e2
    | VBool false -> eval env e3
    | _           -> failwith "runtime error"
    end
  | Binop(op, e1, e2) ->
    eval_op op (eval env e1) (eval env e2)
  | Let(x, e1, e2) ->
    eval (Env.add env x (eval env e1)) e2
  | Fun(x, _, body) ->
    VFun(fun v -> eval (Env.add env x v) body)
  | Funrec(f, x, _, e, _) -> (* Zadanie 3. *)
    let rec fun_f a =
      let env' = Env.add env x a in
      let env'' = Env.add env' f (VFun fun_f) in
      eval env'' e
    in VFun fun_f
  | App(e1, e2) ->
    begin match eval env e1 with
    | VFun f -> f (eval env e2)
    | _      -> failwith "runtime error"
    end
  | Pair(e1, e2) ->
    VPair(eval env e1, eval env e2)
  | Fst e ->
    begin match eval env e with
    | VPair(v, _) -> v
    | _           -> failwith "runtime error"
    end
  | Snd e ->
    begin match eval env e with
    | VPair(_, v) -> v
    | _           -> failwith "runtime error"
    end
  | Unit -> VUnit
  | Reref(e1, e2) -> 
    begin match eval env e1 with
    | VRef(r) -> r := eval env e2; VUnit
    | _ -> failwith"runtime error"
    end
  | Ref(e) -> VRef(ref (eval env e))
  | Deref(e) -> 
    begin match eval env e with
      | VRef(r) -> !r
      | _ -> failwith "runtime error"
    end
  | Empty_list(_) -> VList([]) (* Zadanie 4. *)
  | List(ls) -> 
    let rec it ls =
    begin match ls with
    | [] -> []
    | l :: ls' -> (eval env l) :: it ls'
    end
    in VList(it ls)
  | Cons(hd, tl) -> 
    begin match eval env tl with 
    | VList(tl') ->  VList(eval env hd :: tl')
    | _ -> failwith "runtime error" 
    end
  | Match(list, e1, x, xs, e2) ->
    begin match eval env list with
    | VList ls -> 
      begin match ls with 
      | [] -> eval env e1
      | l :: ls' -> 
        let env' = Env.add env x l in
        let env'' = Env.add env' xs (VList ls') in
        eval env'' e2
      end
    | _ -> failwith "runtime error" 
    end
    
   
let run prog =
  eval Env.empty prog
  |> string_of_value
  |> print_endline

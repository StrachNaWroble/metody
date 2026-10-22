open Ast

type value =
  | VInt of int
  | VBool of bool
  | VStr of string

module Memory : sig
  type 'a t
  val empty : 'a t
  val add : string -> 'a -> 'a t -> 'a t
  val find_opt : string -> 'a t -> 'a option
end = Map.Make(String)

type memory = value Memory.t

let show_value (v : value) : string =
  match v with
  | VInt a -> string_of_int a
  | VBool b -> string_of_bool b
  | VStr s -> s

let print_value (v : value) : unit =
  v |> show_value |> print_string

let equal_value (v1 : value) (v2 : value) : bool =
  match v1, v2 with
  | VInt a1, VInt a2 -> Int.equal a1 a2
  | VBool a1, VBool a2 -> Bool.equal a1 a2
  | VStr a1, VStr a2 -> String.equal a1 a2
  | _ -> false

let eval_op (op : bop) (l : value) (r : value) : value =
  match op, l, r with
  | Mult, VInt l, VInt r -> VInt (l * r)
  | Div, VInt l, VInt r -> VInt (l / r)
  | Add, VInt l, VInt r -> VInt (l + r)
  | Add, VStr l, VStr r -> VStr (l ^ r)
  | Sub, VInt l, VInt r -> VInt (l - r)
  | Eq, l, r -> VBool (equal_value l r)
  | Neq, l, r -> VBool (equal_value l r |> not)
  | Gt, VInt l, VInt r -> VBool (l > r)
  | Geq, VInt l, VInt r -> VBool (l >= r)
  | Lt, VInt l, VInt r -> VBool (l < r)
  | Leq, VInt l, VInt r -> VBool (l <= r)
  | _ -> failwith "type error"

let rec eval (e : expr) (m : memory) : value =
  match e with
  | Int a -> VInt a
  | Bool b -> VBool b
  | Str s -> VStr s
  | Binop (op, l, r) -> eval_op op (eval l m) (eval r m)
  | Var x ->
      begin match Memory.find_opt x m with
      | Some v -> v
      | None -> VInt 0 (* wartość domyślna *)
      end

let rec exec (s : stmt) : memory Transformer.t =
  match s with
  | Skip -> Transformer.id
  | Assign (x, e) -> Transformer.of_fun 
      (fun m -> 
        let v = eval e m in
        Memory.add x v m
      )
  | Print e -> Transformer.with_fun 
      (fun m -> eval e m)
      (fun v -> print_value v; Transformer.id)
  | Cmp (s1, s2) ->
      Transformer.compose (exec s1) (exec s2)
  | If (p, t, e) -> Transformer.with_fun 
      (fun m -> eval p m)
      (fun v -> 
        match v with
        | VBool true -> exec t
        | VBool false -> exec e
        | _ -> failwith "type error"
      )
  | While (p, b) -> Transformer.with_fun
      (fun m -> eval p m)
      (fun v -> 
        match v with
        | VBool false -> Transformer.id
        | VBool true -> Transformer.compose 
          (exec b) (exec (While(p, b)))
        | _ -> failwith "type error")
  | Halt -> Transformer.id

let parse (s : string) : stmt =
  Parser.main Lexer.read (Lexing.from_string s)

let interp (s : string) : unit =
  ignore (Transformer.run (exec (parse s)) Memory.empty)

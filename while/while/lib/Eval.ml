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

let rec exec (s : stmt) (m : memory) : memory option =
  match s with
  | Halt -> None
  | Assign (x, e) ->
      let v = eval e m in
      Some (Memory.add x v m)
  | Skip -> Some m
  | Cmp (s1, s2) ->
    (match exec s1 m with
    | None -> None
    | Some m' -> exec s2 m')
  | Print e ->
      let v = eval e m in
      print_value v;
      Some m
  | If (p, t, e) ->
      let v = eval p m in
      (match v with
       | VBool true -> exec t m
       | VBool false -> exec e m
       | _ -> failwith "type error")
  | While (p, b) ->
      let v = eval p m in
      (match v with
       | VBool false -> Some m
       | VBool true -> 
        (match exec b m with
        | None -> None
        | Some m' -> exec s m')
       | _ -> failwith "type error")

let parse (s : string) : stmt =
  Parser.main Lexer.read (Lexing.from_string s)

let interp (s : string) : unit =
  ignore (exec (parse s) Memory.empty)

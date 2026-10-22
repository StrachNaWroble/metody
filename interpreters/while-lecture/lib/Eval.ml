open Ast

(* Zadanie 2 : Zamiana int -> bigint *)

module IntMap = Map.Make(Int)  (* Zadanie 3. *)

type value =
  | VInt of Bigint.t
  | VBool of bool
  | VStr of string
  | VArray of value IntMap.t (* Zadanie 3. *)

module Memory : sig
  type 'a t
  val empty : 'a t
  val add : string -> 'a -> 'a t -> 'a t
  val find_opt : string -> 'a t -> 'a option
end = Map.Make(String)

type memory = value Memory.t

let rec show_value (v : value) : string = (* Zadanie 3. *)
  match v with
  | VInt a -> Bigint.to_string a
  | VBool b -> string_of_bool b
  | VStr s -> s
  | VArray arr ->                         (* Zadanie 3. *)
    if IntMap.is_empty arr then "[]" else
      let (max_idx, _) = IntMap.max_binding arr in
      let rec it i =
        if i > max_idx then "" else
          let v = match IntMap.find_opt i arr with 
          | Some v -> v
          | None -> VInt Bigint.zero in
          let s = show_value v in
          if i = max_idx then s else s ^ "; " ^ it (i + 1)
        in "[" ^ it 0 ^ "]"

let print_value (v : value) : unit =
  v |> show_value |> print_string;
  flush stdout

let equal_value (v1 : value) (v2 : value) : bool =
  match v1, v2 with
  | VInt a1, VInt a2 -> Bigint.equal a1 a2
  | VBool a1, VBool a2 -> Bool.equal a1 a2
  | VStr a1, VStr a2 -> String.equal a1 a2
  | _ -> false

module State : sig
  type 'a t (* typ abstrakcyjny! *)
  val pure : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val get : ident -> value t
  val set : ident -> value -> unit t
  val run : 'a t -> memory -> 'a option (* Zadanie 7. *)
  val halt : 'a t    (* Zadanie 7. *)
end =
struct
  type 'a t = memory -> (memory * 'a) option (* Zadanie 7. : dodanie wszędzie w module option do wyniku*)

  let pure (a : 'a) : 'a t =
    fun m -> Some (m, a)

  let bind (s : 'a t) (f : 'a -> 'b t) : 'b t =
    fun m ->
      match s m with
      | None -> None
      | Some (m1, v) -> f v m1

  let get (x : ident) : value t =
    fun m ->
      match Memory.find_opt x m with
      | Some v -> Some (m, v)
      | _ -> Some (m, VInt (Bigint.of_int 0)) (* Zadanie 1. *)

  let set (x : ident) (v : value) : unit t =
    fun m ->
      Some (Memory.add x v m, ())

  let run (s : 'a t) (m : memory) : 'a option =
    match s m with
    | Some (_, v) -> Some v
    | None -> None

  let halt : 'a t =
    fun _ -> None  (* Zadanie 7. *)
end

type 'a state = 'a State.t

let (let*) = State.bind

let eval_op (op : bop) (l : value) (r : value) : value =
  Bigint.(
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
  )

let rec eval (e : expr) : value state =
  match e with
  | Int a -> State.pure (VInt a)
  | Bool b -> State.pure (VBool b)
  | Str s -> State.pure (VStr s)
  | Binop (op, l, r) ->
      let* vl = eval l in
      let* vr = eval r in
      State.pure (eval_op op vl vr)
  | Var x ->
      State.get x
  | Preincr x ->
      let* v = State.get x in
      (match v with
      | VInt n ->
          let k = VInt Bigint.(n + of_int 1) in
          let* _ = State.set x k in
          State.pure k
      | _ -> failwith "type error")
  | Array es -> (* Zadanie 3. *)
        let rec build_array idx es res_map =
          match es with
          | [] -> 
              State.pure (VArray res_map)
          | e :: es' ->
              let* v = eval e in
              let next_map = IntMap.add idx v res_map in
              build_array (idx + 1) es' next_map
        in
        build_array 0 es IntMap.empty
  | ArrayRead (x, i) -> (* Zadanie 4. *)
      let* v_x = State.get x in
      let* v_i = eval i in 
      match v_x, v_i with
      | VArray map, VInt indx ->  
        (match Bigint.to_int indx with (*czy indx nie wykracza poza rozmiar arr*)
            | Some indx ->
                (match IntMap.find_opt indx map with (*czy indx ma cos przypisane w arr*)
                | Some v -> State.pure v
                | None -> State.pure (VInt (Bigint.of_int 0)))
            | None -> 
                State.pure (VInt (Bigint.of_int 0)))
      | _ -> failwith "type error"

let rec exec (s : stmt) : unit state =
  match s with
  | Halt -> State.halt (* Zadanie 7. *)
  | Assign (x, e) ->
      let* v = eval e in
      State.set x v
  | Skip -> State.pure ()
  | Cmp (s1, s2) ->
      let* _ = exec s1 in
      exec s2
  | Print e ->
      let* v = eval e in
      print_value v;
      State.pure ()
  | If (p, t, e) ->
      let* v = eval p in
      (match v with
      | VBool true -> exec t
      | VBool false -> exec e
      | _ -> failwith "type error")
  | While (p, b) ->
      let* v = eval p in
      (match v with
      | VBool false -> State.pure ()
      | VBool true -> let* _ = exec b in exec s
      | _ -> failwith "type error")
  | AssignArray (x, i, e) ->
      let* x_v = State.get x in
      let* i_v = eval i in
      let* new_v = eval e in
      (match x_v, i_v with
      | VArray map, VInt indx ->
          (match Bigint.to_int indx with
          | Some indx ->
              let new_map = IntMap.add indx new_v map in
              State.set x (VArray new_map)
          | None -> State.pure ())
      | _ -> failwith "type error")


let parse (s : string) : stmt =
  Parser.main Lexer.read (Lexing.from_string s)

let interp (s : string) : unit =
  ignore (State.run (exec (parse s)) Memory.empty)

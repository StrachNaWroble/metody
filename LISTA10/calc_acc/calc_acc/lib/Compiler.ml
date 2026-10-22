open VM

module Env : sig
  type t

  val empty    : t
  val add      : t -> Ast.var -> t
  val push_tmp : t -> t
  val lookup   : t -> Ast.var -> int
end = struct
  type t = Ast.var option list

  let empty     = []
  let add env x = Some x :: env
  let push_tmp env = None :: env

  let lookup env x =
    match List.find_index (fun y -> y = Some x) env with
    | Some i -> i
    | None   -> failwith ("Unbound variable " ^ x)
end

let compile_op (op : Ast.bop) =
  match op with
  | Add  -> [ MAdd ]
  | Sub  -> [ MSub ]
  | Div  -> [ MDiv ]
  | Mult -> [ MMul ]

let rec compile env (e : Ast.expr) =
  match e with
  | Var x -> [ MGetLocal (Env.lookup env x) ]
  | Int n -> [ MConst n ]
  | Binop(op, e1, e2) ->
    compile env e1 @
    [ MPush ] @
    compile (Env.push_tmp env) e2 @
    compile_op op
  | Let(x, e1, e2) ->
    compile env e1 @
    [ MPush ] @
    compile (Env.add env x) e2 @
    [ MPopN 1 ]

(* Zadanie 3. *)

let new_compile_op (op : Ast.bop) =
  match op with
  | Add  -> MAdd
  | Sub  -> MSub
  | Div  -> MDiv
  | Mult -> MMul

let rec new_compile env (e : Ast.expr) res =
  match e with
  | Var x -> MGetLocal (Env.lookup env x) :: res
  | Int n -> MConst n :: res
  | Binop(op, e1, e2) ->
    new_compile env e1 ( MPush :: (new_compile (Env.push_tmp env) e2 (new_compile_op op :: res) ) )
  | Let(x, e1, e2) ->
    new_compile env e1 ( MPush :: new_compile (Env.add env x) e2 ( MPopN 1 :: res ) )

(* Zadanie 1. *)
let rec get_depth_helper e d max_d = 
  match e with
  | e' :: es -> 
    begin match e' with 
    | MPush -> get_depth_helper es (d + 1) (max (d + 1) max_d)
    | MPopN n -> get_depth_helper es (d - n) max_d
    | MAdd | MSub | MMul | MDiv -> get_depth_helper es (d - 1) max_d
    | _ -> get_depth_helper es d (max d max_d)
    end
  | [] -> max_d

let get_depth e = get_depth_helper e 0 0 

(* Zadanie 2. 
let rec decompile code = 
  match code with
  | [] -> ()
  | MConst n :: code -> (Ast.Int n, decompile code)
  | MAdd :: code -> 
    let 
  | MSub :: code -> decompile code 
  | MMul :: code -> decompile code 
  | MDiv :: code -> decompile code 
  | _ -> failwith "decompilation failed"*)


let compile_program e =
  new_compile Env.empty e []


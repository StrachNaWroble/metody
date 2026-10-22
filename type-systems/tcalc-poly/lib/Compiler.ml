open VM

type var_info =
  | VLocal of int
  | VFun   of label
  | VClo

module Env : sig
  type t

  val empty    : t
  val frame    : t -> t
  val add      : t -> Ast.var -> t
  val add_fun  : t -> Ast.var -> label -> t
  val push_tmp : t -> t
  val lookup   : t -> Ast.var -> var_info
end = struct
  module StrMap = Map.Make(String)
  type t =
    { var_map    : var_info StrMap.t;
      stack_size : int
    }

  let empty     =
    { var_map    = StrMap.empty;
      stack_size = 0
    }

  let frame env =
    { var_map    = 
        StrMap.map (function VLocal _ -> VClo | vi -> vi) env.var_map;
      stack_size = 0
    }

  let add env x =
    { var_map    = StrMap.add x (VLocal (env.stack_size + 1)) env.var_map;
      stack_size = env.stack_size + 1
    }

  let add_fun env x l =
    { var_map    = StrMap.add x (VFun l) env.var_map;
      stack_size = env.stack_size
    }

  let push_tmp env =
    { env with stack_size = env.stack_size + 1 }

  let lookup env x =
    match StrMap.find_opt x env.var_map with
    | Some (VLocal i) -> VLocal (env.stack_size - i)
    | Some ((VFun _ | VClo) as vi) -> vi
    | None   -> failwith ("Unbound variable " ^ x)
end

(* Generowanie świeżych etykiet *)
let next_id = ref 0
let fresh_label () =
  let id = !next_id in
  next_id := id + 1;
  "$L_" ^ string_of_int id

let compile_cmp_op (op : Ast.bop) =
  match op with
  | Lt  -> MC_LT
  | Gt  -> MC_GT
  | Leq -> MC_LE
  | Geq -> MC_GE
  | Neq -> MC_NE
  | Eq  -> MC_EQ
  | _   -> failwith "Not a comparison operator"

let compile_op (op : Ast.bop) =
  match op with
  | Add  -> [ MAdd ]
  | Sub  -> [ MSub ]
  | Div  -> [ MDiv ]
  | Mult -> [ MMul ]
  | Lt | Gt | Leq | Geq | Neq | Eq  ->
    let l1 = fresh_label () in
    let l2 = fresh_label () in
    [ MBranch(compile_cmp_op op, l1);
      MConst 0;
      MJump l2;
      MLabel l1;
      MConst 1;
      MLabel l2
    ]

(* Upewnia się, że podany kod zaczyna się od etykiety. Zwraca parę złożoną
  z tej etykiety i potencjalnie zmodyfikowanego kodu (z dodaną etykietą). *)
let add_label code =
  match code with
  | MLabel l :: _ -> (l, code)
  | _ ->
    let l = fresh_label () in
    (l, MLabel l :: code)

(* Operator wiążący, który wiąże etykietę do podanego kodu, zaś sam kod
  umieszczany jest na końcu programu wygenerowanego przez funkcję f.
  Pozwala zgrabnie tworzyć kod z nietrywialnym przepływem sterowania, np.
  w instrukcji if. *)
let (let^) code f =
  match code with
  | MJump l :: _ ->
    (* Jeśli kod jest skokiem bezwarunkowym, to nie musimy go nigdzie
      dodawać -- zamiast skakać do niego, możemy skoczyć bezpośrednio
      pod etykietę l, dzięki czemu unikniemy generowania zbędnych
      łańcuchów skoków. *)
    f l
  | _ ->
    let (l, code) = add_label code in
    f l @ code

(* Funkcja compile bierze dodatkowy parametr cont (kontynuację), który jest
  kodem do którego powinniśmy przejść po obliczeniu wartości wyrażenia e (tak
  należało rozwiązać zadanie 3 z listy 10). Dzięki temu, że w OCamlu operator
  aplikacji (@@) zdefiniowany w bibliotece standardowej jako

  let (@@) f x = f x

  wiąże w prawo na tym samym poziomie co konkatenacja list (@), kod tej funkcji
  możemy zapisać elegancko, bez konieczności używania nawiasów. Na przykład,
  kompilując operator binarny, napiszemy

  compile env e1 @@
  [ MPush ] @
  compile (Env.push_tmp env) e2 @@
  compile_op op @
  cont

  co znaczy dokładnie to samo co

  compile env e1
    ([ MPush ] @ compile (Env.push_tmp env) e2
      (compile_op op @ cont))

  tyle, że jest bardziej czytelne. *)

let rec compile env (e : Ast.expr) cont =
  match e.data with
  | Var x ->
    begin match Env.lookup env x with
    | VLocal n -> MGetLocal n
    | VFun   l -> MGetLabel l
    | VClo     ->
      failwith "Closures are not supported (yet?)"
    end :: cont
  | Int n ->  MConst n :: cont
  | Bool b -> MConst (if b then 1 else 0) :: cont
  | Binop(op, e1, e2) ->
    compile env e1 @@
    [ MPush ] @
    compile (Env.push_tmp env) e2 @@
    compile_op op @
    cont
  | Let(f, { data = Fun(x, _, body); _ }, e) ->
    let^ l = compile (Env.add (Env.frame env) x) body [ MRet ] in
    compile (Env.add_fun env f l) e @@
    cont
  | Let(x, e1, e2) ->
    compile env e1 @@
    [ MPush ] @
    compile (Env.add env x) e2 @@
    [ MPopN 1 ] @
    cont
  | If(e1, e2, e3) ->
    (* Tutaj musimy zadbać o to, by nie duplikować kodu cont. W tym celu
      posłużymy się operatorem (let^) by go umieścić w jednym miejscu pod
      etykietą l. W obu gałęziach wyrażenia warunkowego na końcu skaczemy
      do l. *)
    let^ l = cont in
    compile_cond env e1
      (compile env e2 [MJump l])
      (compile env e3 [MJump l])
  | Not e ->
    [ MConst 1 ] @
    [ MPush ] @
    compile (Env.push_tmp env) e @@
    [ MSub ] @
    cont
  | And(e1, e2) ->
    let (l, cont) = add_label cont in
    compile env e1 @@
    [ MBranch(MC_Z, l) ] @
    compile env e2 @@
    cont
  | Fun(x, _, e) ->
    let^ l = compile (Env.add (Env.frame env) x) e [ MRet ] in
    [ MGetLabel l ] @
    cont
  | App(e1, e2) ->
    compile env e2 @@
    [ MPush ] @ (* odłóż argument na stos *)
    compile (Env.push_tmp env) e1 @@
    [ MCallAcc ] @
    [ MPopN 1 ] @ (* po powrocie z funkcji usuń argument ze stosu *)
    cont
  | Pair(e1, e2) ->
    [ MAlloc 2 ] @
    [ MPush    ] @
    compile (Env.push_tmp env) e1 @@
    [ MSet   0 ] @
    compile (Env.push_tmp env) e2 @@
    [ MSet   1 ] @
    [ MPopAcc  ] @
    cont
  | Fst e ->
    compile env e @@
    [ MGet   0 ] @
    cont
  | Snd e ->
    compile env e @@
    [ MGet   1 ] @
    cont

(* Funkcja compile_cond jest podobna do compile, ale przyjmuje dwie
  kontynuacje: tcont, do której należy przejść, jeśli wyrażenie e jest
  prawdziwe, oraz fcont, do której należy przejść, jeśli e jest
  fałszywe. *)
and compile_cond env e tcont fcont =
  match e.data with
  | Bool true  -> tcont
  | Bool false -> fcont
  | Binop((Lt | Gt | Leq | Geq | Neq | Eq) as op, e1, e2) ->
    let^ l = tcont in
    compile env e1 @@
    [ MPush ] @
    compile (Env.push_tmp env) e2 @@
    [ MBranch(compile_cmp_op op, l) ] @
    fcont
  | Not e ->
    (* Tu wystarczy zamienić kontynuacje miejscami. *)
    compile_cond env e fcont tcont
  | And(e1, e2) ->
    let^ l = fcont in
    compile_cond env e1
      (compile_cond env e2 tcont [MJump l])
      [MJump l]
  | _ ->
    let^ l = fcont in
    compile env e @@
    [ MBranch(MC_Z, l) ] @
    tcont

let compile_program e =
  let l = fresh_label () in
  compile Env.empty e [ MJump l ] @ [ MLabel l ]

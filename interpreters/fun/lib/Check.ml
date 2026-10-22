
(* zdanie 5. *)
let rec free_vars (e : Ast.expr) : Ast.ident list =  (* zadanie 5. *)
  match e with
  | Var i -> [i]
  | Int _ | Bool _ | Unit -> []
  | Binop (_, e1, e2) -> (free_vars e1) @ (free_vars e2)
  | Pair (e1, e2) | App (e1, e2) -> (free_vars e1) @ (free_vars e2)
  | If (e1, e2, e3) -> (free_vars e1) @ (free_vars e2) @ (free_vars e3) (*to i poprzednie dodają do listy wolnych*)
  | Fun (i, body) -> 
    let vars = free_vars body in 
    List.filter (fun x -> x <> i) vars (*usuwam te ktore sa zwiazane przez funkcję*)
  | Funrec (f, i, body) -> 
    let vars = free_vars body in 
    List.filter (fun x -> x <> f && x <> i) vars 
  | Let (i, e1, e2) -> (*let x = y + 1 in x + 2 *)
    let vars_in_e1 = free_vars e1 in 
    let vars_in_e2 = free_vars e2 in 
    let vars_filtered = List.filter (fun x -> x <> i) vars_in_e2 in
      vars_in_e1 @ vars_filtered 
  | Match (e1, i1, i2, e2) -> (*match e1 with (i1, i2) -> e2*)
    let vars_in_e1 = free_vars e1 in 
    let vars_in_e2 = free_vars e2 in 
    let filtered_e2 = List.filter (fun x -> x <> i1 && x <> i2) vars_in_e2 in
    vars_in_e1 @ filtered_e2

(* zadanie 6. *)
let rec unused_vars (e : Ast.expr) : unit =
  match e with
  | Var _ | Int _ | Bool _ | Unit -> ()
  | Binop (_, e1, e2) | Pair (e1, e2) | App (e1, e2) ->
      unused_vars e1;
      unused_vars e2
  | If (e1, e2, e3) ->
      unused_vars e1;
      unused_vars e2;
      unused_vars e3
  | Fun (i, body) ->
    unused_vars body;
    if i <> "_" && not (List.mem i (free_vars body)) then (*jeśli zmienna nie jest wolna w ciele to jest nieużywana*)
      Printf.printf "Warning: unused variable %s\n" i
  | Funrec (f, i, body) ->
    unused_vars body;
    if i <> "_" && not (List.mem i (free_vars body)) then
      Printf.printf "Warning: unused variable %s\n" i
    else if f <> "_" && not (List.mem f (free_vars body)) then
      Printf.printf "Warning: unused variable %s\n" f 
  | Let (i, e1, e2) ->
    unused_vars e1;
    unused_vars e2;
    if i <> "_" && not (List.mem i (free_vars e2)) then
      Printf.printf "Warning: unused variable %s\n" i
  | Match (e1, i1, i2, e2) -> (*match e1 with (i1, i2) -> e2*)
    unused_vars e1;
    unused_vars e2;
    if i1 <> "_" && not (List.mem i1 (free_vars e2)) then
      Printf.printf "Warning: unused variable %s\n" i1
    else if i2 <> "_" && not (List.mem i2 (free_vars e2)) then
      Printf.printf "Warning: unused variable %s" i2

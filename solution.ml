(* ustalenia:
  - limit "zachowuje" n wierszy licząc od góry tabeli 
  - row_number dodaje nowy numerowany wiersz na początku tabeli (po lewej)
  - add_column dodaje kolumnę na koniec tabeli (po prawej)
*)

type column = string

type row = Eval.value list

type table = column list * row list

type q_options =
  | Limit of int
  | DropColumn of string
  | RowNumber of string
  | Filter of Ast.expr
  | AddColumn of string * Ast.expr

type query = q_options list

(*LIMIT*)
let limit i t = 
  match t with(cols, rows) -> 
    (cols, List.take i rows)

(*DROP COLUMN*)
let remove_nth n lst = 
   List.filteri (fun idx _ -> idx <> n) lst

let drop_column name t = 
  match t with (cols, rows) ->
    let n = List.find_index (fun x -> x = name) cols in 
      match n with 
      | Some n -> (remove_nth n cols, List.map (fun x -> remove_nth n x) rows)
      | None -> failwith "The name of the column doesn't exist"

(*ROW NUMBER*)
let row_number name t =
  match t with (cols, rows) ->
    let rec add_numbers i rs =
      match rs with 
      | [] -> []
      | r :: rs' -> (Eval.VInt i :: r) :: add_numbers (i + 1) rs'
    in (name :: cols, add_numbers 0 rows)

(*FILTER*)
let rec collect_env cols row env = 
  match cols, row with
  | c :: cs, r :: rs -> collect_env cs rs (Eval.Env.add c r env)
  | _, _ -> env

let eval_e cols row e =
  let env = collect_env cols row Eval.Env.empty 
  in Eval.eval env e

let filter e t = 
  match t with (cols, rows) ->
    (cols, List.filter (fun r -> eval_e cols r e = VBool true) rows)

(*ADD COLUMN*)
let add_column c e t=
    match t with (cols, rows) ->
      (cols @ [c], List.map (fun r -> r @ [eval_e cols r e]) rows)

let eval (q : query) (t : table) : table = 
  let rec iter qs t = 
    match qs with 
    | [] -> t
    | q :: qs' ->
      begin match q with 
      | Limit i -> iter qs' (limit i t)
      | DropColumn c -> iter qs' (drop_column c t)
      | RowNumber c -> iter qs' (row_number c t)
      | Filter e -> iter qs' (filter e t)
      | AddColumn (c, e) -> iter qs' (add_column c e t)
      end
  in iter q t
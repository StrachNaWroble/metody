open VM

let rec eval code acc stack =
  match code, stack with
  | [], _ -> acc
  | MConst n :: code, _ -> eval code n stack
  | MGetLocal n :: code, _ ->
    eval code (List.nth stack n) stack
  | MPush :: code, _ -> eval code acc (acc :: stack)
  | MPopN n :: code, _ -> eval code acc (List.drop n stack)
  | MAdd :: code, n1 :: stack ->
    eval code (n1 + acc) stack
  | MSub :: code, n1 :: stack ->
    eval code (n1 - acc) stack
  | MMul :: code, n1 :: stack ->
    eval code (n1 * acc) stack
  | MDiv :: code, n1 :: stack ->
    eval code (n1 / acc) stack
  | _ -> failwith "Runtime error"

let run (code : instr list) : int =
  eval code 0 []

open VM

let memmory = Array.make 100 0

let rec eval code acc stack ptr =
  match code, stack with
  | [], _ -> acc
  | MConst n :: code, _ -> eval code n stack ptr
  | MGetLocal n :: code, _ ->
    eval code (List.nth stack n) stack ptr
  | MPush :: code, _ -> eval code acc (acc :: stack) ptr
  | MPopN n :: code, _ -> eval code acc (List.drop n stack) ptr
  | MAdd :: code, n1 :: stack ->
    eval code (n1 + acc) stack ptr
  | MSub :: code, n1 :: stack ->
    eval code (n1 - acc) stack ptr
  | MMul :: code, n1 :: stack ->
    eval code (n1 * acc) stack ptr
  | MDiv :: code, n1 :: stack ->
    eval code (n1 / acc) stack ptr
  | MPopAcc :: code, n1 :: stack -> (* Zadanie 5. *)
    eval code n1 stack ptr
  | MAlloc n :: code, stack -> 
    eval code ptr stack (ptr + n)
  | MGet n :: code, stack ->
    eval code (Array.get memmory (n + acc)) stack ptr
  | MSet n :: code, adr :: stack -> 
    Array.set memmory (adr + n) acc;
    eval code acc (adr :: stack) ptr
  | _ -> failwith "Runtime error"


let run (code : instr list) : int =
  eval code 0 [] 0

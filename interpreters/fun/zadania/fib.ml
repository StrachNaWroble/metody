let rec fib_ml n =
  if n = 0 then 0
  else if n = 1 then 1
  else (fib_ml (n-1)) + (fib_ml (n-2));;


let time f x =
let t = Sys.time () in
let fx = f x in
Printf.printf " Execution time: %fs\n" ( Sys.time () -. t );
fx

(*
utop:

time fib 40;;
 Execution time: 2.734438s
*)

(*
ocamlopt:

ocamlopt -I zadania -o fib_ocaml zadania/fib.ml zadania/test.ml
 Execution time: 0.465118s
*)

(*
FUN
#load zadania/fib.fun
fib_fun 40
(Execution time: 68.585308s)
*)
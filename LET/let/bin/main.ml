let welcome () =
  print_string
{|
  #     ####  #####
  #     #       #
  #     ###     #
  #     #       #
  ####  ####    #
|}

let rec prompt () =
  print_newline ();
  print_string "> ";
  flush stdout;
  match read_line () with
  | "" -> prompt ()
  | s -> s

let rec repl () =
  try
    prompt () |> Let.Eval.interp |> Let.Eval.print_value |> repl
  with
  | Failure msg -> print_string ("FAILED: " ^ msg) |> repl
  | Division_by_zero -> print_string ("FAILED: division by zero") |> repl

let () =
  welcome ();
  try repl () with
    End_of_file -> print_endline "bye!"

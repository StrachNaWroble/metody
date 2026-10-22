let load_file (path : string) : string =
  In_channel.with_open_text path In_channel.input_all

let exec (file : string) : unit =
  try
    load_file file |> While.Eval.interp
  with
  | Failure msg | Sys_error msg ->
      print_endline ("FAILED: " ^ msg)
  | Division_by_zero ->
      print_endline ("FAILED: division by zero")

let () =
  if Array.length Sys.argv > 2 then
    let arg1 = Sys.argv.(1) in
    let arg2 = Sys.argv.(2) in
    if arg1 = "pretty_print" then
      let s = While.Parser.main While.Lexer.read (Lexing.from_string (load_file arg2)) in
      print_endline (While.Pretty_printer.pretty_print s)
    else
      exec arg1
  else if Array.length Sys.argv > 1 then
    let arg1 = Sys.argv.(1) in
    exec arg1
  else
    print_endline "No argument provided. Run with \"while <FILENAME>\""

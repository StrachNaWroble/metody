let process_program prog =
  try
    Prac3.Solution.check_program prog;
    Prac3.Eval.run_program prog
  with
  | Prac3.Solution.Type_error ->
    Printf.eprintf "Type error\n";
    exit 1

let parse_input lexbuf =
  try Prac3.Parser.main Prac3.Lexer.token lexbuf with
  | Prac3.Parser.Error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Printf.eprintf ":%s:%d:%d: Unexpected token '%s'\n"
        pos.Lexing.pos_fname
        pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
        (Lexing.lexeme lexbuf);
      exit 1
  | Prac3.Lexer.Error c ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Printf.eprintf ":%s:%d:%d: Illegal character '%s'\n"
        pos.Lexing.pos_fname
        pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
        (Char.escaped c);
      exit 1

let process_string str =
  let lexbuf = Lexing.from_string str in
  process_program (parse_input lexbuf)

let process_file fname =
  let chan = open_in fname in
  let lexbuf = Lexing.from_channel chan in
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = fname };
  let prog   = parse_input lexbuf in
  close_in chan;
  process_program prog

let usage_string =
  Printf.sprintf "Usage: %s OPTIONs [FILE]\nAvailable OPTIONs are:" Sys.argv.(0)

let cmd_args_options =
  Arg.align [
    "-input", Arg.String process_string,
    "INPUT Read the input program from the command line instead of a file"
  ]

let () =
  Arg.parse cmd_args_options process_file usage_string

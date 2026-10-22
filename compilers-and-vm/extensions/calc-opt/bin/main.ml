type mode =
  | Help
  | DumpVM
  | DumpAsm
  | Compile

let mode = ref Help
let output_file = ref "a.out"

let write_output content =
  let chan = open_out_bin !output_file in
  output_bytes chan content;
  close_out chan

let process_program (prog : Calc.Ast.expr) =
  match !mode with
  | Help    -> ()
  | DumpVM  ->
    prog
    |> Calc.Compiler.compile_program
    |> List.iter (fun i -> print_endline (Calc.VM.string_of_instr i))
  | DumpAsm ->
    prog
    |> Calc.Compiler.compile_program
    |> Calc.VM.asm_of_instrs
    |> Fun.flip (@) Calc.Platform.epilog
    |> List.iter (fun i -> print_endline (Calc.MPU6809.string_of_instr i))
  | Compile ->
    prog
    |> Calc.Compiler.compile_program
    |> Calc.VM.asm_of_instrs
    |> Fun.flip (@) Calc.Platform.epilog
    |> Calc.Assembler.assemble
    |> write_output

let parse_input lexbuf =
  try Calc.Parser.main Calc.Lexer.read lexbuf with
  | Calc.Parser.Error ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Printf.eprintf ":%s:%d:%d: Unexpected token '%s'\n"
        pos.Lexing.pos_fname
        pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
        (Lexing.lexeme lexbuf);
      exit 1
  | Calc.Lexer.Error c ->
      let pos = lexbuf.Lexing.lex_curr_p in
      Printf.eprintf ":%s:%d:%d: Illegal character '%s'\n"
        pos.Lexing.pos_fname
        pos.Lexing.pos_lnum
        (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)
        (Char.escaped c);
      exit 1

let process_string str =
  let lexbuf = Lexing.from_string str in
  let prog   = parse_input lexbuf in
  process_program prog

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
    "-dump-vm", Arg.Unit (fun () -> mode := DumpVM),
    " Compile the FILE and print the VM code";

    "-dump-asm", Arg.Unit (fun () -> mode := DumpAsm),
    " Compile the FILE and print the assembly code";

    "-compile", Arg.Unit (fun () -> mode := Compile),
    " Compile the FILE to 6809";

    "-input", Arg.String process_string,
    "INPUT Read the input program from the command line instead of a file";

    "-o", Arg.String (fun s -> output_file := s),
    "OUTPUT Specify the output file name (default: a.out)"
  ]

let () =
  Arg.parse cmd_args_options process_file usage_string;
  match !mode with
  | Help -> Arg.usage cmd_args_options usage_string
  | _    -> ()

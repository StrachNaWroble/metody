let welcome () =
  print_string
{|
  ####   #    #   ##    #
  #      #    #   # #   #
  ###    #    #   #  #  #
  #      #    #   #   # #
  #       ####    #    ##
|}

let rec prompt () : string =
  print_string "> ";
  flush stdout;
  match read_line () with
  | "" -> prompt ()
  | s -> s

let load_file (path : string) : string =
  In_channel.with_open_text path In_channel.input_all

let rec repl (env : Fun.Eval.env) =
  try
    let p = prompt () in
    if String.starts_with ~prefix:"#load " p
      then
        let s = String.sub p 6 (String.length p - 6) |> String.trim |> load_file in
        let env' = Fun.Eval.interp_prog env s in
        repl env'
      else
        (* zadanie 4. *)
        let start = Sys.time () in
        let result = Fun.Eval.interp env p in
        let stop = Sys.time () in
        result |> Fun.Eval.print_value;
        Printf.printf "\n(Execution time: %fs)\n" (stop -. start);
        repl env
  with
  | Failure msg | Sys_error msg ->
      print_endline ("FAILED: " ^ msg);
      repl env
  | Division_by_zero ->
      print_endline ("FAILED: division by zero");
      repl env

let () =
  welcome ();
  try repl Fun.Eval.Env.empty with
  | End_of_file -> print_endline "bye!"

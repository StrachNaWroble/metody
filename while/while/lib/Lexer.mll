{
  open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let number = digit+
let ident = ['_' 'a'-'z' 'A'-'Z']['_' 'a'-'z' 'A'-'Z' '0'-'9']*

rule read =
  parse
  | white { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "if" { IF }
  | "else" { ELSE }
  | "while" { WHILE }
  | "print" { PRINT }
  | "skip" { SKIP }
  | "*" { MULT }
  | "+" { ADD }
  | "-" { SUB }
  | "/" { DIV }
  | ">" { GT }
  | ">=" { GEQ }
  | "<" { LT }
  | "<=" { LEQ }
  | "<>" { NEQ }
  | "==" { EQEQ }
  | "=" { EQ }
  | ";" { SC }
  | "halt" { HALT } (* zadanie 7. *)
  | number { INT (Lexing.lexeme lexbuf
                  |> int_of_string) }
  | ident { IDENT (Lexing.lexeme lexbuf) }
  | "(" { LPAR }
  | ")" { RPAR }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | '"' { read_string (Buffer.create 16) lexbuf }
  | eof { EOF }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '"'  { Buffer.add_char buf '"';  read_string buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | _ as c    { Buffer.add_char buf c;    read_string buf lexbuf }
  | eof       { failwith "unterminated string literal" }

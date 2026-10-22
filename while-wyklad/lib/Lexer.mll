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
  | "for" { FOR } (* Zadanie 1. *)
  | "halt" { HALT } (* Zadanie 7. *)
  | "*" { MULT }
  | "+" { ADD }
  | "++" { PLUSPLUS }
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
  | number { INT (Lexing.lexeme lexbuf
                  |> Bigint.of_string) } (* Zadanie 2. *)
  | ident { IDENT (Lexing.lexeme lexbuf) } 
  | "(" { LPAR }
  | ")" { RPAR }
  | "{" { LBRACE }
  | "}" { RBRACE }
  | "[" { LBRACK } (* Zadanie 4. *)
  | "]" { RBRACK } 
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

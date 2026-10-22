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
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "match" { MATCH }
  | "with" { WITH }
  | "fun" { FUN }
  | "funrec" { FUNREC }
  | "def" { DEF }
  | "rec" { REC } (* zadanie 1. *)
  | "->" { ARR }
  | "=" { EQ }
  | "in" { IN }
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
  | number { INT (Lexing.lexeme lexbuf
                  |> int_of_string) }
  | ident { IDENT (Lexing.lexeme lexbuf) }
  | "(" { LPAR }
  | ")" { RPAR }
  | "," { COMMA }
  | ident { IDENT (Lexing.lexeme lexbuf) }
  | number { INT (Lexing.lexeme lexbuf
                  |> int_of_string) }
  | eof { EOF }

{
  open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let number = digit+
let ident = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9']*

rule read =
  parse
  | white { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "fst" { FST } (* zadanie 4. *)
  | "snd" { SND } (* zadanie 4.*)
  | "match" { MATCH } (* zadanie 5. *)
  | "with" { WITH } (* zadanie 5. *)
  | "sum" { SUM } (* zadanie 6. *)
  | "to" { TO } (* zadanie 6. *) 
  | "in" { IN } (* zadanie 6. *)
  | "->" { ARROW } (* zadanei 5. *)
  | "=" { EQ }
  | "*" { MULT }
  | "+" { ADD }
  | "-" { SUB }
  | "/" { DIV }
  | number { INT (Lexing.lexeme lexbuf
                  |> int_of_string) }
  | ident { IDENT (Lexing.lexeme lexbuf) }
  | "(" { LPAR }
  | ")" { RPAR }
  | ',' { COMMA} (* zadanie 4.*)
  | eof { EOF }

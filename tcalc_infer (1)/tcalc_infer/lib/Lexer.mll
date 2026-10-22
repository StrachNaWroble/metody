{
  open Parser

  exception Error of char
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let number = digit+
let id_start = ['a'-'z' 'A'-'Z' '_']
let ident  = id_start (id_start | digit)*

rule read =
  parse
  | white { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "let" { LET }
  | "in"  { IN  }
  | "fun" { FUN }
  | "not" { NOT }
  | "and" { AND }
  | "fst" { FST }
  | "snd" { SND }
  | "int" { KW_INT }
  | "bool" { KW_BOOL }
  | "list" { LIST }
  | "[" { LBRACKET }
  | "]" { RBRACKET}
  | "&&" { AND }
  | "->"  { ARROW }
  | ","   { COMMA }
  | ":"   { COLON }
  | "*" { MULT }
  | "+" { ADD }
  | "-" { SUB }
  | "/" { DIV }
  | "=" { EQ  }
  | "<" { LT }
  | ">" { GT }
  | "<=" { LEQ }
  | ">=" { GEQ }
  | "<>" { NEQ }
  | "==" { EQEQ }
  | "_"  { UNDERSCORE }
  | number as x { INT (int_of_string x) }
  | ident as x  { IDENT x }
  | "(" { LPAR }
  | ")" { RPAR }
  | eof { EOF }
  | _ as c { raise (Error c) }

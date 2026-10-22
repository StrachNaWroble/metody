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
  | "*" { MULT }
  | "+" { ADD }
  | "-" { SUB }
  | "/" { DIV }
  | "=" { EQ  }
  | "let" { LET }
  | "in"  { IN  }
  | number as x { INT (int_of_string x) }
  | ident as x  { IDENT x }
  | "(" { LPAR }
  | ")" { RPAR }
  | eof { EOF }
  | _ as c { raise (Error c) }

{
  open Parser

  exception Error of char
}

let whitespace = [' ' '\t' '\r']
let digit = ['0'-'9']
let id_start = ['a'-'z' 'A'-'Z' '_']
let id_char = id_start | digit | ['\'']

rule token = parse
  | whitespace { token lexbuf }
  | '\n'       { Lexing.new_line lexbuf; token lexbuf }
  | "("        { LPAR   }
  | ")"        { RPAR   }
  | "["        { LBRK   }
  | "]"        { RBRK   }
  | "{"        { LBRC   }
  | "}"        { RBRC   }
  | "->"       { ARROW  }
  | "*"        { ASTER  }
  | ":"        { COLON  }
  | ","        { COMMA  }
  | "="        { EQ     }
  | ">="       { GEQ    }
  | ">"        { GT     }
  | "#"        { HASH   }
  | "<="       { LEQ    }
  | "<"        { LT     }
  | "-"        { MINUS  }
  | "<>"       { NEQ    }
  | "+"        { PLUS   }
  | ";"        { SEMI   }
  | "bool"     { BOOL   }
  | "else"     { ELSE   }
  | "false"    { FALSE  }
  | "fun"      { FUN    }
  | "if"       { IF     }
  | "in"       { IN     }
  | "int"      { INT    }
  | "let"      { LET    }
  | "read"     { READ   }
  | "return"   { RETURN }
  | "then"     { THEN   }
  | "true"     { TRUE   }
  | "write"    { WRITE  }
  | id_start id_char* as id { IDENT id }
  | digit+ as n             { NUM (int_of_string n) }
  | eof        { EOF }
  | _ as c     { raise (Error c) }

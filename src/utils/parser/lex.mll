{
    open Lexing
    open Par

    exception SyntaxError of string
}

(* Definitions for textual structure *)
let espace   = [' ' '\t']+
let typ_name = ['A'-'Z'] ['a'-'z' '0'-'9']*
let var_name = ['A'-'Z' '0'-'9'] ['A'-'Z' '0'-'9']*
let bTerm = ['A'-'Z' '0'-'9'] ['A'-'Z' '0'-'9']*
let name = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9']*
let newLine = '\r' | '\n' | "\r\n"

(*  *)
rule lexer = parse
  | newLine         { new_line lexbuf; lexer lexbuf }
  | espace          { lexer lexbuf }
  | "(*"            { commentaire lexer lexbuf }
  | "("             { LPAREN  }
  | ")"             { RPAREN  }
  | "["             { LBRACE  }
  | "]"             { RBRACE  }
  (* | ":"             { COLON   } *)
  | ";"             { SEP     }
  (* | "types"         { TY_ID   } *)
  (* | "signature"     { SIG_ID  } *)
  (* | "vars"          { VAR_ID  } *)
  (* | "rules"         { RULE_ID } *)
  | "==>"           { RW_ARR }
  | "-->"           { TY_ARR   }
  (* | "^"             { LAMBDA } *)
  (* | "."             { LBD_SPLIT } *)
  (* | "@"             { APP    } *)
  (* | bTerm           { BTERM (Lexing.lexeme lexbuf) } *)
  | var_name         { VNAME (Lexing.lexeme lexbuf) }
  | name            { STRING (Lexing.lexeme lexbuf) }
  | _               { raise (SyntaxError ("Unexpected character: " ^ Lexing.lexeme lexbuf))}
  | eof             { EOF }

and commentaire continuation = parse
  | "*)" { continuation lexbuf }
  | "(*" { commentaire (commentaire continuation) lexbuf }
  | _    { commentaire continuation lexbuf }

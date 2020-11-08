{
open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let float = digit+ '.' digit+
let int = digit+
let chars = ['a'-'z' 'A'-'Z' '_']
let string = chars+

rule read = 
  parse
  | white { read lexbuf }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | string { STRING (Lexing.lexeme lexbuf) }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { OVER }
  | "%" { MOD }
  | "^" { TOTHEPOWER }
  | "=" { EQUALS }
  | ">" { GT }
  | "<" { LT }
  | ">=" { GTE }
  | "<=" { LTE }
  | "&" { AND }
  | "|" { OR  }
  | "~" { NOT }
  | "->" { PASSTO }
  | "[" { LBRACK }
  | "]" { RBRACK }
  | "," { COMMA }
  | "::" { CONS }
  | "." { DOT }
  | "\"" { QUOTE }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "fun" { FUN }
  | "let" { LET }
  | "in" { IN }
  | "true" { TRUE }
  | "false" { FALSE }
  | eof { EOF; }
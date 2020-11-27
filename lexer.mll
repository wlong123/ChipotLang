{
open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let float = digit+ '.' digit+
let int = digit+
let comment = "<<" ['a'-'z' 'A'-'Z' '0'-'9' ''' '_' ' ' '\t' '\n' ',' ';' '.']* ">>"
let string = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' ''' '_']* 

rule read = 
  parse
  | white { read lexbuf }
  | comment { read lexbuf }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { OVER }
  | "%" { MOD }
  | "^" { TOTHEPOWER }
  | "=" { EQUALS }
  | "!=" { NOTEQUALS }
  | ">" { GT }
  | "<" { LT }
  | ">=" { GTE }
  | "<=" { LTE }
  | "and" { AND }
  | "or" { OR  }
  | "not" { NOT }
  | "=>" { PASSTO }
  | "[" { LBRACK }
  | "]" { RBRACK }
  | "," { COMMA }
  | "::" { CONS }
  | "." { DOT }
  | "\"" { QUOTE }
  | "if" { IF }
  | "then" { THEN }
  | "|" { CASE }
  | "else" { ELSE }
  | "fun" { FUN }
  | "def" { LET }
  | "in" { IN }
  | "true" { TRUE }
  | "false" { FALSE }
  | "thread" { CTHREAD }
  | "kill" { KILL }
  | "tid" { TID }
  | "fork" { FORK }
  | "join" { JOIN }
  | "joinall" { JOINALL }
  | "print" { PRINT }
  | "lock" { LOCK }
  | "unlock" { UNLOCK }
  | string { STRING (Lexing.lexeme lexbuf) }
  | eof { EOF; }
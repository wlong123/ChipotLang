{
  open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let symbol = ['(' ')' '+' '-' '*' '/' '%' '^' '=' '!' '>'
              '<' '=' '[' ']' ',' ':' '.' '"' '|' ';' 
              '`' '~' '@' '#' '$' '&' '*' ''' '_' '{' '}' '\\' '?']
let float = digit+ '.' digit+
let int = digit+
let comment = "<<" (digit | letter | white | symbol)* ">>"
let string = letter (letter | digit | ''' | '_')* 

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
  | "else" { ELSE }
  | "fun" { FUN }
  | "def" { DEF }
  | "in" { IN }
  | "true" { TRUE }
  | "false" { FALSE }
  | "thread" { CTHREAD }
  | "tid" { TID }
  | "fork" { FORK }
  | "join" { JOIN }
  | "joinall" { JOINALL }
  | "print" { PRINT }
  | "lock" { LOCK }
  | "lockall" { LOCKALL }
  | "unlock" { UNLOCK }
  | "unlockall" { UNLOCKALL }
  | "none" { NONE }
  | "hd" { HEAD }
  | "tl" { TAIL }
  | "!" { DEREF }
  | ":=" { ASSIGN }
  | "ref" { CREATEREF }
  | ";" { SEQSEP }
  | string { STRING (Lexing.lexeme lexbuf) }
  | eof { EOF; }

%{
open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token LPAREN RPAREN PLUS MINUS TIMES OVER MOD TOTHEPOWER NOTEQUALS EQUALS GT LT
  GTE LTE AND OR  NOT PASSTO LBRACK RBRACK COMMA CONS DOT QUOTE IF THEN CASE
  FUN DEF IN TRUE FALSE CTHREAD KILL TID FORK JOIN JOINALL PRINT LOCK UNLOCK
	ASSIGN NONE DEREF SEQSEP CREATEREF EOF

%nonassoc EQUALS NOTEQUALS GT LT GTE LTE
%nonassoc DEF IN
%nonassoc IF ELSE
%nonassoc SEQSEP
%nonassoc ASSIGN
%nonassoc PRINT
%nonassoc DEREF
%left PLUS MINUS
%left TIMES OVER  
%left MOD  
%left TOTHEPOWER

%start <Ast.expr> prog

%%

prog : expr EOF { $1 }

expr :
	| LPAREN; expr; RPAREN { $2 } 
	| expr; SEQSEP; expr { Seq ($1, $3) }
	| str { $1 }
	| arith_expr { $1 }
	| bool_expr { $1 }
	| data_struct { $1 }
	| func { $1 }
	| app { $1 }
	| sync { $1 }
	| constructs { $1 }
	;

str : QUOTE; STRING; QUOTE { String $2 }
	
var : STRING { Var $1 }

num :
	| INT { Int $1 }
	| FLOAT { Float $1 }
	| MINUS; INT { Int (~-$2) }
	| MINUS; FLOAT { Float (~-.$2) }
	;

arith_expr :
	| expr; PLUS; expr { Binop (Add, $1, $3) }
	| expr; MINUS; expr { Binop (Sub, $1, $3) } 
	| var; MINUS; expr { Binop (Sub, $1, $3) } 
	| expr; MINUS; var { Binop (Sub, $1, $3) } 
	// | expr; MINUS; var { Binop (Sub, $1, $3) } 
	| expr; TIMES; expr { Binop (Mul, $1, $3) } 
	| expr; OVER; expr { Binop (Div, $1, $3) } 
	| expr; MOD; expr { Binop (Mod, $1, $3) } 
	| expr; TOTHEPOWER; expr { Binop (Pow, $1, $3) } 
	| num { $1 }
	| var { $1 }
	;
	
bool :
	| TRUE { Bool true }
	| FALSE { Bool false }
	;

bool_expr :
	| expr; EQUALS; expr { Binop (Eq, $1, $3) } 
	| expr; NOTEQUALS; expr { Binop (Neq, $1, $3) } 	
	| expr; GT; expr { Binop (GT, $1, $3) } 
	| expr; LT; expr { Binop (LT, $1, $3) } 
	| expr; GTE; expr { Binop (GTE, $1, $3) } 
	| expr; LTE; expr { Binop (LTE, $1, $3) }
	| expr; AND; expr { Binop (AND, $1, $3) } 
	| expr; OR; expr { Binop (OR, $1, $3) } 
	| NOT; expr { Unop (NOT, $2) }
	| bool { $1 }
	;

expr_list :
	| { [] }
	| expr; RBRACK { $1 :: [] }
	| expr; COMMA; expr_list { $1 :: $3 }
	;

lst : LBRACK; expr_list { List $2 }

data_struct :
	| lst { $1 }
	| expr; CONS; expr { Binop (CONS, $1, $3) }
	| lst; DOT; expr { Binop (PROJ, $1, $3) }
	;

func :
	| FUN; STRING; PASSTO; expr { Fun ($2, $4) }

app :
	| func; expr { App ($1, $2) }
	| var; expr { App ($1, $2) }
	
sync :
	| CTHREAD; app { CThread $2 }
	| KILL; INT { Kill (Int $2) }
	| JOIN; INT { Join (Int $2) }
	| JOINALL { Joinall }
	// | LOCK; expr { Lock $2 }
	// | UNLOCK; expr { Unlock $2 }
	;

constructs :
	| IF; expr; THEN; CASE; expr; CASE; expr { If ($2, $5, $7) }
	| DEF; expr; IN; expr { Def ($2, $4) }
	| NONE { None }
	| CREATEREF; expr { CreateRef $2 }
	| DEREF; STRING { Deref $2 }
	| STRING; ASSIGN; expr { RefAssign ($1, $3) }
	| PRINT; expr { Print $2 }
	;

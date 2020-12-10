
%{
open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token LPAREN RPAREN PLUS MINUS TIMES OVER MOD TOTHEPOWER NOTEQUALS EQUALS GT LT
  GTE LTE AND OR NOT PASSTO LBRACK RBRACK COMMA CONS DOT QUOTE IF THEN CASE
  FUN DEF IN TRUE FALSE CTHREAD KILL TID FORK JOIN JOINALL PRINT LOCK UNLOCK
	LOCKALL UNLOCKALL ASSIGN NONE DEREF SEQSEP CREATEREF EOF

%right DEF IN FUN PASSTO IF THEN CASE
%right SEQSEP
%nonassoc NOTEQUALS EQUALS GT LT GTE LTE AND OR NOT TRUE FALSE NONE
	CTHREAD TID FORK JOIN JOINALL LOCK UNLOCK LOCKALL
	PRINT QUOTE ASSIGN CREATEREF 
	LBRACK RBRACK COMMA CONS DOT     
%left MOD  
%left PLUS MINUS
%left TIMES OVER  
%left TOTHEPOWER
%right LPAREN RPAREN 
%right DEREF

%start <Ast.expr> prog

%%

prog : expr EOF { $1 }

expr :
	| LPAREN; expr; RPAREN { $2 } 
	| expr; SEQSEP; expr { Seq ($1, $3) }
	| str { $1 }
	| arith_expr { $1 }
	| bool_expr { $1 }
	| unop_expr { $1 }
	| data_struct { $1 }
	| func { $1 }
	| app { $1 }
	| constructs { $1 }
	| NONE { None }
	;

str : QUOTE; STRING; QUOTE { String $2 }
	
var : STRING { Var $1 }

num :
	| MINUS; INT { Int (~-$2) }
	| MINUS; FLOAT { Float (~-.$2) }
	| INT { Int $1 }
	| FLOAT { Float $1 }
	;

%inline arith_binop : 
	| PLUS { Add }
	| MINUS { Sub }
	| TIMES { Mul }
	| OVER { Div }
	| MOD { Mod }
	| TOTHEPOWER { Pow }

arith_expr :
	| expr; arith_binop; expr { Binop ($2, $1, $3) }
	| var { $1 }
	| num { $1 }
	;
	
bool :
	| TRUE { Bool true }
	| FALSE { Bool false }
	;

%inline bool_binop :
	| EQUALS { Eq }
	| NOTEQUALS { Neq }
	| GT { GT }
	| LT { LT }
	| GTE { GTE }
	| LTE { LTE }
	| AND { And }
	| OR { Or }
	;

bool_expr :
	| expr; bool_binop; expr { Binop ($2, $1, $3) } 
	| bool { $1 }
	;

%inline unop :
	| NOT { Not }
	| JOIN {Join }
	| LOCK { Lock }
	| UNLOCK { Unlock }
	| LOCKALL { Lockall }
	| UNLOCKALL { Unlockall }

unop_expr :
	| unop; expr { Unop ($1, $2) }
	| JOINALL { Unop (Joinall, None) }
	| PRINT; LPAREN; expr; RPAREN { Unop (Print, $3) }
	| CTHREAD; LPAREN; expr; RPAREN { Unop (CThread, $3) }


expr_list :
	| RBRACK { [] }
	| expr; RBRACK { $1 :: [] }
	| expr; COMMA; expr_list { $1 :: $3 }
	;

lst : LBRACK; expr_list { List $2 }

data_struct :
	| lst { $1 }
	| expr; CONS; expr { Binop (Cons, $1, $3) }
	| lst; DOT; expr { Binop (Proj, $1, $3) }
	;

func :
	| FUN; STRING; PASSTO; expr { Fun ($2, $4) }

app :
	| func; expr { App ($1, $2) }
	| var; LPAREN; expr; RPAREN { App ($1, $3) }

constructs :
	| IF; expr; THEN; CASE; expr; CASE; expr { If ($2, $5, $7) }
	| DEF; expr; IN; expr { Def ($2, $4) }
	| NONE { None }
	| CREATEREF; expr { CreateRef $2 }
	| DEREF; STRING { Deref $2 }
	| STRING; ASSIGN; expr { RefAssign ($1, $3) }
	;

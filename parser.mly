
%{
open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> STRING
%token LPAREN
%token RPAREN
%token PLUS
%token MINUS
%token TIMES
%token OVER
%token MOD
%token TOTHEPOWER
%token EQUALS
%token GT
%token LT
%token GTE
%token LTE
%token AND
%token OR 
%token NOT
%token PASSTO
%token LBRACK
%token RBRACK
%token COMMA
%token CONS
%token DOT
%token COLON
%token QUOTE
%token IF
%token THEN
%token ELSE
%token FUN
%token LET
%token IN
%token TRUE
%token FALSE
%token EOF

%nonassoc EQUALS
%nonassoc GT
%nonassoc LT
%nonassoc GTE
%nonassoc LTE
%left PLUS
%left MINUS
%left TIMES
%left OVER  
%left MOD  
%left TOTHEPOWER  

%start <Ast.expr> prog

%%
assoc_list:
  | (* empty *) { [] }
  | k = expr; COLON; v = expr; COMMA; tail = expr_list
    { (k, v) :: tail }

expr_list:
  | (* empty *) { [] }
  | e = expr; COMMA; tail = expr_list
    { e :: tail }

prog: expr EOF { $1 }

expr:
	| x = STRING { Var x }
	| i = INT { Int i }
	| f = FLOAT { Float f }
	| MINUS; i = INT { Int (~-i) }
	| MINUS; f = FLOAT { Float (~-.f) }
	| TRUE { Bool true }
	| FALSE { Bool false }
	| e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
	| e1 = expr; MINUS; e2 = expr { Binop (Sub, e1, e2) } 
	| e1 = expr; TIMES; e2 = expr { Binop (Mul, e1, e2) } 
	| e1 = expr; OVER; e2 = expr { Binop (Div, e1, e2) } 
	| e1 = expr; MOD; e2 = expr { Binop (Mod, e1, e2) } 
	| e1 = expr; TOTHEPOWER; e2 = expr { Binop (Pow, e1, e2) } 
	| e1 = expr; EQUALS; e2 = expr { Binop (Eq, e1, e2) } 
	| e1 = expr; GT; e2 = expr { Binop (GT, e1, e2) } 
	| e1 = expr; LT; e2 = expr { Binop (LT, e1, e2) } 
	| e1 = expr; GTE; e2 = expr { Binop (GTE, e1, e2) } 
	| e1 = expr; LTE; e2 = expr { Binop (LTE, e1, e2) } 
	| e1 = expr; AND; e2 = expr { Binop (And, e1, e2) } 
	| e1 = expr; OR; e2 = expr { Binop (Or, e1, e2) } 
	| NOT; e = expr { Uniop (Not, e) } 
	| LBRACK; contents = expr_list; RBRACK { Assoc contents }
	| LBRACK; contents = assoc_list; RBRACK { List contents }
	| LPAREN; contents = tuple; RPAREN { TODO }
	| e1 = expr; CONS; e2 = expr { TODO }
	| IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { TODO }
	| LET; e1 = expr; IN; e2 = expr { TODO }
	| FUN; x1 = ID; PASSTO; e = expr { TODO }
	| LPAREN; e=expr; RPAREN { e } 
	;

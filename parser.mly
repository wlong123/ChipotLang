
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

expr_list:
| { [] }
| e = expr; COMMA; tail = expr_list { e :: tail }

prog: expr EOF { $1 }

expr:
  | QUOTE; s = STRING; QUOTE { String s }
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
	| e1 = expr; AND; e2 = expr { Binop (AND, e1, e2) } 
	| e1 = expr; OR; e2 = expr { Binop (OR, e1, e2) } 
	| NOT; e = expr { Unop (NOT, e) } 
	| LBRACK; contents = expr_list; RBRACK { List contents }
	| e1 = expr; CONS; e2 = expr { Binop (CONS, e1, e2) }
	| IF; e1 = expr; THEN; e2 = expr; ELSE; e3 = expr { If (e1, e2, e3) }
	| LET; e1 = expr; IN; e2 = expr { Let (e1, e2) }
	| FUN; x1 = STRING; PASSTO; e = expr { Fun (x1, e) }
	| LPAREN; e=expr; RPAREN { e } 
	| e1 = expr; DOT; e2 = expr { Binop (PROJ, e1, e2) }
	;

%{
open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> ID
%token PLUS
%token TIMES  
%token OVER  
%token MOD
%token TOTHEPOWER
%token EQUALS
%token GT
%token LT
%token GTE
%token LTE
%token LPAREN
%token RPAREN
%token MINUS  
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
prog: expr EOF                           { $1 }

expr:
	| x = ID { Var x }
	| i = INT { Int i }
	| f = FLOAT { Float f }
	| MINUS; i = INT { Int (~-i) }
	| MINUS; f = FLOAT { Float (~-.f) }
	| e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
	| e1 = expr; MINUS; e2 = expr { Binop (Sub, e1, e2) } 
	| e1 = expr; TIMES; e2 = expr { Binop (Mul, e1, e2) } 
	| i = INT; x = ID { Binop (Mul, Int i, Var x) } 
	| f = FLOAT; x = ID { Binop (Mul, Float f, Var x) } 
	| MINUS; i = INT; x = ID { Binop (Mul, Int ~-i, Var x) } 
	| MINUS; f = FLOAT; x = ID { Binop (Mul, Float ~-.f, Var x) } 
	| e1 = expr; OVER; e2 = expr { Binop (Div, e1, e2) } 
	| e1 = expr; MOD; e2 = expr { Binop (Mod, e1, e2) } 
	| e1 = expr; TOTHEPOWER; e2 = expr { Binop (Pow, e1, e2) } 
	| e1 = expr; EQUALS; e2 = expr { Binop (Eq, e1, e2) } 
	| e1 = expr; GT; e2 = expr { Binop (GT, e1, e2) } 
	| e1 = expr; LT; e2 = expr { Binop (LT, e1, e2) } 
	| e1 = expr; GTE; e2 = expr { Binop (GTE, e1, e2) } 
	| e1 = expr; LTE; e2 = expr { Binop (LTE, e1, e2) } 
	| LPAREN; e=expr; RPAREN { e } 
	;
	
type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Pow
  | Eq
  | LT
  | GT
  | LTE
  | GTE
  | AND
  | OR
  | CONS
  | PROJ

type unop =
  | NOT

type expr = 
  | Var of string
  | Int of int
  | Float of float
  | Bool of bool
  | String of string
  | Unop of unop * expr
  | Binop of binop * expr * expr
  | If of expr * expr * expr
  | Let of expr * expr
  | Fun of string * expr
  | List of expr list

let string_of_binop = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Mod -> "Mod"
  | Pow -> "Pow"  
  | Eq -> "Eq"
  | LT -> "LT"
  | GT -> "GT"
  | LTE -> "LTE"
  | GTE -> "GTE"
  | AND -> "AND"
  | OR -> "OR"
  | CONS -> "CONS"
  | PROJ -> "PROJ"

let string_of_unop = function
  | NOT -> "NOT"

let rec string_of_expr = function
  | Var x -> "Var " ^ x
  | Int i -> "Int " ^ string_of_int i
  | Float f -> "Float " ^ string_of_float f
  | Bool b -> "Bool " ^ string_of_bool b
  | String s -> "String " ^ s
  | Unop (op ,e) -> "Uniop (" ^ (string_of_unop op) ^ ", " ^ (string_of_expr e) ^ ")"
  | Binop (op, e1, e2) -> "Binop (" ^ (string_of_binop op) ^ ", " ^
                          (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
  | If (e1, e2, e3) -> "If (" ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ", " ^ (string_of_expr e3) ^ ")"
  | Let (e1, e2) -> "Let (" ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
  | Fun (id, e) -> "Fun (" ^ id ^ " -> " ^ (string_of_expr e) ^ ")"
  | List [] -> "List ([])"
  | List (h::t) -> "List (" ^ (string_of_expr h) ^ ", " ^ (string_of_expr (List t))  ^ ")"
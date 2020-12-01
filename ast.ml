open Thread
type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Pow
  | Neq
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
  | Def of expr * expr
  | Fun of string * expr
  | List of expr list
  | App of expr * expr
  | CThread of expr
  | Tid of Thread.t
  | Kill of expr
  | Print of expr
  | Lock of expr
  | Unlock of expr
  | Join of expr
  | Joinall
  | None
  | CreateRef of expr
  | Ref of expr ref * Mutex.t
  | Deref of string
  | RefAssign of string * expr
  | Seq of expr * expr

let string_of_binop = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Mod -> "Mod"
  | Pow -> "Pow"  
  | Neq -> "Neq"
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
  | Def (e1, e2) -> "Def (" ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
  | Fun (id, e) -> "Fun (" ^ id ^ " -> " ^ (string_of_expr e) ^ ")"
  | List [] -> "List ([])"
  | List (h::t) -> "List (" ^ (string_of_expr h) ^ ", " ^ (string_of_expr (List t))  ^ ")"
  | App (e1, e2) -> "App (" ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
  | CThread e -> "Thread (" ^ (string_of_expr e) ^ ")"
  | Tid t -> "Thread ID: " ^ string_of_int (Thread.id t)
  | Kill e -> "Kill (" ^ (string_of_expr e) ^ ")"
  | Print e -> "Print (" ^ (string_of_expr e) ^ ")"
  | Lock e -> "Lock (" ^ (string_of_expr e) ^ ")"
  | Unlock e -> "Unlock (" ^ (string_of_expr e) ^ ")"
  | Join e -> "Join (" ^ (string_of_expr e) ^ ")"
  | Joinall -> "Joinall"
  | None -> "None"
  | CreateRef e -> "CreateRef (" ^ (string_of_expr e) ^ ")"
  | Ref (e, _) -> "Ref (" ^ (string_of_expr !e) ^ ")"
  | Deref e -> "Deref (" ^ e ^ ")"
  | RefAssign (s, e) -> "RefAssign (s, " ^ (string_of_expr e) ^ ")"
  | Seq (e1, e2) -> "Seq(" ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")" 

(** [binop] is the type of binary operators. *)
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
  | And
  | Or
  | Cons
  | Proj
  | RefAssign

(** [unop] is the type of unary operators. *)

type unop =
  | Not
  | Print
  | Lock
  | Unlock
  | Lockall
  | Unlockall
  | Join
  | Joinall
  | CThread
  | CreateRef
  | Deref
  | Head
  | Tail

(** [expr] is the type of expressions. *)
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
  | Tid of int
  | None
  | Ref of expr ref * Mutex.t
  | Seq of expr * expr
  | Thunk of (expr -> expr)

(** [string_of_binop bop] is the string representation of binary operator [bop]. *)
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
  | And -> "And"
  | Or -> "Or"
  | Cons -> "Cons"
  | Proj -> "Proj"
  | RefAssign -> "RefAssign"

(** [string_of_binop unop] is the string representation of unary operator [unop]. *)
let string_of_unop = function
  | Not -> "Not"
  | Print -> "Print"
  | Lock -> "Lock"
  | Unlock -> "Unlock"
  | Lockall -> "Lockall"
  | Unlockall -> "Unlockall"
  | Join -> "Join"
  | Joinall -> "Joinall"
  | CThread -> "CThread"
  | CreateRef -> "CreateRef"
  | Deref -> "Deref"
  | Head -> "Head"
  | Tail -> "Tail"

(** [string_of_expr e] is the string representation of expression [e]. *)
let rec string_of_expr = function
  | Var x -> "Var " ^ x
  | Int i -> "Int " ^ string_of_int i
  | Float f -> "Float " ^ string_of_float f
  | Bool b -> "Bool " ^ string_of_bool b
  | String s -> "String " ^ s
  | Unop (op ,e) -> "Unop (" ^ (string_of_unop op) ^ ", " ^ (string_of_expr e) ^ ")"
  | Binop (op, e1, e2) -> "Binop (" ^ (string_of_binop op) ^ ", " ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
  | If (e1, e2, e3) -> "If (" ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ", " ^ (string_of_expr e3) ^ ")"
  | Def (e1, e2) -> "Def (" ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
  | Fun (id, e) -> "Fun (" ^ id ^ " -> " ^ (string_of_expr e) ^ ")"
  | List [] -> "List ([])"
  | List (h::t) -> "List (" ^ (string_of_expr h) ^ ", " ^ (string_of_expr (List t))  ^ ")"
  | App (e1, e2) -> "App (" ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
  | Tid t -> "Thread ID: " ^ string_of_int t
  | None -> "None"
  | Ref (e, _) -> "Ref (" ^ (string_of_expr !e) ^ ")"
  | Seq (e1, e2) -> "Seq(" ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")" 
  | Thunk fn -> "Thunk()"

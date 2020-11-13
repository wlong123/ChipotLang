open Ast

exception Illegal
exception UnboundVariable
exception InvalidGuard
exception InvalidUnopType
exception InvalidBinopType
exception InvalidApp

type store = (string * expr) list

let rec add_var s x v =
  match s with
  | [] -> [(x,v)]
  | (x',v')::t -> if x' = x then (x,v)::t else (x',v')::(add_var t x v)

let rec get_var s x =
  match s with
  | [] -> raise UnboundVariable
  | (x',v')::t -> if x' = x then v' else get_var t x

let eval_binop op e1 e2 =
  match op with
  | Add -> begin
      match e1, e2 with
      | Int i1, Int i2 -> Int (i1 + i2)
      | _ -> raise InvalidBinopType
    end
  | Sub -> begin
      match e1, e2 with
      | Int i1, Int i2 -> Int (i1 - i2)
      | _ -> raise InvalidBinopType
    end
  | Mul -> begin
      match e1, e2 with
      | Int i1, Int i2 -> Int (i1 * i2)
      | _ -> raise InvalidBinopType
    end
  | Div -> begin
      match e1, e2 with
      | Int i1, Int i2 -> Int (i1 / i2)
      | _ -> raise InvalidBinopType
    end
  | Mod -> begin
      match e1, e2 with
      | Int i1, Int i2 -> Int (i1 mod i2)
      | _ -> raise InvalidBinopType
    end
  | Pow -> begin
      match e1, e2 with
      | Int i1, Int i2 -> Int (int_of_float (float_of_int i1 ** float_of_int i2))
      | _ -> raise InvalidBinopType
    end
  | Eq -> begin
      match e1, e2 with
      | Bool b1, Bool b2 -> Bool (b1 = b2)
      | Int i1, Int i2 -> Bool (i1 = i2)
      | _ -> raise InvalidBinopType
    end
  | Neq -> begin
      match e1, e2 with
      | Bool b1, Bool b2 -> Bool (b1 <> b2)
      | Int i1, Int i2 -> Bool (i1 <> i2)
      | _ -> raise InvalidBinopType
    end
  | LT -> begin
      match e1, e2 with
      | Int i1, Int i2 -> Bool (i1 < i2)
      | _ -> raise InvalidBinopType
    end
  | GT -> begin
      match e1, e2 with
      | Int i1, Int i2 -> Bool (i1 > i2)
      | _ -> raise InvalidBinopType
    end
  | LTE -> begin
      match e1, e2 with
      | Int i1, Int i2 -> Bool (i1 <= i2)
      | _ -> raise InvalidBinopType
    end
  | GTE -> begin
      match e1, e2 with
      | Int i1, Int i2 -> Bool (i1 >= i2)
      | _ -> raise InvalidBinopType
    end
  | AND -> begin
      match e1, e2 with
      | Bool b1, Bool b2 -> Bool (b1 && b2)
      | _ -> raise InvalidBinopType
    end
  | OR -> begin
      match e1, e2 with
      | Bool b1, Bool b2 -> Bool (b1 || b2)
      | _ -> raise InvalidBinopType
    end
  | CONS -> begin
      match e1, e2 with
      | e', List e -> List (e'::e)
      | _ -> raise InvalidBinopType
    end
  | PROJ -> begin
      match e1, e2 with
      | List e, Int i -> List.nth e i
      | _ -> raise InvalidBinopType
    end

let rec eval' e s = 
  match e with
  | Var x -> eval' (get_var s x) s
  | Int i -> Int i
  | Float f -> Float f
  | Bool b -> Bool b
  | String s -> String s
  | Unop (op, e) -> begin
      let e' = eval' e s in
      match op with
      | NOT -> begin
          match e' with
          | Bool b -> Bool (not b)
          | _ -> raise InvalidUnopType
        end
    end
  | Binop (op, e1, e2) -> begin
      let e1' = eval' e1 s in
      let e2' = eval' e2 s in
      eval_binop op e1' e2'
    end
  | If (e1, e2, e3) -> begin
      match eval' e1 s with
      | Bool b -> if b then eval' e2 s else eval' e3 s
      | _ -> raise InvalidGuard
    end
  | Let (e1, e2) -> begin
      match e1 with
      | Binop (Eq, Var x, e) -> eval' e2 (add_var s x (eval' e s))
      | _ -> raise InvalidGuard
    end
  | Fun (x, e) -> Fun (x, e)
  | List [] -> List []
  | List l -> List (List.map (fun e -> eval' e s) l)
  | App (e1, e2) -> begin
      match eval' e1 s with
      | Fun (x, e) -> eval' e (add_var s x (eval' e2 s))
      | _ -> raise InvalidApp
    end

let eval e = eval' e []
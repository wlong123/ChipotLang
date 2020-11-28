open Ast

exception Illegal
exception UnboundVariable
exception InvalidGuard
exception InvalidUnopType
exception InvalidBinopType
exception InvalidApp
exception UnboundThread
exception InvalidDereference
exception InvalidRefAssignment

type store = (string * expr) list

let threads = ref [Thread.self ()]

let rec add_var (s:store) (x:string) (v:expr) =
  match s with
  | [] -> [(x,v)]
  | (x',v')::t -> if x' = x then (x,v)::t else (x',v')::(add_var t x v)

let rec get_var s x =
  match s with
  | [] -> raise UnboundVariable
  | (x',v')::t -> if x' = x then v' else get_var t x

let rec expr_to_string e =
  match e with
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | String s -> s
  | Fun _ -> "Function"
  | List l -> "[" ^ (List.fold_left (fun x e -> x ^ (expr_to_string e) ^ ", ") "" l) ^ "]"
  | Tid t -> "Thread " ^ (string_of_int (Thread.id t))
  | _ -> string_of_expr e

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
  let tid = Thread.self () in
  (* print_endline ("Executing Thread: " ^ (string_of_int (Thread.id tid)) ^ " " ^ (string_of_expr e)); *)
  match e with
  | Var x -> eval' (get_var s x) s
  | Int i -> Int i, s
  | Float f -> Float f, s
  | Bool b -> Bool b, s
  | String s' -> String s', s
  | Unop (op, e) -> begin
      let e' = eval' e s in
      match op with
      | NOT -> begin
          match e' with
          | Bool b, s -> Bool (not b), s
          | _ -> raise InvalidUnopType
        end
    end
  | Binop (op, e1, e2) -> begin
      let e1' = fst (eval' e1 s) in
      let e2' = fst (eval' e2 s) in
      eval_binop op e1' e2', s
    end
  | If (e1, e2, e3) -> begin
      match eval' e1 s with
      | Bool b, s -> if b then eval' e2 s else eval' e3 s
      | _ -> raise InvalidGuard
    end
  | Def (e1, e2) -> begin
      match e1 with
      | Binop (Eq, Var x, e) -> begin 
        let (e', s') = eval' e s in
        eval' e2 (add_var s' x e')
      end
      | _ -> raise InvalidGuard
    end
  | CreateRef e -> begin 
      let (e', s') = eval' e s in
      Ref (ref e'), s'
    end
  | Ref e -> Ref e, s
  | Deref e -> begin
      match eval' e s with
      | Ref r, s -> !r, s
      | _ -> raise InvalidDereference
    end
  | RefAssign (x, e) -> begin
      match get_var s x with
      | Ref r -> begin
        let (e', s') = eval' e s in
         r := e'; None, s'
      end
      | _ -> raise InvalidRefAssignment
    end
  | Fun (x, e) -> Fun (x, e), s
  | List [] -> List [], s
  | List l -> (List (List.map (fun e -> fst (eval' e s)) l)), s
  | App (e1, e2) -> begin
      match eval' e1 s with
      | Fun (x, e), s -> begin 
        let (e2', s') = eval' e2 s in
        eval' e (add_var s' x e2') 
      end
      | _ -> raise InvalidApp
    end
  | CThread e -> begin
      let t' = Thread.create (fun () -> eval' e s) () in
      threads := t' :: !threads;
      Thread.yield ();
      None, s
    end
  | Tid t -> Tid t, s
  | Kill e -> begin
      match eval' e s with
      | Tid t, s -> Thread.join t; None, s
      | _ -> raise UnboundThread
    end
  | Print e -> fst (eval' e s) |> expr_to_string |> print_endline; None, s
  | Join e -> begin
      match eval' e s with
      | Tid t, s -> Thread.join t; None, s
      | _ -> failwith "Join requires thread ID"
    end
  | Joinall -> List.iter (fun t -> if Thread.id t <> Thread.id tid then Thread.join t else ()) !threads; None, s
  | None -> None, s
  | Seq (e1, e2) -> begin
    let (e1', s') = eval' e1 s in
    eval' e2 s'
  end
  | Lock e | Unlock e -> failwith "TODO"

let eval e = fst (eval' e [])

open Ast

exception Illegal
exception UnboundVariable of string
exception InvalidGuard
exception InvalidUnopType
exception InvalidBinopType
exception InvalidApp
exception UnboundThread
exception InvalidDereference
exception InvalidRefAssignment
exception InvalidLock

type store = (string * expr) list

(* let threads = ref [] *)
(* let max_tid = ref 0 *)

let rec add_var (s:store) (x:string) (v:expr) =
  match s with
  | [] -> [(x,v)]
  | (x',v')::t -> if x' = x then (x,v)::t else (x',v')::(add_var t x v)

let rec get_var s x =
  match s with
  | [] -> raise (UnboundVariable x)
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

let fresh_id : (unit -> string) =
  let n = ref 0 in
  fun () ->
    let x = "$tmp" ^ string_of_int !n in
    n := !n + 1; x

let rec cps (e:expr) (s:store) : expr =
  match e with
  | Var x -> begin
      let k = fresh_id () in
      Fun (k, App (Var k, get_var s x))
    end
  | Int i -> begin
      let k = fresh_id () in
      Fun (k, App (Var k, Int i))
    end
  | Float f -> begin
      let k = fresh_id () in
      Fun (k, App (Var k, Float f))
    end
  | Bool b -> begin
      let k = fresh_id () in
      Fun (k, App (Var k, Bool b))
    end
  | String s -> begin
      let k = fresh_id () in
      Fun (k, App (Var k, String s))
    end
  | Unop (op, e) -> begin
      let k = fresh_id () in
      let k' = fresh_id () in
      Fun (k, App ((cps e s), Fun(k', App (Var k, (Unop (op, Var k'))))))
    end
  | Binop (op, e1, e2) -> begin
      let k = fresh_id () in
      let n = fresh_id () in
      let m = fresh_id () in
      let n_op_m = Binop (op, Var n, Var m) in
      Fun (k, App (cps e1 s, (Fun (n, (App (cps e2 s, (Fun (m, App (Var k, n_op_m)))))))))
    end
  | If (e1, e2, e3) -> begin
      let k = fresh_id () in
      let b = fresh_id () in
      Fun (k, App (cps e1 s, Fun (b, If (Var b, App (cps e2 s, Var k), App (cps e3 s, Var k)))))
    end
  | Def (e1, e2) -> begin
      let k = fresh_id () in
      let k' = fresh_id () in
      match e1 with
      | Binop (Eq, Var x, e) -> begin 
          Fun (k, App (cps e s, Fun (k', App (cps e2 (add_var s x (Var k')), Var k))))
        end
      | _ -> raise InvalidGuard
      (* match e1 with
         | Binop (Eq, Var x, e) -> cps (App (Fun (x, e2), e))
         | _ -> raise InvalidGuard *)
    end
  | Fun (x, e) -> begin
      let k = fresh_id () in
      let v = fresh_id () in
      let k' = fresh_id () in
      Fun (k, App (Var k, Fun (v, Fun (k', App (cps e (add_var s x (Var v)), Var k')))))
      (* let k = fresh_id () in
         let k' = fresh_id () in
         Fun (k, App (Var k, Fun (x, Fun (k', App (cps e s, Var k'))))) *)
    end
  | App (e1, e2) -> begin
      let k = fresh_id () in
      let f = fresh_id () in
      let v = fresh_id () in
      let fvk = App (App (Var f, Var v), Var k) in
      Fun (k, App (cps e1 s, Fun (f, (App (cps e2 s, Fun (v, fvk))))))
    end
  | _ -> failwith "unimplemented"

let rec eval_unop op e s =
  let (v, s') = eval' e s in
  match op, v with
  | Not, Bool b -> Bool (not b), s'
  | Not, _ -> failwith "Cannot negate non-boolean value"
  | Print, _ -> expr_to_string v |> print_endline; (None, s')
  | Join, Tid t -> Thread.join t; (None, s')
  | Join, _ -> failwith "Join requires thread ID"
  (* | Joinall, _ -> List.iter (fun t -> if Thread.id t <> Thread.id tid then Thread.join t else ()) !threads; (None, s') *)
  | Joinall, _ -> failwith "failure"
  | Lock, Ref (ptr, l) -> Mutex.lock l; Ref (ptr, l), s'
  | Lockall, List lst -> begin
      let lock_elem = function
        | Ref (ptr, l) -> Mutex.lock l; Ref (ptr, l)
        | _ -> raise InvalidLock in
      (List (List.map lock_elem lst), s')
    end
  | Lock, _ | Lockall, _ -> failwith "Cannot lock non-reference value"
  | Unlock, Ref (ptr, l) -> Mutex.unlock l; Ref (ptr, l), s'
  | Unlockall, List lst -> 
    let unlock_elem = function
      | Ref (ptr, l) -> Mutex.unlock l; Ref (ptr, l)
      | _ -> raise InvalidLock in
    (List (List.map unlock_elem lst), s')
  | Unlock, _ | Unlockall, _ -> failwith "Cannot unlock non-reference value"
  | CThread, _ -> failwith "TODO"

and eval_binop op e1 e2 s =
  let (v1, s') = eval' e1 s in
  let (v2, s'') = eval' e2 s' in
  let result = match op, v1, v2 with
    | Add, Int i1, Int i2 -> Int (i1 + i2)
    | Sub, Int i1, Int i2 -> Int (i1 - i2)
    | Mul, Int i1, Int i2 -> Int (i1 * i2)
    | Div, Int i1, Int i2 -> Int (i1 / i2)
    | Mod, Int i1, Int i2 -> Int (i1 mod i2)
    | Pow, Int i1, Int i2 -> Int (int_of_float (float_of_int i1 ** float_of_int i2))
    | Eq, Bool b1, Bool b2 -> Bool (b1 = b2)
    | Eq, Int i1, Int i2 -> Bool (i1 = i2)
    | Neq, Bool b1, Bool b2 -> Bool (b1 <> b2)
    | Neq, Int i1, Int i2 -> Bool (i1 <> i2)
    | LT, Int i1, Int i2 -> Bool (i1 < i2)
    | GT, Int i1, Int i2 -> Bool (i1 > i2)
    | LTE, Int i1, Int i2 -> Bool (i1 <= i2)
    | GTE, Int i1, Int i2 -> Bool (i1 >= i2)
    | And, Bool b1, Bool b2 -> Bool (b1 && b2)
    | Or, Bool b1, Bool b2 -> Bool (b1 || b2)
    | Cons, e', List e -> List (e'::e)
    | Proj, List e, Int i -> List.nth e i
    | _ -> raise InvalidBinopType
  in
  (result, s'')

and eval' e s = 
  print_endline (string_of_expr e);
  let tid = Thread.self () in
  match e with
  | Var x -> eval' (get_var s x) s
  | Int i -> Int i, s
  | Float f -> Float f, s
  | Bool b -> Bool b, s
  | String s' -> String s', s
  | Unop (op, e) -> eval_unop op e s
  | Binop (op, e1, e2) -> eval_binop op e1 e2 s
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
      Ref (ref e', Mutex.create ()), s'
    end
  | Ref (e, l) -> Ref (e, l), s
  | Deref x -> begin
      match get_var s x with
      | Ref (r, _) -> !r, s
      | _ -> raise InvalidDereference
    end
  | RefAssign (x, e) -> begin
      match get_var s x with
      | Ref (r, _) -> begin
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
      | _ -> print_endline ("ERROR: " ^ string_of_expr e); raise InvalidApp
    end
  (* | CThread e -> begin
      let t' = Thread.create (fun () -> eval' e s) () in
      (* threads := t' :: !threads;
         Thread.yield (); *)
      Tid t', s
     end *)
  | Tid t -> Tid t, s
  | None -> None, s
  | Seq (e1, e2) -> begin
      let (e1', s') = eval' e1 s in
      eval' e2 s'
    end

let pick_run_length unit = 10

let rec add_to_list l x i =
  match l with
  | [] -> []
  | h :: t -> if i = 0 then x :: t else h :: (add_to_list t x (i-1))

let is_val e =
  match e with 
  | Int _
  | Float _
  | Bool _
  | String _
  | Ref _
  | Fun _
  | List _ 
  | Tid _
  | None -> true
  | _ -> false

let eval_cps e_cps e' =
  match e_cps with
  | Fun (x, e) as f -> eval' (App (f, e')) []
  | _ -> failwith "bad"

let eval e = 
  let k = fresh_id () in
  fst (eval_cps (cps e []) (Fun (k, (Var k))))

(* threads := [(!max_tid), (eval' e [])];
   let thread_to_run = ref 0 in
   let out = ref None in
   while List.mem_assoc 0 (!threads) do
   thread_to_run := if !thread_to_run >= List.length (!threads) then 0 else !thread_to_run;
   let to_eval = List.assoc (!thread_to_run) (!threads) in
   let e', s' = to_eval (pick_run_length ()) () in
   if is_val e' then (
    if (!thread_to_run) = 0 then out := e' else (); 
    threads := List.remove_assoc (!thread_to_run) (!threads)
   )
   else threads := add_to_list (!threads) (!thread_to_run, (eval' e' s')) (!thread_to_run)
   done;
   !out *)


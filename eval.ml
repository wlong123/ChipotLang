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

let threads = ref []
let max_tid = ref 0

let rec thread_e e x e' =
  match e with
  | Var x' -> if x' = x then e' else Var x'
  | Int i -> Int i
  | Float f -> Float f
  | Bool b -> Bool b
  | String s -> String s
  | Unop (op, e) -> Unop (op, thread_e e x e')
  | Binop (op, e1, e2) -> Binop (op, thread_e e1 x e', thread_e e2 x e')
  | If (e1, e2, e3) -> If (thread_e e1 x e', thread_e e2 x e', thread_e e3 x e')
  | Def (e1, e2) -> Def (thread_e e1 x e', thread_e e2 x e')
  | Fun (x', e) -> Fun (x', thread_e e x e')
  | List l -> List (List.map (fun e -> thread_e e x e') l)
  | App (e1, e2) -> App (thread_e e1 x e', thread_e e2 x e')
  | Tid t -> Tid t
  | None -> None
  | Ref (r, t) -> Ref (ref (thread_e !r x e'), t)
  | Seq (e1, e2) -> Seq (thread_e e1 x e', thread_e e2 x e')
  | Thunk fn -> Thunk fn

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
  | Tid t -> "Thread " ^ (string_of_int t)
  | _ -> string_of_expr e (* TODO *)

let fresh_id : (unit -> string) =
  let n = ref 0 in
  fun () ->
    let x = "$tmp" ^ string_of_int !n in
    n := !n + 1; x

let z_comb () = 
  let f = fresh_id () in
  let x = fresh_id () in
  let y = fresh_id () in
  let p = Fun (x, App (Var f, Fun (y, (App (App (Var x, Var x), Var y))))) in
  Fun (f, App (p, p))

let rec replace_variable x e exp =
  match e with
  | Var x' -> if x' = x then exp else Var x'
  | Int _ | Float _ | Bool _ | String _ | None | Tid _ | Thunk _ -> e
  | Ref (ptr, lock) -> ptr := replace_variable x !ptr exp; e
  | Unop (op, e') -> Unop (op, replace_variable x e' exp)
  | Binop (op, e1, e2) -> Binop (op, (replace_variable x e1 exp), (replace_variable x e2 exp))
  | If (e1, e2, e3) -> If ((replace_variable x e1 exp), (replace_variable x e2 exp), (replace_variable x e3 exp))
  | Def (Binop (Eq, Var x', e1), e2) ->
    (* if x' = x then Def (Binop(Eq, Var x', e1), e2)
       else  *)
    Def (Binop (Eq, Var x', (replace_variable x e1 exp)), (replace_variable x e2 exp))
  | Def _ -> failwith "Impossible"
  | Fun (x', e') -> 
    (* if x' = x then Fun (x', e')
       else  *)
    Fun (x', (replace_variable x e' exp))
  | List lst -> List (List.map (fun elem -> replace_variable x elem exp) lst)
  | App (e1, e2) -> App ((replace_variable x e1 exp), (replace_variable x e2 exp))
  | Seq (e1, e2) -> Seq ((replace_variable x e1 exp), (replace_variable x e2 exp))

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
  | None -> begin
      let k = fresh_id () in
      Fun (k, App (Var k, None))
    end
  | Ref (ptr, l) -> begin
      let k = fresh_id () in
      Fun (k, App (Var k, Ref (ptr, l)))
    end
  | Tid t -> begin
      let k = fresh_id () in
      Fun (k, App (Var k, Tid t))
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
      let thunk (bexp:expr) (s:store) : expr =
        let get_bool = function
          | Bool bl -> bl
          | Var x -> begin
              match get_var s x with
              | Bool bl' -> bl'
              | _ -> failwith ("Not a bool 1")
            end
          | _ -> failwith ("Not a bool 2") in
        if get_bool bexp then App (cps e2 s, Var k)
        else App (cps e3 s, Var k)
      in
      Fun (k, App (cps e1 s, Fun (b, App (Thunk (fun x -> thunk x s), Var b))))
    end
  | Def (e1, e2) -> begin
      match e1 with
      | Binop (Eq, Var x, Fun (x_bar, e_bar)) -> begin
          let v = fresh_id () in
          let k = fresh_id () in
          let f = fresh_id () in
          let e' = replace_variable x e_bar (App (Var f, Var f)) in
          let fn' = Fun (f, Fun (x_bar, e')) in
          let fn = App (fn', fn') in
          Fun (k, App (cps fn s, Fun (v, App (cps e2 (add_var s x (Var v)), Var k))))
        end
      | Binop (Eq, Var x, e) -> cps (App (Fun (x, e2), e)) s
      | _ -> raise InvalidGuard
    end
  | Fun (x, e) -> begin
      let k = fresh_id () in
      let v = fresh_id () in
      let k' = fresh_id () in
      Fun (k, App (Var k, Fun (v, Fun (k', App (cps e (add_var s x (Var v)), Var k')))))
    end
  | App (e1, e2) -> begin
      let k = fresh_id () in
      let f = fresh_id () in
      let v = fresh_id () in
      let fvk = App (App (Var f, Var v), Var k) in
      Fun (k, App (cps e1 s, Fun (f, (App (cps e2 s, Fun (v, fvk))))))
    end
  | Seq (e1, e2) -> begin
      let k = fresh_id () in
      let k' = fresh_id () in
      let k'' = fresh_id () in
      Fun (k, App (cps e1 s, Fun (k', (App (cps e2 s, Fun (k'', App (Var k, Seq (Var k', Var k''))))))))
    end
  | List lst -> begin
      let k = fresh_id () in
      let x1 = fresh_id () in
      let x2 = fresh_id () in
      match lst with
      | [] -> Fun (k, App (Var k, List []))
      | h::t ->
        Fun (k, App (cps h s, Fun (x1, App (cps (List t) s, Fun (x2, App (Var k, Binop (Cons, Var x1, Var x2)))))))
    end
  | Thunk _ -> failwith "Impossible"

let eval_unop op e s n =
  let v = match e with
    | Var x -> get_var s x 
    | _ -> failwith "Variable not in store" 
  in
  match op, v with
  | Not, Bool b -> Bool (not b), s, (n - 1)
  | Print, _ -> expr_to_string v |> print_endline; None, s, (n - 1)
  | Join, Tid t -> begin
      match List.assoc_opt t !threads with
      | Some _ -> Unop (Join, e), s, 0
      | None -> None, s, (n - 1)
    end
  | Joinall, _ -> if List.length !threads > 1 then Unop (Joinall, e), s, 0 else None, s, (n - 1)
  | Lock, Ref (ptr, l) -> if Mutex.try_lock l then None, s, (n - 1) else Unop (Lock, e), s, 0
  | Lockall, List lst -> begin
      let lock_elem = function
        | Ref (ptr, l) -> Mutex.lock l; None
        | _ -> raise InvalidLock in

      let is_locked = function
        | Ref (ptr, l) -> if Mutex.try_lock l then (Mutex.unlock l; true) else false
        | _ -> raise InvalidLock in

      let all_unlocked = List.fold_left (fun b e -> b && (is_locked e)) true lst in
      if all_unlocked then (ignore (List.map lock_elem lst); None, s, (n - 1)) else Unop (Lockall, e), s, 0
    end
  | Unlock, Ref (ptr, l) -> Mutex.unlock l; None, s, (n - 1)
  | Unlockall, List lst -> 
    let unlock_elem = function
      | Ref (ptr, l) -> Mutex.unlock l
      | _ -> raise InvalidLock in
    ignore (List.map unlock_elem lst); None, s, (n - 1)
  | CThread, _ -> begin
      max_tid := !max_tid + 1;
      threads := (!max_tid, (v,s))::(!threads);
      Tid !max_tid, s, 0 (* Set to 0 to force re-scheduling *)
    end
  | CreateRef, _ -> Ref (ref v, Mutex.create ()), s, (n - 1)
  | Deref, Ref (r, _)  -> !r, s, (n - 1)
  | _ -> raise InvalidUnopType

let eval_binop op e1 e2 s n =
  let v1, v2 = match e1, e2 with
    | Var x1, Var x2 -> get_var s x1, get_var s x2  
    | _ -> failwith "Variable not in store"
  in
  match op, v1, v2 with
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
  | RefAssign, Ref (r, _), _ -> r := v2; None
  | _ -> print_endline (string_of_expr v1); print_endline (string_of_expr v2); raise InvalidBinopType

let eval_lst lst s =
  let get_elem = function
    | Var x -> get_var s x
    | e -> print_endline (string_of_expr e);  failwith "Invalid list" in
  List (List.map get_elem lst)

let rec eval_if e1 e2 e3 s n =
  let v1 = match e1 with
    | Var v1 -> get_var s v1
    | _ -> failwith "Variable not in store"
  in
  match v1 with
  | Bool b -> if b then eval' e2 s (n-1) else eval' e3 s (n-1)
  | _ -> raise InvalidGuard

and eval_app e1 e2 s n =
  match eval' e1 s (n-1) with
  | Fun (x, e), s, n -> begin
      if n = 0 then App (Fun (x,e), e2), s, n else
        let (e2', s', n) = eval' e2 s (n-1) in
        if n = 0 then App (Fun (x,e), e2'), s', n else
          (* let x' = fresh_id () in
             let e' = replace_variable x e (Var x') in *)
          eval' e (add_var s' x e2') (n-1)
    end
  | e, s, n -> begin
      if n = 0 then App (e, e2), s, n else raise InvalidApp
    end

and eval_seq e1 e2 s n =
  let (e1', s', n) = eval' e1 s (n-1) in
  if n = 0 then Seq (e1', e2), s', n else
    eval' e2 s' n

and eval' e s n = 
  (* print_endline (string_of_expr e); *)
  if n = 0 then e, s, n else
    match e with
    | Var x -> get_var s x, s, (n - 1)
    | Int i -> Int i, s, (n - 1)
    | Float f -> Float f, s, (n - 1)
    | Bool b -> Bool b, s, (n - 1)
    | String s' -> String s', s, (n - 1)
    | Unop (op, e) -> eval_unop op e s n
    | Binop (op, e1, e2) -> eval_binop op e1 e2 s n, s, (n - 1)
    | If (e1, e2, e3) -> eval_if e1 e2 e3 s n
    | Def (e1, e2) -> failwith "Handled by CPS translation"
    | Fun (x, e) -> Fun (x, e), s, (n - 1)
    | List [] -> List [], s, (n - 1)
    | App (Thunk e1, e2) ->
      let (v, s', c) = eval' e2 s (n-1) in e1 v, s', (c - 1)
    | App (e1, e2) -> eval_app e1 e2 s n
    | None -> None, s, (n - 1)
    | Tid t -> Tid t, s, (n - 1)
    | Ref (e, l) -> Ref (e, l), s, (n - 1)
    | Seq (e1, e2) -> eval_seq e1 e2 s n
    | List lst -> eval_lst lst s, s, n
    | Thunk fn -> Thunk fn, s, (n - 1)

(* Must be greater than or equal to 5 in order to ensure the thunk is applied *)
let pick_run_length unit = 7

let rec add_to_list l x i =
  match l with
  | [] -> []
  | (x', s) :: t -> if x' = i then x :: t else (x', s) :: (add_to_list t x i)

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

let eval_cps e_cps e' s n =
  match e_cps with
  | Fun (x, e) as f -> eval' (App (f, e')) s n
  | e -> eval' e s n

let pick_next_thread l =
  let index_to_run = Random.int (List.length l) in
  List.nth l index_to_run

let eval e = 
  let main_e = cps e [] in
  threads := (0, (main_e, []))::!threads;
  let out = ref None in
  while List.mem_assoc 0 (!threads) do
    let (thread_to_run, (e, s)) = pick_next_thread !threads in
    (* print_endline ("About to start thread: " ^ (string_of_int thread_to_run)); *)
    let k = fresh_id () in
    let e', s', _ = eval_cps e (Fun (k, (Var k))) s (pick_run_length ()) in
    (* print_endline ("Finished thread: " ^ (string_of_int thread_to_run)); *)
    if is_val e' then (
      if (thread_to_run) = 0 then out := e' else (); 
      threads := List.remove_assoc (thread_to_run) (!threads)
    )
    else threads := add_to_list (!threads) (thread_to_run, (e', s')) (thread_to_run)
  done;
  !out

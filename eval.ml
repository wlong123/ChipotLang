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
  let x' = fresh_id () in
  let y' = fresh_id () in
  let p1 = Fun (x, App (Var f, Fun (y, (App (App (Var x, Var x), Var y))))) in
  let p2 = Fun (x', App (Var f, Fun (y', (App (App (Var x', Var x'), Var y'))))) in
  Fun (f, App (p1, p2))


let eval_unop op e s =
  let v = match e with
    | Var x -> get_var s x 
    | _ -> failwith ("Variable not in store: " ^ (string_of_expr e))
  in
  match op, v with
  | Not, Bool b -> Bool (not b)
  | Print, _ -> expr_to_string v |> print_endline; None
  | Join, Tid t -> Thread.join t; None
  (* | Joinall, _ -> List.iter (fun t -> if Thread.id t <> Thread.id tid then Thread.join t else ()) !threads; None *)
  | Joinall, _ -> failwith "failure"
  | Lock, Ref (ptr, l) -> Mutex.lock l; Ref (ptr, l)
  | Lockall, List lst -> begin
      let lock_elem = function
        | Ref (ptr, l) -> Mutex.lock l; Ref (ptr, l)
        | _ -> raise InvalidLock in
      List (List.map lock_elem lst)
    end
  | Unlock, Ref (ptr, l) -> Mutex.unlock l; Ref (ptr, l)
  | Unlockall, List lst -> 
    let unlock_elem = function
      | Ref (ptr, l) -> Mutex.unlock l; Ref (ptr, l)
      | _ -> raise InvalidLock in
    List (List.map unlock_elem lst)
  | CThread, _ -> failwith "TODO"
  | CreateRef, _ -> Ref (ref v, Mutex.create ())
  | Deref, Ref (r, _)  -> !r
  | _ -> raise InvalidUnopType

let eval_binop op e1 e2 s =
  let v1, v2 = match e1, e2 with
    | Var x1, Var x2 -> get_var s x1, get_var s x2  
    | _ -> failwith ("Variable not in store: " ^ (string_of_expr e1))
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
    | _ -> failwith "Invalid list" in
  List (List.map get_elem lst), s


let rec eval_if e1 e2 e3 s =
  let v1, s' = (* TODO *)
    (* eval' e1 s *)
    match e1 with
    | Var v1 -> get_var s v1, s
    | _ -> failwith ("Variable not in store: " ^ (string_of_expr e1))
  in
  match v1 with
  | Bool b -> if b then eval' e2 s' else eval' e3 s'
  | _ -> raise InvalidGuard

and eval_app e1 e2 s =
  match eval' e1 s with
  | Fun (x, e), s -> let (e2', s') = eval' e2 s in eval' e (add_var s' x e2') 
  | _ -> raise InvalidApp

and eval_seq e1 e2 s =
  let (e1', s') = eval' e1 s in
  eval' e2 s'

and eval' e s = 
  (* print_endline (string_of_expr e); *)
  let tid = Thread.self () in
  match e with
  | Var x -> get_var s x, s
  | Int i -> Int i, s
  | Float f -> Float f, s
  | Bool b -> Bool b, s
  | String s' -> String s', s
  | Unop (op, e) -> eval_unop op e s, s
  | Binop (op, e1, e2) -> eval_binop op e1 e2 s, s
  | If (e1, e2, e3) -> eval_if e1 e2 e3 s
  | Def (e1, e2) -> failwith "Handled by CPS translation"
  | Fun (x, e) -> Fun (x, e), s
  | List [] -> List [], s
  | App (e1, e2) -> eval_app e1 e2 s
  | None -> None, s
  | Tid t -> Tid t, s
  | Ref (e, l) -> Ref (e, l), s
  | Seq (e1, e2) -> eval_seq e1 e2 s
  | List lst -> eval_lst lst s

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
      Fun (k, App (cps e1 s, Fun (b, If (Var b, App (cps e2 s, Var k), App (cps e3 s, Var k)))))
    end
  | Def (e1, e2) -> begin
      let rec insert_combinator x e f =
        print_endline (string_of_expr e);
        match e with
        | Var x' -> if x' = x then (print_endline ("REPLACING [" ^ (string_of_expr e) ^ "] WITH [" ^ (string_of_expr f) ^ "]"); f) else Var x'
        | Int _ | Float _ | Bool _ | String _ | None | Tid _ | Ref _ -> e
        | Unop (op, e') -> Unop (op, insert_combinator x e' f)
        | Binop (op, e1, e2) -> Binop (op, (insert_combinator x e1 f), (insert_combinator x e2 f))
        | If (e1, e2, e3) -> If ((insert_combinator x e1 f), (insert_combinator x e2 f), (insert_combinator x e3 f))
        | Def (Binop (Eq, String x', e1), e2) ->
          (* if x' = x then Def (Binop(Eq, String x', e1), e2) else *)
          Def (Binop(Eq, String x', (insert_combinator x e1 f)), (insert_combinator x e2 f))
        | Fun (x', e') -> 
          (* if x' = x then Fun (x', e') else *)
          Fun (x', (insert_combinator x e' f))
        | List _ -> failwith "TODO"
        | App (e1, e2) -> App ((insert_combinator x e1 f), (insert_combinator x e2 f))
        | Seq (e1, e2) -> Seq ((insert_combinator x e1 f), (insert_combinator x e2 f))
        | _ -> failwith "Error"
      in
      let k = fresh_id () in
      let p = fresh_id () in
      match e1 with
      | Binop (Eq, Var x, Fun (x_bar, e_bar)) -> begin
          let e = Fun (x_bar, e_bar) in
          let f = fresh_id () in
          let e' = insert_combinator x e (Var f) in
          let g = Fun (f, e') in
          let z = z_comb () in
          let zg = App (z, g) in
          let zg' = insert_combinator f zg zg in (* Trying to substitute zg for f in the evaluated zg *)
          print_endline ("f has ID: " ^ f);
          print_endline ("z_comb is: " ^ (string_of_expr (eval' z s |> fst)));
          print_endline ("zg is: " ^ (string_of_expr (eval' zg s |> fst)));
          print_endline ("g is: " ^ (string_of_expr (eval' g s |> fst)));
          print_endline ("zg' is: " ^ (string_of_expr (eval' zg' s |> fst)));
          Fun (k, App (cps zg' (add_var s f zg'), Fun (p, App (cps e2 (add_var s x (Var p)), (Var k)))))
        end
      | Binop (Eq, Var x, e) ->
        Fun (k, App (cps e s, Fun (p, App (cps e2 (add_var s x (Var p)), (Var k)))))
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

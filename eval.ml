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

let fresh_id : unit -> string =
  let n = ref 0 in
  fun () ->
  let x = "tmp" ^ string_of_int !n in
  n := !n + 1;
  x

let rec cps (e:expr) (s:store) : (expr -> expr) -> expr =
  match e with 
  | Var x -> fun k -> k (Var x)
  | Int i -> fun k -> k (Int i)
  | Float f -> fun k -> k (Float f)
  | Bool b -> fun k -> k (Bool b)
  | String s -> fun k -> k (String s)
  | Unop (op, e) -> fun k -> (cps e s) (fun temp -> k (Unop (op, temp)))
  | Binop (op, e1, e2) -> fun k -> (cps e1 s) (fun temp1 -> (cps e2 s) (fun temp2 ->
    let r = fresh_id () in
    Def (Binop (Eq, Var r, (Binop (op, temp1, temp2))), k (Var r))))
  | If (e1, e2, e3) -> fun k -> 
    let k' = fresh_id () in
    let k'_f = fun r -> App (Var k', r) in
    Def (Binop (Eq, Var k', (let r = fresh_id () in Fun (r, k (Var r)))),
    cps e1 s (fun v1 ->
      If (v1, cps e2 s k'_f, cps e3 s k'_f)))
  | Def (e1, e2) -> begin
    match e1 with
    | Binop (Eq, Var x, e) -> begin 
        fun k -> (cps e s) (fun k' -> Def (Binop (Eq, Var x, k'), (cps e2 s) k))
      end
    | _ -> raise InvalidGuard
  end
  | Fun (x, e) -> fun k -> 
    let k' = fresh_id () in
    k (Fun (k', (Fun (x, (cps e s) (fun r -> App (Var k', r))))))
  | App (e1, e2) -> fun k ->
    let r = fresh_id () in
    cps e1 s (fun f -> 
      (cps e2 s) (fun v -> 
        App (f, App (Fun (r, k (Var r)), v))))
  | _ -> failwith "unimplemented"

let rec eval' e s n unit = 
  (* if n = 0 then e, s else *)
  print_endline (string_of_expr e);
  let tid = Thread.self () in
  match e with
  | Var x -> eval' (get_var s x) s (n-1) ()
  | Int i -> Int i, s
  | Float f -> Float f, s
  | Bool b -> Bool b, s
  | String s' -> String s', s
  | Unop (op, e) -> begin
      let e' = eval' e s (n-1) () in
      match op with
      | NOT -> begin
          match e' with
          | Bool b, s -> Bool (not b), s
          | _ -> raise InvalidUnopType
        end
    end
  | Binop (op, e1, e2) -> begin
      let e1' = fst (eval' e1 s (n-1) ()) in
      let e2' = fst (eval' e2 s (n-1) ()) in
      eval_binop op e1' e2', s
    end
  | If (e1, e2, e3) -> begin
      match eval' e1 s (n-1) () with
      | Bool b, s -> if b then eval' e2 s (n-1) () else eval' e3 s (n-1) ()
      | _ -> raise InvalidGuard
    end
  | Def (e1, e2) -> begin
      match e1 with
      | Binop (Eq, Var x, e) -> begin 
          let (e', s') = eval' e s (n-1) () in
          eval' e2 (add_var s' x e') (n-1) ()
        end
      | _ -> raise InvalidGuard
    end
  | CreateRef e -> begin 
      let (e', s') = eval' e s (n-1) () in
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
          let (e', s') = eval' e s (n-1) () in
          r := e'; None, s'
        end
      | _ -> raise InvalidRefAssignment
    end
  | Fun (x, e) -> Fun (x, e), s
  | List [] -> List [], s
  | List l -> (List (List.map (fun e -> fst (eval' e s (n-1) ())) l)), s
  | App (e1, e2) -> begin
      match eval' e1 s (n-1) () with
      | Fun (x, e), s -> begin 
          let (e2', s') = eval' e2 s (n-1) () in
          eval' e (add_var s' x e2') (n-1) () 
        end
      | _ -> raise InvalidApp
    end
  | CThread e -> begin
      let t' = Thread.create (fun () -> eval' e s) () in
      (* threads := t' :: !threads;
      Thread.yield (); *)
      Tid t', s
    end
  | Tid t -> Tid t, s
  | Print e -> fst (eval' e s (n-1) ()) |> expr_to_string |> print_endline; None, s
  | Join e -> begin
      match eval' e s (n-1) () with
      | Tid t, s -> Thread.join t; None, s
      | _ -> failwith "Join requires thread ID"
    end
  |Joinall -> failwith "failure"
  (* | Joinall -> List.iter (fun t -> if Thread.id t <> Thread.id tid then Thread.join t else ()) !threads; None, s *)
  | None -> None, s
  | Seq (e1, e2) -> begin
      let (e1', s') = eval' e1 s (n-1) () in
      eval' e2 s' (n-1) ()
    end
  | Lock e -> begin
      let (e', s') = eval' e s (n-1) () in
      match e' with
      | Ref (e, l) -> Mutex.lock l; Ref (e, l), s'
      | _ -> raise InvalidLock
    end
  | Lockall e -> begin
      let (e', s') = eval' e s (n-1) () in
      match e' with
      | List lst ->
        (fun x -> match x with
           | Ref (e, l) -> Mutex.lock l; Ref (e, l)
           | _ -> raise InvalidLock)
        |> (fun map_fn -> let e' = List.map map_fn lst in
             (List e', s'))
      | _ -> raise InvalidLock
    end
  | Unlock e -> begin
      let (e', s') = eval' e s (n-1) () in
      match e' with
      | Ref (e, l) -> Mutex.unlock l; Ref (e, l), s'
      | _ -> raise InvalidLock
    end
  | Unlockall e -> begin
      let (e', s') = eval' e s (n-1) () in
      match e' with
      | List lst ->
        (fun x -> match x with
           | Ref (e, l) -> Mutex.unlock l; Ref (e, l)
           | _ -> raise InvalidLock)
        |> (fun map_fn -> let e' = List.map map_fn lst in
             (List e', s'))
      | _ -> raise InvalidLock
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

let eval e = 
  let c = cps e [] in
  fst (eval' (c (fun e -> e)) [] 0 ())
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
  

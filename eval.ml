open Ast

(** ===========================================================================
    EXCEPTIONS
    ==========================================================================*)

(** [UnboundVariable msg] is an exception raised when a varible is not found in
    a given store.  *)
exception UnboundVariable of string

(** [TypeError msg] is an exception raised when type error occurs.  *)
exception TypeError of string

(* exception InvalidGuard
   exception InvalidUnopType
   exception InvalidBinopType
   exception InvalidApp
   exception UnboundThread
   exception InvalidDereference
   exception InvalidRefAssignment
   exception InvalidLock *)

(** ===========================================================================
    TYPES AND INITIALIZATIONS
    ==========================================================================*)

(** [store] is the type of varible stores, which map the name of a variable
    to its value in the current context. *)
type store = (string * expr) list

(** [threads] is a global reference to a list of the current active threads at
    any point during the interpretation of a ChipotLang program. *)
let threads = ref []

(** [max_tid] is the value of the last thread ID assigned and is initially
    zero, which corresponds to the main thread. *)
let max_tid = ref 0

(** ===========================================================================
    UTILITY FUNCTIONS
    ==========================================================================*)

(** [add_var s x v] is the store [s], except [x] is now bound to [v]. If [x] was
    not in [s], then a new binding is created; otherwise, the old one is 
    updated. 
    Requires: [s] has no duplicate bindings and [v] is a value. *)
let add_var (s : store) (x : string) (v : expr) =
  let rec add_var_aux s x v acc =
    match s with
    | [] -> (x, v) :: acc |> List.rev
    | (x', v') :: t -> begin
        if x' = x then 
          (x', v) :: t |> ( @ ) (List.rev acc)
        else 
          let acc' = (x', v') :: acc in
          add_var_aux t x v acc'
      end
  in
  add_var_aux s x v []

(** [get_var s x] is the value is bound to [x] in store [s] if the binding 
    exists; otherwise, [UnboundVariable] is raised.
    Requires: [s] has no duplicate bindings. *)
let rec get_var (s : store) (x : string) : expr =
  match s with
  | [] -> raise (UnboundVariable x)
  | (x', v') :: t ->
    if x' = x then v'
    else get_var t x

(** [string_of_value v] is the string representation of value [v]
    Requires: [v] is a value. *)
let rec string_of_value (v : expr) : string =
  match v with
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | Bool b -> string_of_bool b
  | String s -> s
  | Fun _ -> "<function>"
  | List l -> string_of_list l
  | Tid t -> "Thread ID: " ^ (string_of_int t)
  | None -> "none"
  | Ref _ -> "<reference>"
  | _ -> failwith "Not a value"

(** [string_of_list l] is the string representation of the list [l]. *)
and string_of_list (l : expr list) : string =
  let rec string_of_list_aux l acc =
    match l with
    | [] -> acc
    | h :: t -> 
      let str = string_of_value h in
      let str' = if t = [] then str else str ^ ", " in
      string_of_list_aux t (acc ^ str')
  in
  "[" ^ (string_of_list_aux l "") ^ "]"

(** [fresh_id ()] is a fresh ID that can be assigned to a variable in the 
    CPS transformation of a program. *)
let fresh_id : (unit -> string) =
  let n = ref 0 in
  fun () ->
    (* '$' character ensures no name clashes with program variables *)
    let x = "$tmp" ^ string_of_int !n in
    n := !n + 1; x

(** [replace_variable x e new_exp] is the expression [e] with all instances of
    [Var x] replaced with [new_exp]. *)
let rec replace_variable (x : string) (e : expr) (new_exp : expr) : expr =
  match e with
  | Var x' -> if x' = x then new_exp else Var x'
  | Int _ | Float _ | Bool _ | String _ | None | Tid _ | Thunk _ -> e
  | Ref (ptr, lock) -> ptr := replace_variable x !ptr new_exp; e
  | Unop (op, e1) -> Unop (op, replace_variable x e1 new_exp)
  | Binop (op, e1, e2) -> Binop (op, (replace_variable x e1 new_exp), (replace_variable x e2 new_exp))
  | If (e1, e2, e3) ->
    If ((replace_variable x e1 new_exp), (replace_variable x e2 new_exp), (replace_variable x e3 new_exp))
  | Def (Binop (Eq, Var x', e1), e2) as e' ->
    if x' = x then e' (* To ensure proper scoping of variable names *)
    else Def (Binop (Eq, Var x', (replace_variable x e1 new_exp)), (replace_variable x e2 new_exp))
  | Def _ -> failwith "Impossible"
  | Fun (x', e1) as e' -> 
    if x' = x then e' (* To ensure proper scoping of variable names *)
    else Fun (x', (replace_variable x e1 new_exp))
  | List lst -> List (List.map (fun elem -> replace_variable x elem new_exp) lst)
  | App (e1, e2) -> App ((replace_variable x e1 new_exp), (replace_variable x e2 new_exp))
  | Seq (e1, e2) -> Seq ((replace_variable x e1 new_exp), (replace_variable x e2 new_exp))

(** ===========================================================================
    CPS TRANSFORMATION
    ==========================================================================*)

(** [cps e s] is the expression [e] in store [s] translated into continuation
    passing style (CPS).
    Requires: [s] has no duplicate bindings. *)
let rec cps (e : expr) (s : store) : expr =
  let k = fresh_id () in (* continuation variable *)
  match e with
  | Var x -> Fun (k, App (Var k, get_var s x))
  | Int i -> Fun (k, App (Var k, Int i))
  | Float f -> Fun (k, App (Var k, Float f))
  | Bool b -> Fun (k, App (Var k, Bool b))
  | String s -> Fun (k, App (Var k, String s))
  | None -> Fun (k, App (Var k, None))
  | Ref (ptr, l) -> Fun (k, App (Var k, Ref (ptr, l)))
  | Tid t -> Fun (k, App (Var k, Tid t))
  | Unop (op, e) -> begin
      let v = fresh_id () in (* variable for value of e *)
      let unop_expr = Unop (op, Var v) in
      Fun (k, App (cps e s, Fun (v, App (Var k, unop_expr))))
    end
  | Binop (op, e1, e2) -> begin
      let n = fresh_id () in (* variable for value of e1 *)
      let m = fresh_id () in (* variable for value of e2 *)
      let binop_expr = Binop (op, Var n, Var m) in
      Fun (k, App (cps e1 s, Fun (n, App (cps e2 s, Fun (m, App (Var k, binop_expr))))))
    end
  | If (e1, e2, e3) -> begin
      let b = fresh_id () in (* variable for value of e1 *)
      Fun (k, App (cps e1 s, Fun (b, App (Thunk (fun v1 -> cps_of_branch_expr v1 s e2 e3 k), Var b))))
    end
  | Def (e1, e2) -> cps_of_def_expr e1 e2 s k
  | Fun (x, e) -> begin
      let v = fresh_id () in 
      let k' = fresh_id () in
      let s' = add_var s x (Var v) in
      Fun (k, App (Var k, Fun (v, Fun (k', App (cps e s', Var k')))))
    end
  | App (e1, e2) -> begin
      let f = fresh_id () in (* variable for value of e1 *)
      let v = fresh_id () in (* variable for value of e2 *)
      let fvk = App (App (Var f, Var v), Var k) in
      Fun (k, App (cps e1 s, Fun (f, App (cps e2 s, Fun (v, fvk)))))
    end
  | Seq (e1, e2) -> begin
      let k' = fresh_id () in
      let k'' = fresh_id () in
      let seq_expr = Seq (Var k', Var k'') in
      Fun (k, App (cps e1 s, Fun (k', App (cps e2 s, Fun (k'', App (Var k, seq_expr))))))
    end
  | List lst -> begin
      let x1 = fresh_id () in
      let x2 = fresh_id () in
      match lst with
      | [] -> Fun (k, App (Var k, List []))
      | h::t ->
        let cons_expr = Binop (Cons, Var x1, Var x2) in
        Fun (k, App (cps h s, Fun (x1, App (cps (List t) s, Fun (x2, App (Var k, cons_expr))))))
    end
  | Thunk _ -> failwith "Impossible to write syntactically"

(** [cps_of_def_expr e1 e2 s k] is the expression [Def (e1, e2)] translated to
    CPS with store [s] and continuation [k].
    Requires: [s] has no duplicate bindings. *)
and cps_of_def_expr (e1 : expr) (e2 : expr) (s : store) (k : string): expr =
  match e1 with
  | Binop (Eq, Var x, Fun (x', e)) -> begin
      (* Case: Defining a function *)
      let v = fresh_id () in (* varible for value of function *)
      let f = fresh_id () in (* varible for recursion removal trick *)
      (* Apply the recursion removal trick in case function is recursive *)
      let e' = replace_variable x e (App (Var f, Var f)) in
      let fn' = Fun (f, Fun (x', e')) in
      let fn = App (fn', fn') in
      let s' = (add_var s x (Var v)) in
      Fun (k, App (cps fn s, Fun (v, App (cps e2 s', Var k))))
    end
  | Binop (Eq, Var x, e) -> cps (App (Fun (x, e2), e)) s
  | _ -> failwith "Impossible"

(** [cps_of_branch_expr e1 s e2 e3 k] is the CPS expression that is generated
    used the expression [e1] during runtime. If [e1] is truthy, then the 
    if branch is evaluated by translating [e2] to CPS with continuation [k];
    otherwise, the else branch if evaluated by translating [e3] in the same
    fashion.
    Requires: [s] has no duplicate bindings. *)
and cps_of_branch_expr (e1 : expr) (s : store) (e2 : expr) (e3 : expr) (k : string) : expr =
  let bool_of_expr = function
    | Bool b -> b
    | Var x -> begin
        match get_var s x with
        | Bool b -> b
        | _ -> failwith ("Not a bool")
      end
    | _ -> failwith ("Not a bool")
  in
  if bool_of_expr e1 then App (cps e2 s, Var k)
  else App (cps e3 s, Var k)

(** ===========================================================================
    CPS EVALUATION
    ==========================================================================*)

let eval_unop op e s n =
  let v = match e with
    | Var x -> get_var s x 
    | _ -> raise (UnboundVariable (string_of_expr e))
  in
  match op, v with
  | Not, Bool b -> Bool (not b), s, (n - 1)
  | Not, _ -> raise (TypeError ("Cannot negate non-boolean: " ^ (string_of_expr v)))
  | Print, _ -> string_of_value v |> print_endline; None, s, (n - 1)
  | Join, Tid t -> begin
      match List.assoc_opt t !threads with
      | Some _ -> Unop (Join, e), s, 0
      | None -> None, s, (n - 1)
    end
  | Join, _ -> raise (TypeError ("Cannot perform join on non-TID: " ^ (string_of_expr v)))
  | Joinall, _ -> begin
      if List.length !threads > 1 then Unop (Joinall, e), s, 0 (* TODO: ASK WILL *)
      else None, s, (n - 1)
    end
  | Lock, Ref (ptr, l) -> begin
      if Mutex.try_lock l then None, s, (n - 1)
      else Unop (Lock, e), s, 0
    end
  | Lock, _ -> raise (TypeError ("Cannot lock non-reference: " ^ (string_of_expr v)))
  | Lockall, List lst -> begin
      let lock_ref ref = 
        match ref with
        | Ref (ptr, l) -> Mutex.lock l; None
        | _ -> raise (TypeError ("Cannot lock non-reference: " ^ (string_of_expr ref))) in
      let is_locked ref =
        match ref with
        | Ref (ptr, l) -> if Mutex.try_lock l then (Mutex.unlock l; true) else false
        | _ -> raise (TypeError ("Cannot lock non-reference" ^ (string_of_expr ref))) in
      let all_unlocked = List.fold_left (fun acc ref -> acc && (is_locked ref)) true lst in
      if all_unlocked then (ignore (List.map lock_ref lst); None, s, (n - 1))
      else Unop (Lockall, e), s, 0
    end
  | Lockall, _ -> raise (TypeError ("Cannot perform lockall on non-list: " ^ (string_of_expr v)))
  | Unlock, Ref (ptr, l) -> Mutex.unlock l; None, s, (n - 1)
  | Unlock, _ -> raise (TypeError ("Cannot unlock non-reference: " ^ (string_of_expr v)))
  | Unlockall, List lst -> 
    let unlock_ref ref =
      match ref with
      | Ref (ptr, l) -> Mutex.unlock l
      | _ -> raise (TypeError ("Cannot unlock non-reference: " ^ (string_of_expr ref))) in
    ignore (List.map unlock_ref lst); None, s, (n - 1)
  | Unlockall, _ -> raise (TypeError ("Cannot perform unlockall on non-list" ^ (string_of_expr v)))
  | CThread, _ -> begin
      max_tid := !max_tid + 1;
      threads := (!max_tid, (v, s)) :: (!threads);
      Tid !max_tid, s, 0 (* Set to 0 to force re-scheduling *)
    end
  | CreateRef, _ -> Ref (ref v, Mutex.create ()), s, (n - 1)
  | Deref, Ref (r, _)  -> !r, s, (n - 1)
  | Deref, _ -> raise (TypeError ("Cannot dereference non-reference" ^ (string_of_expr v)))

let eval_binop op e1 e2 s n =
  let v1 = match e1 with
    | Var x1 -> get_var s x1 
    | _ -> raise (UnboundVariable (string_of_expr e1)) in
  let v2 = match e2 with
    | Var x2 -> get_var s x2 
    | _ -> raise (UnboundVariable (string_of_expr e2))
  in
  match op, v1, v2 with
  | Add, Int i1, Int i2 -> Int (i1 + i2)
  | Add, Float f1, Float f2 -> Float (f1 +. f2)
  | Add, String s1, String s2 -> String (s1 ^ s2)
  | Add, _, _ -> raise (TypeError ("Cannot add " ^ (string_of_expr v1) ^ " + " ^ (string_of_expr v2)))
  | Sub, Int i1, Int i2 -> Int (i1 - i2)
  | Sub, Float f1, Float f2 -> Float (f1 -. f2)
  | Sub, _, _ -> raise (TypeError ("Cannot subtract " ^ (string_of_expr v1) ^ " - " ^ (string_of_expr v2)))
  | Mul, Int i1, Int i2 -> Int (i1 * i2)
  | Mul, Float f1, Float f2 -> Float (f1 *. f2)
  | Mul, _, _ -> raise (TypeError ("Cannot multiply " ^ (string_of_expr v1) ^ " * " ^ (string_of_expr v2)))
  | Div, Int i1, Int i2 -> Int (i1 / i2)
  | Div, Float f1, Float f2 -> Float (f1 /. f2)
  | Div, _, _ -> raise (TypeError ("Cannot divide " ^ (string_of_expr v1) ^ " / " ^ (string_of_expr v2)))
  | Pow, Int i1, Int i2 -> Int (int_of_float (float_of_int i1 ** float_of_int i2))
  | Pow, Float f1, Float f2 -> Float (f1 ** f2)
  | Pow, _, _ -> raise (TypeError ("Cannot raise " ^ (string_of_expr v1) ^ " ^ " ^ (string_of_expr v2)))
  | Mod, Int i1, Int i2 -> Int (i1 mod i2)
  | Mod, _, _ -> raise (TypeError ("Cannot perform modulo " ^ (string_of_expr v1) ^ " mod " ^ (string_of_expr v2)))
  | Eq, _, _ -> Bool (v1 = v2)
  | Neq, _, _ -> Bool (v1 <> v2)
  | LT, _, _ -> Bool (v1 < v2)
  | GT, _, _ -> Bool (v1 > v2)
  | LTE, _, _ -> Bool (v1 <= v2)
  | GTE, _, _ -> Bool (v1 >= v2)
  | And, Bool b1, Bool b2 -> Bool (b1 && b2)
  | And, _, _ -> raise (TypeError ("Cannot and " ^ (string_of_expr v1) ^ " && " ^ (string_of_expr v2)))
  | Or, Bool b1, Bool b2 -> Bool (b1 || b2)
  | Or, _, _ -> raise (TypeError ("Cannot or " ^ (string_of_expr v1) ^ " || " ^ (string_of_expr v2)))
  | Cons, e', List e -> List (e' :: e)
  | Cons, _, _ -> raise (TypeError ("Cannot cons onto non-list: " ^ (string_of_expr v2)))
  | Proj, List e, Int i -> List.nth e i
  | Proj, List e, _ -> raise (TypeError ("Cannot index list with non-integer: " ^ (string_of_expr v2)))
  | Proj, _, _ -> raise (TypeError ("Cannot get projection from non-list: " ^ (string_of_expr v1)))
  | RefAssign, Ref (r, _), _ -> r := v2; None
  | RefAssign, _, _ -> raise (TypeError ("Cannot assign to non-reference: " ^ (string_of_expr v1)))

let eval_lst lst s =
  let get_elem = function
    | Var x -> get_var s x
    | _ -> failwith "Impossible based on CPS. eval_lst::get_elem"
  in
  List (List.map get_elem lst)

let rec eval_if e1 e2 e3 s n =
  let v1 = match e1 with
    | Var v1 -> get_var s v1
    | _ -> raise (UnboundVariable (string_of_expr e1))
  in
  match v1 with
  | Bool b -> if b then eval' e2 s (n-1) else eval' e3 s (n-1)
  | _ -> raise (TypeError ("Cannot use non-boolean value as guard for if-then-else expression"))

and eval_app e1 e2 s n =
  match eval' e1 s (n-1) with
  | Fun (x, e), s, n -> begin
      if n = 0 then App (Fun (x,e), e2), s, n
      else
        let (e2', s', n) = eval' e2 s (n-1) in
        if n = 0 then App (Fun (x,e), e2'), s', n
        else eval' e (add_var s' x e2') (n-1)
    end
  | e, s, n -> begin
      if n = 0 then App (e, e2), s, n
      else raise (TypeError ("Cannot use apply expression to non-function: " ^ (string_of_expr e)))
    end

and eval_seq e1 e2 s n =
  let (e1', s', n) = eval' e1 s (n-1) in
  if n = 0 then Seq (e1', e2), s', n
  else eval' e2 s' n

and eval' e s n = 
  if n = 0 then e, s, n
  else
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

(** ===========================================================================
    SCHEDULING LOGIC
    ==========================================================================*)

(** [pick_run_length ()] is the number of steps of evaluation that a thread is
    allowed to run for before having to yield.
    Requires: Value must be greater than or equal to 5 in order to ensure that
    CPS can begin translation. *)
let pick_run_length () = 10

(** [add_to_thread_list tid e s] updates thread [tid] in the global reference
    [threads] with its most recent expressoin [e] and store [s] to be computed
    when scheduled again. *)
let add_to_thread_list tid e s =
  let rec add_to_thread_list_aux l =
    match l with
    | [] -> []
    | (tid', (e', s')) :: t ->
      if tid' = tid then (tid', (e, s)) :: t
      else (tid', (e', s')) :: (add_to_thread_list_aux t)
  in
  add_to_thread_list_aux !threads

(** [is_val e] is true iff [e] is in the following set of types:
    [{Int, Float, Bool, String, Ref, Fun, List, Tid, None}]
    and is otherwise false. *)
let is_val = function 
  | Int _ | Float _ | Bool _ | String _ | Ref _ | Fun _ | List _ | Tid _ | None -> true
  | _ -> false

(** [eval_cps e_cps k s n] evaluates the CPS expression [e_cps] with the
    continuation [cont] in store [s] for [n] steps. If [n] becomes 0 during
    evaluation, this thread yields and the scheduler attempts to schedule a
    new thread. *)
let eval_cps e_cps cont s n =
  match e_cps with
  | Fun (x, e) as f -> eval' (App (f, cont)) s n
  | e -> eval' e s n

(** [pick_next_thread ()] is the next thread to run, which is randomly chosen
    from the list of currently executing threads. *)
let pick_next_thread () =
  let index_to_run = Random.int (List.length !threads) in
  List.nth !threads index_to_run

(** [eval e] is the result of evaluating expression [e], which is the abstract
    syntax tree generated from lexing and parsing a ChipotLang program. *)
let eval e = 
  let main_e = cps e [] in
  let main_tid = 0 in
  threads := (main_tid, (main_e, [])) :: !threads;
  let out = ref None in
  while List.mem_assoc main_tid (!threads) do
    let (thread_to_run, (e, s)) = pick_next_thread () in
    let k = fresh_id () in
    let cont = Fun (k, (Var k)) in
    let e', s', _ = eval_cps e cont s (pick_run_length ()) in
    if is_val e' then
      if thread_to_run = main_tid then out := e'
      else threads := List.remove_assoc thread_to_run !threads
    else threads := add_to_thread_list thread_to_run e' s'
  done; !out

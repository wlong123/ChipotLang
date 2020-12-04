# ChipotLang: An interpretted langauge for functional systems programming
### Benjamin Posnick (bmp53), William Long (wl359)

#### Vision
We are envisioning an elegant programming language that draws heavily on the ideas of functional programming and provides the necessary constructs for effective systems and concurrent programming. The idea is that the language will consist of almost entirely immutable constructs, except for variables used in shared memory accesses during concurrent operations. We aim to abstract out the idea of pointers as much as possible and provide synchronization primitives as built-in language features. We see the language as an educational tool for learning how to correctly program concurrently -- it is not intended to be a high performance systems language like C or C++, especially given that our language is interpretted and not compiled. ChipotLang has constructs that draw inspiration from OCaml's functional features, Python's untypedness, and Haskell's continuations.

#### Status
The current state of the language supports the following langauge constructs:
- Primitive data types (integers, floats, strings, booleans)
- Arithmetic (addition, subtraction, multiplication, division, modulo, exponentiation, inequalities, and, or, not)
- Lists and n-tuples
- Variables and let statements
- If-else statements
- Anonymous functions as values
- Call-by-value function application
- Recursion
- Threads (using OCaml's Thread module)
- Mutual exclusion locks
- References
- Sequences

Here are a few sample ChipotLang programs:
##### test01.guac: Shows the recursive functions are supported
```haskell
<< recursive fibonacci numbers >>
def fib = fun n =>
  if n <= 1 then
  | n
  | fib (n - 1) + fib (n - 2) in
fib (12)
========
* Parsed Input: Def (Binop (Eq, Var fib, Fun (n -> If (Binop (LTE, Var n, Int 1), Var n, Binop (Add, App (Var fib, Binop (Sub, Var n, Int 1)), App (Var fib, Binop (Sub, Var n, Int 2)))))), App (Var fib, Int 12))
* Output as AST: Int 144
```

##### test08.guac: Shows that result of atomic sequence of increments and decrements is deterministic
```haskell
def x = @ 0 in
def inc_x = fun y =>
  lock x;
  x := !x + 1;
  x := !x - 1;
  x := !x + 1;
  x := !x - 1;
  x := !x + 1;
  unlock x
in
def loop = fun n =>
  if (n = 0) then
  | none
  | thread (inc_x (none)); loop (n - 1)
in
loop (10);
joinall;
print (!x)
==========
* Parsed Input: Def (Binop (Eq, Var x, CreateRef (Int 0)), Def (Binop (Eq, Var inc_x, Fun (y -> Seq(Lock (Var x), Seq(RefAssign (s, Binop (Add, Deref (x), Int 1)), Seq(RefAssign (s, Binop (Sub, Deref (x), Int 1)), Seq(RefAssign (s, Binop (Add, Deref (x), Int 1)), Seq(RefAssign (s, Binop (Sub, Deref (x), Int 1)), Seq(RefAssign (s, Binop (Add, Deref (x), Int 1)), Unlock (Var x))))))))), Def (Binop (Eq, Var loop, Fun (n -> If (Binop (Eq, Var n, Int 0), None, Seq(Thread (App (Var inc_x, None)), App (Var loop, Binop (Sub, Var n, Int 1)))))), Seq(App (Var loop, Int 10), Seq(Joinall, Print (Deref (x)))))))
* Result of Print: 10
```

##### test09.guac: Shows that an odd number of threads swapping x and y (with locks) has a net result equivalent to swapping x and y once
```haskell
def x = @ 0 in
def y = @ 1 in
def swap = fun z =>
  lockall [x, y];
  def tmp = !x in
  x := !y;
  y := tmp;
  unlockall [x, y]
in
def loop = fun n =>
  if (n = 0) then
  | none
  | thread (swap (none)); loop (n - 1)
in
loop (3);
joinall;
print (!x);
print (!y)
==========
* Parsed Input: Def (Binop (Eq, Var x, CreateRef (Int 0)), Def (Binop (Eq, Var y, CreateRef (Int 1)), Def (Binop (Eq, Var swap, Fun (z -> Seq(Lockall (List (Var x, List (Var y, List ([])))), Def (Binop (Eq, Var tmp, Deref (x)), Seq(RefAssign (s, Deref (y)), Seq(RefAssign (s, Var tmp), Unlockall (List (Var x, List (Var y, List ([])))))))))), Def (Binop (Eq, Var loop, Fun (n -> If (Binop (Eq, Var n, Int 0), None, Seq(Thread (App (Var swap, None)), App (Var loop, Binop (Sub, Var n, Int 1)))))), Seq(App (Var loop, Int 3), Seq(Joinall, Seq(Print (Deref (x)), Print (Deref (y)))))))))
* Result of Prints: 1, 0
```

##### test11.guac: Shows that threads can be scheduled in various permutations
```haskell
def log = @ [] in
def add_id_to_log = fun n =>
  lock (log);
  log := (n :: !log);
  unlock (log)
in
def loop = fun n =>
  if (n = 0) then
  | none
  | thread (add_id_to_log (n)); loop (n - 1)
in
loop (25);
joinall;
print (!log)
============
* Parsed Input: Def (Binop (Eq, Var log, CreateRef (List ([]))), Def (Binop (Eq, Var add_id_to_log, Fun (n -> Seq(Lock (Var log), Seq(RefAssign (s, Binop (CONS, Var n, Deref (log))), Unlock (Var log))))), Def (Binop (Eq, Var loop, Fun (n -> If (Binop (Eq, Var n, Int 0), None, Seq(Thread (App (Var add_id_to_log, Var n)), App (Var loop, Binop (Sub, Var n, Int 1)))))), Seq(App (Var loop, Int 25), Seq(Joinall, Print (Deref (log)))))))
* Result of Print for Execution 1: [2, 1, 5, 8, 10, 13, 14, 17, 20, 21, 3, 4, 6, 7, 9, 12, 11, 15, 16, 18, 19, 22, 23, 24, 25]
* Result of Print for Execution 2: [11, 4, 6, 13, 14, 22, 1, 3, 2, 5, 7, 8, 9, 10, 12, 15, 16, 17, 18, 19, 20, 21, 23, 24, 25]
```

### Next Steps
Going forward, we plan to refactor the interpretter in such a way that supports continuations, which will likely involve using CPS. The goal is that continuations will allow us to implement threads from scratch (rather than calling OCaml's Thread module). We would then have control over how these threads get scheduled. Given that OCaml does not support true thread-level parallelism, we will be implementing simulated parallelism. We also plan to add support for records, while loops, and some built-in utility functions (such as map, length, etc). As a stretch goal, we will try to implement inter-thread communication.


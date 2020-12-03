# ChipotLang: An interpretted langauge for functional systems programming
### Benjamin Posnick (bmp53), William Long (wl359)

#### Vision
We are envisioning an elegant language that draws heavily on the ideas of functional programming and provides the constructs and support necessary for effective systems and concurrent programming. The idea is that the language will consist of almost entirely immutable constructs, except for variables used in shared memory accesses during concurrent operations. We aim to abstract out the idea of pointers as much as possible and provide synchronization primitives as easy-to-use, built-in language features. We will strive for the highest performance reasonably achievable by an interpretted language (recognizing that compiled langauges are generally faster). Ultimately, we seek to bridge the gap between functional and systems programming.

#### Status
Our current prototype supports the following langauge constructs
- Primitive data types (integers, floats, strings, booleans)
- Arithmetic (addition, subtraction, multiplication, division, modulo, exponentiation, inequalities, and, or, not)
- Lists and n-tuples
- Variables and let statements
- If-else statements
- Anonymous functions as values
- Call-by-value function application
- Recursion
- Threads
- Mutual exclusion locks
- References
- Sequences

Here are a few sample ChipotLang programs:
##### test01.guac
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

##### test08.guac
```haskell
<< ten threads increment and decrement a shared variable with locks >>
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
* Result of Print: 10 (shows that result is deterministic, since without locks this output is non-deterministic as reads/writes can get lost)
```

##### test09.guac
```haskell
<< Swap two refs with odd number of threads to show (un)lockall primitives >>
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
* Result of Prints: 1, 0 (shows that odd number of threads swapping x and y has a net result equivalent to swapping x and y once)
```

##### test11.guac
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
* Shows that thread scheduling is not based off the order in which the threads are spawned
```

### Next Steps
Going forward, we plan to add synchronization primitives (e.g. locks, condition variables, semaphores), functionality for creating threads and forking processes, as well as support for a few additional data types (e.g. records, dictionaries/maps). As mentioned earlier, it is likely that we will need to introduce some amount of imperative/mutability features to more easily allow for modeling of concurrency problems.

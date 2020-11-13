# ChipotLang: An interpretted langauge for functional systems programming
### Benjamin Posnick (bmp53), William Long (wl359)

#### Vision
We are envisioning an elegant language that draws heavily on the ideas of functional programming and provides the constructs and support necessary for effective systems and concurrent programming. The idea is that immutability will be embraced wherever possible, except likely in the case of shared memory accesses during concurrent operations. We aim to abstract out the idea of pointers as much as possible and provide synchronization primitives as built-in language features. We will strive for the highest performance reasonably achievable by an interpretted language (recognizing that compiled langauges are generally faster). Ultimately, we seek to bridge the gap between functional and systems programming.

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

Here are a few sample ChipotLang programs:
##### test01.guac
```haskell
<< recursive fibonacci numbers >>
def fib = fun n =>
  if n <= 1 then
  | n
  | (fib (n - 1)) + (fib (n - 2)) in
fib 12
======
* Parsed Input: Let (Binop (Eq, Var fib, Fun (n -> If (Binop (LTE, Var n, Int 1), Var n, Binop
(Add, App (Var fib, Binop (Sub, Var n, Int 1)), App (Var fib, Binop (Sub, Var n, Int 2)))))), App (Var fib, Int 12))
* Output: Int 144
```

##### test02.guac
```haskell
<< mutually recursive functions for testing parity of natural numbers >>
def is_even = fun n =>
  if n = 0 then
  | true
  | is_odd (n - 1) in
def is_odd = fun n =>
  if n = 0 then
  | false
  | is_even (n - 1) in
[is_odd 7, is_even 7]
=====================
* Parsed Input: Let (Binop (Eq, Var is_even, Fun (n -> If (Binop (Eq, Var n, Int 0), Bool true, App
(Var is_odd, Binop (Sub, Var n, Int 1))))), Let (Binop (Eq, Var is_odd, Fun (n -> If (Binop (Eq, Var n, Int 0),
Bool false, App (Var is_even, Binop (Sub, Var n, Int 1))))), List (App (Var is_odd, Int 7), List (App (Var is_even, Int 7), List ([])))))
* Output: List (Bool true, List (Bool false, List ([])))
```

### Next Steps
Going forward, we plan to add synchronization primitives (e.g. locks, condition variables, semaphores) as well as support for additional built-in data types (e.g. records, dictionaries/maps). As mentioned earlier, it is likely that we will need to introduce some amount of imperative/mutability features to more easily allow for modeling of concurrency problems.

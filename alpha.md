# ChipotLang: An interpretted langauge for functional systems programming
### Benjamin Posnick (bmp53), William Long (wl359)

#### Vision
We are envisioning an elegant language that draws heavily on the ideas of functional programming while providing constructs and support necessary for effective systems programming. The idea is that immutability will be embraced wherever possible, except in the case of shared memory accesses during concurrent operations. We aim to abstract out the idea of pointers as much as possible and provide synchronization primitives as built-in language features. We will aim for the highest performance reasonably achievable by an interpretted language (recognizing that compiled langauges are generally faster). Ultimately, we seek to bridge the gap between functional and systems programming.

#### Status
Our current prototype supports the following langauge constructs
- Primitive data types (integers, floats, strings, booleans
- Arithmetic (addition, subtraction, multiplicaiton, division, modulo, exponentiation, inequalities, and, or, not)
- Lists and n-tuples
- Variables and let statements
- If-then-else statements
- Anonymous functions as values
- Call-by-value function application
- Recursion

Here are a few sample ChipotLang programs:
##### test01.guac
```ocaml
<< recursive fibonacci numbers >>
let fib = fun n ->
  if n <= 1 then n
  else (fib (n - 1)) + (fib (n - 2)) in
fib 12

==> Parsed Input: Let (Binop (Eq, Var fib, Fun (n -> If (Binop (LTE, Var n, Int 1), Var n,
Binop (Add, App (Var fib, Binop (Sub, Var n, Int 1)),
App (Var fib, Binop (Sub, Var n, Int 2)))))), App (Var fib, Int 12))
==> Output: Int 144
```

##### test02.guac
```ocaml
<< mutually recursive functions for testing parity of natural numbers >>
let is_even = fun n ->
  if n = 0 then true
  else is_odd (n - 1) in
let is_odd = fun n ->
  if n = 0 then false
  else is_even (n - 1) in
[is_odd 7, is_even 7,]

==> Parsed Input: Let (Binop (Eq, Var is_even, Fun (n -> If (Binop (Eq, Var n, Int 0), Bool true,
App (Var is_odd, Binop (Sub, Var n, Int 1))))), Let (Binop (Eq, Var is_odd,
Fun (n -> If (Binop (Eq, Var n, Int 0), Bool false, App (Var is_even, Binop (Sub, Var n, Int 1))))),
List (App (Var is_odd, Int 7), List (App (Var is_even, Int 7), List ([])))))
==> Output: List (Bool true, List (Bool false, List ([])))
```

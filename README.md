# ChipotLang: An interpretted langauge for functional systems programming
### Benjamin Posnick (bmp53), William Long (wl359)

As of right now, there are no additional dependencies required to run ChipotLang.
We will later implement a way to interact with the language at the top-level, 
similarly to `utop`. For now, we have created 3 sample tests that can be run
as a demonstration of our progress for this first sprint.

To run the tests, run `make start`. Then, upon being prompted by the main program,
enter...
- `test00.guac` to run a program that calculates the squared length of the
hypotenuse of a right triangle when the legs are length 3 and 4, returning true
if the value is indeed 25. This program demonstrates that integers and functions
are values, functions can be applied to expressions, and if statements behave as
one would expect.
- `test01.guac` to run a program that calculates the 12th Fibonacci number, which
is 144. This program demonstrates that recursive programs can be run and that
because functions are values in ChipotLang, their bodies are not evaluated until
application to an expression. (Note that recursive functions do not require the
use of a `rec` keyword as in OCaml, for example.)
- `test02.guac` to run a program that determines whether 7 is an even or an odd
number by calling both `is_odd` and `is_even` directly, storing their results in
a list in the 0th and 1st indices, respectively. This program demonstrates
that mutual recursion is possible in ChipotLang and that lists can hold the
result of evaluating an expression in a convenient and familiar syntax.

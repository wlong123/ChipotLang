# ChipotLang

TODO:

Add to lexer/parser:
if b then e1 else e2
<!-- while b do e -->
functions
recursive functions
let x = e in
lists
pairs


Syntax:

IF-THEN-ELSE
if b then e1 else e2

FUNCTIONS
fun x1 x2 ... xn -> e

RECURSIVE FUNCTIONS
rec fun x1 x2 ... xn -> e

LET
let x = e in

LISTS
[e1,e2,...,en]
[] : empty list
h :: t : cons
l.i -> ith element of list

PAIRS
(e1,e2, ... en)
(e1,e2, ... en).i -> ith element of pair

Implement interpreter
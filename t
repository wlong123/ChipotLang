def fact = fun n => if n = 0 then
| 1
| fact (n-1) * 0
in
fact (4)
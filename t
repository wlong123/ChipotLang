def fact = fun n => if n = 0 then | 1 | n * fact (n - 1) in
fact (3)
def fact = fun x => if x = 0 then | 1 | x * fact (x-1) in
fact (4)
def fact = fun x => if x = 0 then | 1 | 1 + (fact (x-1)) in
fact (2)
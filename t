def x = 1 in
def y = thread (1 + 1) in
x + 1


Main Thread:
e1 ... thread e' ... e2
cps e1 (cps e2)

Thread 2
thread e => cps e
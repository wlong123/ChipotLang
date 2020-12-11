def l1 = ref [1] in
def l2 = ref [2] in
def t1 = thread (lockall [l1, l2]; l1 := [3]; unlockall [l1, l2]) in
def t2 = thread (lockall [l1, l2]; l2 := [4]; unlockall [l1, l2]) in
joinall;
print (!l1);
print (!l2)
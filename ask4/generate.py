def var(i):
    if i == 0:
        return ""
    return "x" + str(i) + ") " + var(i-1)
            
def func(i):
    if i == 0:
        return ""
    return "(\\x" + str(i) + ". " + func(i-1)

def create(i):
    return func(i) + (i-1) * "(" + "x" + str(i) + " " + var(i-1) + i * ")"


f = open("tests/test_g2.in", "w")
result = create(100);
print result;
f.write("100\n" + 500 * (result + "\n"))
f.close()


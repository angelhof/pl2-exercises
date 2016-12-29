import sys
import os
import random

try:
	n = int(sys.argv[1])
	m = int(sys.argv[2])
	test_dir = sys.argv[3]
except:
	print "Usage: " + sys.agrv[0] + " <nodes:Int> <edges:Int> <test_dir:String>"
	exit(1)

names = map(lambda x: int(x.split(".")[0][4:]) , os.listdir(test_dir))
last_name = test_dir+"/test"+str(max(names)+1)+".in"

file = open(last_name, "w")
file.write(str(n) + " " + str(m) + "\n")
temp = random.randrange(2, n)
set_c = set([1,temp])
set_e = set()
set_e.add((1,temp))
file.write("1 " + str(temp) + "\n")
for i in xrange(m-1):
	x = random.choice(list(set_c))
	y = random.randrange(1, n+1)
	while y == x or (x,y) in set_e or (y,x) in set_e:
		x = random.choice(list(set_c))
		y = random.randrange(1, n)

	set_c.add(y)
	set_e.add((x,y))
	file.write(str(x) + " " + str(y) + "\n")

print last_name
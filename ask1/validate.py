import sys
import os

try:
	my_exe = sys.argv[1]
	val_exe = sys.argv[2]
	test_directory = sys.argv[3]
	out_dir = sys.argv[4]
except:
	print "Usage: " + sys.argv[0] + " <my_exec> <val_exec> <test_dir> <output_dir>"
	exit(1)

all = 0
correct = 0
for file in sorted(os.listdir(test_directory)):
	file_path = test_directory+"/"+file
	
	out_file = file.split(".")[0] + ".out"
	out_file_path = out_dir+"/"+out_file

	v_out_file = "v_"+out_file
	v_out_file_path = out_dir+"/"+v_out_file	
	
	os.system("cat " + file_path + " | " + my_exe + " > " + out_file_path)
	os.system("cat " + file_path + " | " + val_exe + " > " + v_out_file_path)

	all += 1

	with open(out_file_path) as my_out:
		temp = my_out.readline()
		while True:
			try:
				my_res = int(temp)
				break
			except:
				temp = my_out.readline()

	with open(v_out_file_path) as v_out:
		v_res = int(v_out.readline())

	if(v_res == my_res):
		correct += 1
	else:
		print "Wrong output at: " + file

print "Correct: " + str(correct) + " out of: " + str(all)
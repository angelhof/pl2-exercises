
compile: cut_point.java
	javac cut_point.java -d class
	cabal install -j

check: class/Graph.class
	java -cp class Graph

validate: compile validate.py
	python validate.py "cabal run ask1" "java -cp class Graph" tests outputs 

enable_prof:
	cabal configure --enable-executable-profiling ; echo "Add this to ask1.cabal \"-with-rtsopts=-p -s -h -i0.1\" "

disable_prof:
	cabal configure --disable-executable-profiling

profile:
	cat tests/test45.in | cabal run ask1 && hp2ps -e8in -c ask1.hp

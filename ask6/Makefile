
.PHONY : all

all: compile run

debug: compile_debug run

compile:
	gcc interpreter.c -o interpreter

compile_debug:
	gcc interpreter.c -DDEBUG -o interpreter

run:
	./interpreter test1.befunge 
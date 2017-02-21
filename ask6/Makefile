
.PHONY : all

all: compile run

debug: compile_debug run

compile:
	gcc interpreter.c -o interpreter

compile_debug:
	gcc interpreter.c -DDEBUG -g -o interpreter

compile_opt:
	gcc interpreter_opt.c -o interpreter_opt

compile_debug_opt:
	gcc interpreter_opt.c -DDEBUG -g -o interpreter_opt


run:
	./interpreter test1.befunge 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define PROG_WIDTH  80
#define PROG_HEIGHT 25
#define NEXT_INSTRUCTION goto next_instruction

// All possible commands
#define ZERO 0
#define ONE  1
#define TWO  2
#define THREE 3
#define FOUR 4
#define FIVE 5
#define SIX 6
#define SEVEN 7
#define EIGHT 8
#define NINE 9
#define ADD 10
#define SUBTRACT 11
#define MULTIPLY 12
#define DIVIDE 13
#define MODULO 14
#define NOT 15
#define GREATER 16
#define RIGHT 17
#define LEFT 18
#define UP 19
#define DOWN 20
#define RANDOM 21
#define HORZ_IF 22
#define VERT_IF 23
#define STRING 24
#define DUP 25
#define SWAP 26
#define POP 27
#define BRIDGE 28
#define GET 29
#define PUT 30
#define INPUT_VAL 31
#define INPUT_CHAR 32
#define END 33
#define SPACE 34





// Direction 
enum direction { right
			   , down
			   , left
			   , up    };

// Program Counter contains row and column information
struct ProgramCounter
{
	int row;
	int column;
};
typedef struct ProgramCounter PC;

// The program size is 25 rows * 80 columns
static char program[PROG_HEIGHT][PROG_WIDTH];
FILE *program_file;
char * line = NULL;


/*********************************************************************************************/
/**
 * Define the Stack in a static way 
 * Source: https://groups.csail.mit.edu/graphics/classes/6.837/F04/cpp_notes/stack1.html
 */
#define STACK_MAX 1000
struct Stack {
    int     data[STACK_MAX];
    int     size;
};
typedef struct Stack Stack;

void Stack_Init(Stack *S)
{
    S->size = 0;
}

int Stack_Top(Stack *S)
{
    if (S->size == 0) {
        //fprintf(stderr, "Error: stack empty\n");
        return 0;
    } 

    return S->data[S->size-1];
}

void Stack_Push(Stack *S, int d)
{
    if (S->size < STACK_MAX)
        S->data[S->size++] = d;
    else
        fprintf(stderr, "Error: stack full\n");
}

int Stack_Pop(Stack *S)
{
    if (S->size == 0) {
        //Befunge stack doesnt underflow so return 0
    	return 0;
        //fprintf(stderr, "Error: stack empty\n");
    }
    else {
        S->size--;
    	return S->data[S->size];
    }
}
/*********************************************************************************************/

void move_pc(PC *pc, enum direction dir) {
	if(dir == right)
		pc->column = (pc->column + 1) % PROG_WIDTH;
	else if(dir == down)
		pc->row = (pc->row + 1) % PROG_HEIGHT;
	else if(dir == left)
		pc->column = (pc->column + PROG_WIDTH - 1) % PROG_WIDTH;
	else if(dir == up)
		pc->row = (pc->row + PROG_HEIGHT - 1) % PROG_HEIGHT;
	else{
		printf("Internal Error! Wrong direction %d\n", dir);
		exit(1);
	}
}

// TODO: Implement transform_opcodes retransform_opcodes 
/**
 * This function should transform all the characters of the file 
 * in integer opcodes. 
 * The transform opcodes should also transform characters that are not opcodes
 * in numbers so that we can later work seamlessly 
 */

void print_program(){
	int i, j;
	for(i=0; i<PROG_HEIGHT; i++){
		for(j=0; j < PROG_WIDTH; j++){
			if(program[i][j] != '\n'){
				putchar(program[i][j]);
			}
		}
		putchar('\n');
	}
}

/**
 * Just a clear up exit function
 */
void error_exit(int code){
	if(program_file) 
		fclose(program_file);

	if(line)
		free(line);
	exit(code);
}

int check_arguments_and_parse_program(int argc, char *argv[]){
	
    size_t len = 0;
    ssize_t read;
    size_t line_cnt = 0;

	if(argc != 2){
		printf("-- Interpreter Error! Wrong Argument Number\n");
		printf("-- Usage: ./interpreter <program_name>\n");
		error_exit(1);
	}

	program_file = fopen(argv[1], "r");
	if(!program_file){
		printf("-- Interpreter Error! File: \"%s\" doesn't exist\n", argv[1]);
		error_exit(1);
	}
	
    while ((read = getline(&line, &len, program_file)) != -1) {
        if(line_cnt >= PROG_HEIGHT){
			printf("-- Interpreter Error! Program contains more than %d lines\n", PROG_HEIGHT);
        	error_exit(1);
		}

        if(read > PROG_WIDTH + 1){
        	printf("-- Interpreter Error! Line: %lu is longer than %d characters\n", line_cnt+1, PROG_WIDTH);
        	error_exit(1);
        }
		strncpy(program[line_cnt++], line, PROG_WIDTH);
		

    }

#ifdef DEBUG    
    print_program();
#endif

    fclose(program_file);
    if (line)
        free(line);
	return 0;
}

char getOperation(PC pc){
	return program[pc.row][pc.column];
}


/**
 * Main Interpreter loop
 */
void loop(){

	// Program Counter is made up from two parts pc row and pc column
	PC pc; 
	pc.row = 0;
	pc.column = 0;

	// Current Direction of the program counter
    enum direction dir = right;

    // Current operation
    char op;

    // Program Stack
    Stack stack;
    Stack_Init(&stack);

    // Random number generator intialize
    srand(time(NULL));
 
 	// Disable buffer mode so we can print single characters
 	setbuf(stdout, NULL);

 	// String mode
 	int string_mode = 0;

 	// Temporary variables
 	int a, b, c;
 	char d;

    while(1) {
next_instruction:    	
    	op = getOperation(pc);

#ifdef DEBUG    
    	printf("Stack: %d\n", Stack_Top(&stack));
        printf("%c ", op);
#endif    	
    	
    	// Check whether the execution is in string mode
    	if(string_mode == 1){
    		if(op == '"'){
    			string_mode = 0;
    		}
    		else{
	    		a = (int) op;
	    		Stack_Push(&stack, a);
	    	}	
	    	move_pc(&pc, dir);
    		NEXT_INSTRUCTION;
    	}

    	switch (op) {
    		case '+':
    			a = Stack_Pop(&stack);
    			b = Stack_Pop(&stack);
    			c = a + b;
    			Stack_Push(&stack, c);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '-':
    			a = Stack_Pop(&stack);
    			b = Stack_Pop(&stack);
    			c = b - a;
    			Stack_Push(&stack, c);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '*':
    			a = Stack_Pop(&stack);
    			b = Stack_Pop(&stack);
    			c = b * a;
    			Stack_Push(&stack, c);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '/':
    			// TODO: Fix the division by zero
    			a = Stack_Pop(&stack);
    			b = Stack_Pop(&stack);
    			c = b / a;
    			Stack_Push(&stack, c);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '%':
    			// TODO: Fix the division by zero
    			a = Stack_Pop(&stack);
    			b = Stack_Pop(&stack);
    			c = b % a;
    			Stack_Push(&stack, c);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '!':
    			// TODO: Make sure that ! returns 0 or 1 always
    			a = Stack_Pop(&stack);
    			b = !a;
    			Stack_Push(&stack, b);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '`':
    			// TODO: Make sure that ` returns 0 or 1 always
    			a = Stack_Pop(&stack);
    			b = Stack_Pop(&stack);
    			c = b > a;
    			Stack_Push(&stack, c);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '>':
    			dir = right;
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '<':
    			dir = left;
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '^':
    			dir = up;
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case 'v':
    			dir = down;
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '?':
    			dir = rand() % 4;
    			move_pc(&pc, dir); 
    			NEXT_INSTRUCTION;
    		case '_':
    			a = Stack_Pop(&stack);
    			if(a)
    				dir = left;
    			else
    				dir = right;
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '|':
    			a = Stack_Pop(&stack);
    			if(a)
    				dir = up;
    			else
    				dir = down;
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '"':
    			string_mode = 1;
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case ':':
    			// TODO: Consider making this more efficient 
    			a = Stack_Pop(&stack);
    			Stack_Push(&stack, a);
    			Stack_Push(&stack, a);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '\\':
    			a = Stack_Pop(&stack);
    			b = Stack_Pop(&stack);
    			Stack_Push(&stack, a);
    			Stack_Push(&stack, b);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '$':
    			Stack_Pop(&stack);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '.':
    			// TODO: Check whether . prints a space after the int
    			a = Stack_Pop(&stack);
    			printf("%d ", a);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case ',':
    			// TODO: Check how numbers around the area are handled
    			a = Stack_Pop(&stack);
    			printf("%c", a);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '#':
    			move_pc(&pc, dir);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case 'g':
    			// TODO: Ensure if g shouldnt check boundaries
    			a = Stack_Pop(&stack); // y value should be less than 25
    			b = Stack_Pop(&stack); // x value should be less than 80
    			c = (int) program[a][b];
    			Stack_Push(&stack, c);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case 'p':
    			// TODO: Ensure if p shouldnt check boundaries
    			a = Stack_Pop(&stack); // y value should be less than 25
    			b = Stack_Pop(&stack); // x value should be less than 80
    			c = Stack_Pop(&stack); // value to be put
    			program[a][b] = (char) c;
    			Stack_Push(&stack, c);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '&':
    			scanf("%d", &a);
    			Stack_Push(&stack, a);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '~':
    			scanf("%c", &d);
    			Stack_Push(&stack, (int) d);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '0':
    			Stack_Push(&stack, 0);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '1':
    			Stack_Push(&stack, 1);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '2':
    			Stack_Push(&stack, 2);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '3':
    			Stack_Push(&stack, 3);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '4':
    			Stack_Push(&stack, 4);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '5':
    			Stack_Push(&stack, 5);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '6':
    			Stack_Push(&stack, 6);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '7':
    			Stack_Push(&stack, 7);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '8':
    			Stack_Push(&stack, 8);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '9':
    			Stack_Push(&stack, 9);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '@':
    			exit(0);
    		case ' ':
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		default:
    			printf("Error: Unknown Command: %c at (%d, %d) \n", op, pc.column, pc.row);
    			exit(1);
    	}
    }

}


int main(int argc, char *argv[]){
	check_arguments_and_parse_program(argc, argv);	
	loop();
	return 0;
} 
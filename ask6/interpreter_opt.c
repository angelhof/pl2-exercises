#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define PROG_WIDTH  80
#define PROG_HEIGHT 25
//#define NEXT_INSTRUCTION goto next_instruction

#define NEXT_INSTRUCTION goto *(void *)(p_labels[pc.row][pc.column])

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
static void *p_labels[PROG_HEIGHT][PROG_WIDTH];
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

	if(argc != 2 && argc != 3){
		printf("-- Interpreter Error! Wrong Argument Number\n");
		printf("-- Usage: ./interpreter <program_name> [-stdin]\n");
		error_exit(1);
	}

    /**
     * STDIN input used from the api
     */
    if(argc == 3){
        program_file = stdin;
    }
    else{
    	program_file = fopen(argv[1], "r");
     
        if(!program_file){
    		printf("-- Interpreter Error! File: \"%s\" doesn't exist\n", argv[1]);
    		error_exit(1);
    	}
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
    if(argc == 2){
        fclose(program_file);
    }
    
    if (line)
        free(line);

#ifdef DEBUG    
    print_program();
#endif

	return 0;
}

char getOperation(PC pc){
	return program[pc.row][pc.column];
}

void create_program_labels(void *label_tab[]){
    int i,j;
    for(i=0;i<PROG_HEIGHT;i++){
        for(j=0;j<PROG_WIDTH;j++){
            p_labels[i][j] = label_tab[(int)program[i][j]];
        }
    }
}


/**
 * Main Interpreter loop
 */
void loop(){
    // Initialize the label table
    static void *label_tab[] = {
        &&null_label,       // 0 - NULL
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&newline_label,    // 10 - Line feed
        &&error_label,
        &&error_label,
        &&cr_label,         // 13 - Carriage Return    
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,      // 20
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,      // 30
        &&error_label,      // 31 - Last Ascii control character
        &&space_label,      // 32 - Space
        &&not_label,        // 33 - !
        &&string_label,     // 34 - "
        &&jump_label,       // 35 - #
        &&pop_label,        // 36 - $
        &&modulo_label,     // 37 - %
        &&read_int_label,   // 38 - &
        &&error_label,      // 39 - '
        &&error_label,      // 40 - (
        &&error_label,      // 41 - )
        &&multiply_label,   // 42 - *
        &&add_label,        // 43 - +
        &&print_char_label, // 44 - ,
        &&subtract_label,   // 45 - -
        &&print_int_label,  // 46 - .
        &&divide_label,     // 47 - /
        &&zero_label,       // 48 - 0
        &&one_label,
        &&two_label,
        &&three_label,
        &&four_label,
        &&five_label,
        &&six_label,
        &&seven_label,
        &&eight_label,
        &&nine_label,       // 57 - 9
        &&duplicate_label,  // 58 - :
        &&error_label,      // 59 - ;
        &&left_label,       // 60 - <
        &&error_label,      // 61 - =
        &&right_label,      // 62 - >
        &&random_label,     // 63 - ?
        &&exit_label,       // 64 - @
        &&error_label,      // 65 - A
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,      // 90 - Z
        &&error_label,      // 91 - [
        &&invert_label,     // 92 - '\'  
        &&error_label,      // 93 - ]
        &&up_label,         // 94 - ^
        &&horz_if_label,    // 95 - _
        &&greater_label,    // 96 - `
        &&error_label,      // 97 - a
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&get_label,        // 103 - g
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&put_label,        // 112 - p
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,
        &&down_label,       // 118 - v
        &&error_label,
        &&error_label,
        &&error_label,
        &&error_label,      // 122 - z
        &&error_label,      // 123 - {
        &&vert_if_label,    // 124 - |
        &&error_label,      // 125 - }
        &&read_char_label,  // 126 - ~
        &&error_label,      // 127 - DEL
    };

    create_program_labels(label_tab);

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
        printf("Stack size: %d\n", stack.size);
        printf("Current char: '%c'\n", op);
        getchar();
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
    		//NEXT_INSTRUCTION;
            goto next_instruction;
    	}

    	switch (op) {
    		case '+':
            add_label:
    			a = Stack_Pop(&stack);
    			b = Stack_Pop(&stack);
    			c = a + b;
    			Stack_Push(&stack, c);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '-':
            subtract_label:
    			a = Stack_Pop(&stack);
    			b = Stack_Pop(&stack);
    			c = b - a;
    			Stack_Push(&stack, c);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '*':
            multiply_label:
    			a = Stack_Pop(&stack);
    			b = Stack_Pop(&stack);
    			c = b * a;
    			Stack_Push(&stack, c);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '/':
            divide_label:
    			// TODO: Fix the division by zero
    			a = Stack_Pop(&stack);
    			b = Stack_Pop(&stack);
    			c = b / a;
    			Stack_Push(&stack, c);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '%':
            modulo_label:
    			// TODO: Fix the division by zero
    			a = Stack_Pop(&stack);
    			b = Stack_Pop(&stack);
    			c = b % a;
    			Stack_Push(&stack, c);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '!':
            not_label:
    			// TODO: Make sure that ! returns 0 or 1 always
    			a = Stack_Pop(&stack);
    			b = !a;
    			Stack_Push(&stack, b);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '`':
            greater_label:
    			// TODO: Make sure that ` returns 0 or 1 always
    			a = Stack_Pop(&stack);
    			b = Stack_Pop(&stack);
    			c = b > a;
    			Stack_Push(&stack, c);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '>':
            right_label:
    			dir = right;
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '<':
            left_label:
    			dir = left;
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '^':
            up_label:
    			dir = up;
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case 'v':
            down_label:
    			dir = down;
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '?':
            random_label:
    			dir = rand() % 4;
    			move_pc(&pc, dir); 
    			NEXT_INSTRUCTION;
    		case '_':
            horz_if_label:
    			a = Stack_Pop(&stack);
    			if(a)
    				dir = left;
    			else
    				dir = right;
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '|':
            vert_if_label:
    			a = Stack_Pop(&stack);
    			if(a)
    				dir = up;
    			else
    				dir = down;
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '"':
            string_label:
    			string_mode = 1;
    			move_pc(&pc, dir);
    			//NEXT_INSTRUCTION;
                goto next_instruction;
    		case ':':
            duplicate_label:
    			// TODO: Consider making this more efficient 
    			a = Stack_Pop(&stack);
    			Stack_Push(&stack, a);
    			Stack_Push(&stack, a);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '\\':
            invert_label:
    			a = Stack_Pop(&stack);
    			b = Stack_Pop(&stack);
    			Stack_Push(&stack, a);
    			Stack_Push(&stack, b);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '$':
            pop_label:
    			Stack_Pop(&stack);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '.':
            print_int_label:
    			// TODO: Check whether . prints a space after the int
    			a = Stack_Pop(&stack);
    			printf("%d ", a);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case ',':
            print_char_label:
    			// TODO: Check how numbers around the area are handled
    			a = Stack_Pop(&stack);
    			printf("%c", a);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '#':
            jump_label:
    			move_pc(&pc, dir);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case 'g':
            get_label:
    			// TODO: Ensure if g shouldnt check boundaries
    			a = Stack_Pop(&stack); // y value should be less than 25
    			b = Stack_Pop(&stack); // x value should be less than 80
    			c = (int) program[a][b];
                //printf("Char at: %c\n", program[a][b]);
                //printf("a: %d, b: %d, c: %d\n", a,b,c);
    			Stack_Push(&stack, c);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case 'p':
            put_label:
    			// TODO: Ensure if p shouldnt check boundaries
    			a = Stack_Pop(&stack); // y value should be less than 25
    			b = Stack_Pop(&stack); // x value should be less than 80
    			c = Stack_Pop(&stack); // value to be put
    			//printf("Previous char at: %c\n", program[a][b]);
                //printf("a: %d, b: %d, c: %d\n", a,b,c);
                program[a][b] = (char) c;
                if(c >= 0 && c <128){
                    p_labels[a][b] = label_tab[c];
                }
                else{
                    p_labels[a][b] = &&error_label;   
                }
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '&':
            read_int_label:
    			scanf("%d", &a);
    			Stack_Push(&stack, a);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '~':
            read_char_label:
    			scanf("%c", &d);
    			Stack_Push(&stack, (int) d);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '0':
            zero_label:
    			Stack_Push(&stack, 0);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '1':
            one_label:
    			Stack_Push(&stack, 1);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '2':
            two_label:
    			Stack_Push(&stack, 2);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '3':
            three_label:
    			Stack_Push(&stack, 3);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '4':
            four_label:
    			Stack_Push(&stack, 4);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '5':
            five_label:
    			Stack_Push(&stack, 5);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '6':
            six_label:
    			Stack_Push(&stack, 6);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '7':
            seven_label:
    			Stack_Push(&stack, 7);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '8':
            eight_label:
    			Stack_Push(&stack, 8);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '9':
            nine_label:
    			Stack_Push(&stack, 9);
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
    		case '@':
            exit_label:
    			exit(0);
    		case ' ':
            space_label:
    			move_pc(&pc, dir);
    			NEXT_INSTRUCTION;
            // The next three have been added for the api
            case '\r':
            cr_label:
                move_pc(&pc, dir);
                NEXT_INSTRUCTION;
            case '\n':
            newline_label:
                move_pc(&pc, dir);
                NEXT_INSTRUCTION; 
            case 0:
            null_label:
                move_pc(&pc, dir);
                NEXT_INSTRUCTION;       
    		default:
            error_label:
    			printf("Error: Unknown Command: %d at (%d, %d) \n", op, pc.column, pc.row);
    			exit(1);
    	}
    }

}





int main(int argc, char *argv[]){
    check_arguments_and_parse_program(argc, argv);	
	loop();
	return 0;
} 
%{

#include <stdio.h>
#include <stdlib.h>
#include "stack.h"

int p(int base, unsigned int exp) {
        int r = 1;
        for (int i = 0; i < exp; i++){
			r *= base;
		}
        return r;
}

struct Stack stack = {NULL};
int error = 0;

%}

%x ERROR

%%

-?[0-9]+		push(&stack, atoi(yytext));

\+				{
					int val1 = pop(&stack, &error);
					int val2 = pop(&stack, &error);
					if(error){
						printf("Too few arguments\n");
						BEGIN(ERROR);
					}else{
						push(&stack, val1 + val2);
					}
				}
\*              {
                    int val1 = pop(&stack, &error);
					int val2 = pop(&stack, &error);
                    if (error){
                        printf("Too few arguments\n");
						BEGIN(ERROR);
                    }else{
						push(&stack, val1 * val2);
					}
                }
\-              {
                    int val1 = pop(&stack, &error);
                    int val2 = pop(&stack, &error);
                    if (error){
                        printf("Too few arguments\n");
						BEGIN(ERROR);
                    }else{
						push(&stack, val2 - val1);
					}
                }
\/              {
                    int val1 = pop(&stack, &error);
                    int val2 = pop(&stack, &error);
                    if (error) {
                        printf("Too few arguments\n");
						BEGIN(ERROR);
                    } else if (val1 == 0) {
						printf("Can't divide by 0\n");
						BEGIN(ERROR);
                    } else {
                        push(&stack, val2 / val1);
                    }
				}
\^              {
                    int exp = pop(&stack, &error);
					int val = pop(&stack, &error);
                    if (error) {
                        printf("Too few arguments\n");
						BEGIN(ERROR);
                    } else if (exp < 0) {
                        printf("Negative exp\n");
						BEGIN(ERROR);
                    } else {
                        push(&stack, p(val,exp));
                    }
                }
\%              {
                    int val1 = pop(&stack, &error);
                    int val2 = pop(&stack, &error);
                    if (error) {
                        printf("Too few arguments\n");
						BEGIN(ERROR);
                    } else if (val1 == 0) {
						printf("Can't modulo by 0\n");
						BEGIN(ERROR);
                    } else {
                        push(&stack, val2 % val1);
                    }
				}
\n              {
					int result = pop(&stack, &error);
					if(!isEmpty(&stack)){
                        printf("Too few operators\n");
                        while(!isEmpty(&stack)){
							pop(&stack, &error);
						}
                    }else if(!error){
						printf("=%d\n",result);
					}
                }

[[:blank:]]		;
.				{
					printf("Invalid character\n");
					while(!isEmpty(&stack)) {
						pop(&stack, &error);
					}
					BEGIN(ERROR);
				}
<ERROR>{
\n					{
						while(!isEmpty(&stack)){
							pop(&stack, &error);
						}
						error = 0;
						BEGIN(INITIAL);
					}

.					;
}
%%

int yywrap(){
	return 1;
}

int main(int argc, char *argv[]) { 
    
	while(1){
		yylex();
	}

    return 0;
}

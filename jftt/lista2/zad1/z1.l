%{

#include <stdio.h>

int lines = 0;
int words = 0;

%}

%s START
%s NEWLINE
%s WORD
%s SPACES

%%

	BEGIN(NEWLINE);

<NEWLINE>{
[[:blank:]]|\n	;
.		{words++; printf("%s",yytext); BEGIN(WORD);}
}

<WORD>{
[[:blank:]] 	BEGIN(SPACES);
\n 		{lines++; printf("\n"); BEGIN(NEWLINE);}
.		printf("%s",yytext);	
}


<SPACES>{
[[:blank:]] 	;
\n 		{lines++; printf("\n"); BEGIN(NEWLINE);}
. 		{printf(" %s",yytext); words++; BEGIN(WORD);}
}

%%

int yywrap(){
	return 1;
}

int main(int argc, char *argv[]) { 
    FILE *file = fopen(argv[1], "r");
    yyin = file;
    yylex();

    fclose(file);

    return 0;
}

%{

#include <stdio.h>

%}

%x CDATA
%x COMMENT
%x STRING

%%

\"		{ECHO; BEGIN(STRING);}
\<!--		BEGIN(COMMENT);
\<!\[CDATA\[	{ECHO; BEGIN(CDATA);}


<STRING>{
\"		{ECHO; BEGIN(INITIAL);}
.		ECHO;
}

<COMMENT>{
.*--[^>]	{printf("<!--"); ECHO; BEGIN(INITIAL);}
-->		BEGIN(INITIAL);
.		;
}

<CDATA>{
]]>		{ECHO; BEGIN(INITIAL);}
.		ECHO;
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

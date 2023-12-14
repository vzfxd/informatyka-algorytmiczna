%{

#include <stdio.h>
int docs = 0;

%}

%x S_DOC
%x S_COM
%x M_DOC
%x M_COM
%x STRING
%x INCLUDE


abc		\\\n

s_com		\/{abc}*\/
s_doc		\/{abc}*\/{abc}*[\/\!]

m_com_o		\/{abc}*\*
m_com_c		\*{abc}*\/

m_doc_o		\/{abc}*\*{abc}*[\*\!]
m_doc_c		\*{abc}*\/

include \#include[[:blank:]]["\<].*[">]

%%

{include}	{ECHO;}
\"		{ECHO; BEGIN(STRING);}
{m_doc_o}	{if(docs)ECHO;  BEGIN(M_DOC);}
{s_doc}		{if(docs)ECHO;  BEGIN(S_DOC);}
{s_com}		BEGIN(S_COM);
{m_com_o}	BEGIN(M_COM);


<STRING>{
\"		{ECHO; BEGIN(INITIAL);}
.		ECHO;
}

<S_COM>{
{abc}		;
\n		BEGIN(INITIAL);
.		;
}

<M_COM>{
{m_com_c}	BEGIN(INITIAL);
.		;
}

<M_DOC>{
{m_doc_c}	{if(docs)ECHO; BEGIN(INITIAL);}
.		{if(docs)ECHO;}
}

<S_DOC>{
{abc}		{if(docs)ECHO;}
\n		{if(docs)ECHO; BEGIN(INITIAL);}
.		{if(docs)ECHO;}
}

%%

int yywrap(){
	return 1;
}

int main(int argc, char *argv[]) { 
    FILE *file = fopen(argv[1], "r");
    docs = atoi(argv[2]);
    yyin = file;
    yylex();

    fclose(file);

    return 0;
}

%option noyywrap

%{
#include <stdio.h>
#include "parser.hpp"
%}
%s COMMENT
%%
<INITIAL>{
    ^\n               ;
    ^#                {BEGIN(COMMENT);}
    ([0-9])+          {yylval = atoi(yytext); return NUM;}
    [+\-*/\^\(\)]|\n  {return *yytext;}
    \\\n              ;
    .                 {return ERR;}
}

<COMMENT>{
    \\\n          ;
    \n            {BEGIN(INITIAL);}
    .             ;
}
%%
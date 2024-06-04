:-consult('scaner.pl').

program([]) --> [].
program([I|P]) --> instruct(I), [sep(';')], program(P).


instruct(assign(ID,EXP)) --> [id(ID),sep(':=')] , exp(EXP).

instruct(read(ID)) --> [key('read'),id(ID)].
instruct(write(EXP)) --> [key('write')], exp(EXP).
instruct(if(X,Y)) --> [key('if')], cond(X), [key('then')], program(Y), [key('fi')].
instruct(if(X,Y,Z)) --> 
		[key('if')], cond(X), [key('then')], 
		program(Y), [key('else')], program(Z), [key('fi')].
instruct(while(X,Y)) --> [key('while')], cond(X), [key('do')], program(Y), [key('od')].


exp(X+Y) --> element(X), [sep('+')], exp(Y).
exp(X-Y) --> element(X), [sep('-')], exp(Y).
exp(X) --> element(X).

element(X*Y) --> factor(X), [sep('*')], element(Y).
element(X/Y) --> factor(X), [sep('/')], element(Y).
element(X mod Y) --> factor(X), [sep('mod')], element(Y).
element(X) --> factor(X).


factor(id(ID)) --> [id(ID)].
factor(int(INT)) --> [int(INT)].
factor((X)) --> [sep('(')], exp(X), [sep(')')].

cond(X ; Y) --> conj(X), [sep('or')], cond(Y).
cond(X) --> conj(X).

conj(X,Y) --> simple(X), [sep('and')], conj(Y).
conj(Y) --> simple(Y).

simple(X=:=Y) --> exp(X),[sep('=')],exp(Y).
simple(X=\=Y) --> exp(X),[sep('/=')],exp(Y).
simple(X<Y) --> exp(X),[sep('<')],exp(Y).
simple(X>Y) --> exp(X),[sep('>')],exp(Y).
simple(X=<Y) --> exp(X),[sep('=<')],exp(Y).
simple(X>=Y) --> exp(X),[sep('>=')],exp(Y).
simple((X)) --> exp(X).
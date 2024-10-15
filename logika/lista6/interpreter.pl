interpreter(INSTRUKCJE) :-
	interpreter(INSTRUKCJE, []).

% interpreter(+INSTRUKCJE, +ASOCJACJE)
% ASOCJACJE = []
% ASOCJACJE = [ASOCJACJA | ASOCJACJE]
% ASOCJACJA = ID = NUM

interpreter([], _).

interpreter([read(ID) | INSTRUKCJE], ASOC) :- !,
	read(N),
	integer(N),
	podstaw(ASOC, ID, N, ASOC1),
	interpreter(INSTRUKCJE, ASOC1).

interpreter([write(W) | INSTRUKCJE], ASOC) :- !,
	wartosc(W, ASOC, WART),
	write(WART), nl,
	interpreter(INSTRUKCJE, ASOC).

interpreter([assign(ID, W) | INSTRUKCJE], ASOC) :- !,
	wartosc(W, ASOC, WAR),
	podstaw(ASOC, ID, WAR, ASOC1),
	interpreter(INSTRUKCJE, ASOC1).

interpreter([if(C, P) | INSTRUKCJE], ASOC) :- !,
	interpreter([if(C, P, []) | INSTRUKCJE], ASOC).

interpreter([if(C, P1, P2) | INSTRUKCJE], ASOC) :- !,
	(   prawda(C, ASOC)
	->  append(P1, INSTRUKCJE, DALEJ)
	;   append(P2, INSTRUKCJE, DALEJ)),
	interpreter(DALEJ, ASOC).

interpreter([while(C, P) | INSTRUKCJE], ASOC) :- !,
	append(P, [while(C, P)], DALEJ),
	interpreter([if(C, DALEJ) | INSTRUKCJE], ASOC).


podstaw([], ID, N, [ID = N]).

podstaw([ID = _ | ASOC], ID, N, [ID = N | ASOC]) :- !.

podstaw([ID1 = W1 | ASOC1], ID, N, [ID1 = W1 | ASOC2]) :-
	podstaw(ASOC1, ID, N, ASOC2).


pobierz([ID = N | _], ID, N) :- !.

pobierz([_ | ASOC], ID, N) :-
	pobierz(ASOC, ID, N).


wartosc(int(N), _, N).

wartosc(id(ID), ASOC, N) :-
	pobierz(ASOC, ID, N).

wartosc(W1+W2, ASOC, N) :-
	wartosc(W1, ASOC, N1),
	wartosc(W2, ASOC, N2),
	N is N1+N2.

wartosc(W1-W2, ASOC, N) :-
	wartosc(W1, ASOC, N1),
	wartosc(W2, ASOC, N2),
	N is N1-N2.

wartosc(W1*W2, ASOC, N) :-
	wartosc(W1, ASOC, N1),
	wartosc(W2, ASOC, N2),
	N is N1*N2.

wartosc(W1/W2, ASOC, N) :-
	wartosc(W1, ASOC, N1),
	wartosc(W2, ASOC, N2),
	N2 =\= 0,
	N is N1 div N2.

wartosc(W1 mod W2, ASOC, N) :-
	wartosc(W1, ASOC, N1),
	wartosc(W2, ASOC, N2),
	N2 =\= 0,
	N is N1 mod N2.

prawda(W1 =:= W2, ASOC) :-
	wartosc(W1, ASOC, N1),
	wartosc(W2, ASOC, N2),
	N1 =:= N2.

prawda(W1 =\= W2, ASOC) :-
	wartosc(W1, ASOC, N1),
	wartosc(W2, ASOC, N2),
	N1 =\= N2.

prawda(W1 < W2, ASOC) :-
	wartosc(W1, ASOC, N1),
	wartosc(W2, ASOC, N2),
	N1 < N2.

prawda(W1 > W2, ASOC) :-
	wartosc(W1, ASOC, N1),
	wartosc(W2, ASOC, N2),
	N1 > N2.

prawda(W1 >= W2, ASOC) :-
	wartosc(W1, ASOC, N1),
	wartosc(W2, ASOC, N2),
	N1 >= N2.
prawda(W1 =< W2, ASOC) :-
	wartosc(W1, ASOC, N1),
	wartosc(W2, ASOC, N2),
	N1 =< N2.

prawda((W1, W2), ASSOC) :-
	prawda(W1, ASSOC),
	prawda(W2, ASSOC).

prawda((W1; W2), ASSOC) :-
	(   prawda(W1, ASSOC),
	    !
	;   prawda(W2, ASSOC)).
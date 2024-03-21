jednokrotnie(X, L) :-
    select(X,L,L1),
	\+ member(X,L1).

dwukrotnie(X, L) :-
	append(L1, [X|T], L),
	\+ member(X, L1),
	jednokrotnie(X, T).
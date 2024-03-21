jednokrotnie(X, [X|T]) :-
    \+ member(X, T).

jednokrotnie(X, [_|T]) :-
    jednokrotnie(X, T).

dwukrotnie(X, L) :-
	append(L1, [X|T], L),
	\+ member(X, L1),
	jednokrotnie(X, T).
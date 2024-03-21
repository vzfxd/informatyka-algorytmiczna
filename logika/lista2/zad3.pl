arc(a, b).
arc(b, a).
arc(b, c).
arc(c, d).

osiagalny(X,Y) :-
    osiagalny(X,Y, []).

osiagalny(X,X,_).

osiagalny(X,Y,V) :-
	arc(X,Z),
	\+ member(Z,V),
	osiagalny(Z, Y, [X|V]).
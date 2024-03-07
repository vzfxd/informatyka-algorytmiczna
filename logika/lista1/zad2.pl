on(b1,b2).
on(b2,b3).
on(b3,b4).
on(b4,b5).

above(X,Y) :- on(X,Y), X \= Y.
above(X,Y) :- on(X,Z), above(Z,Y).
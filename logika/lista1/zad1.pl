ojciec(bolek,rysiek).
ojciec(rysiek,edek).
matka(halina,edek).
mezczyzna(rysiek).
kobieta(halina).
rodzic(rysiek,edek).
rodzic(halina,edek).

jest_matka(X) :- matka(X,_).

jest_ojcem(X) :- ojciec(X,_).

jest_synem(X) :- mezczyzna(X), rodzic(_,X).

siostra(X,Y) :- kobieta(X), rodzenstwo(X,Y).

dziadek(X,Y) :- ojciec(X,Z), rodzic(Z,Y), X \= Y.

rodzenstwo(X,Y) :- rodzic(Z,X), rodzic(Z,Y), X \= Y.

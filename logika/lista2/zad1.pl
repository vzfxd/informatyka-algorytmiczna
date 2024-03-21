dlugosc([], 0).
dlugosc([_|T], Len) :-
    dlugosc(T, LenT),
    Len is LenT + 1.

srodkowy(L, X) :-
    dlugosc(L, Len),
    LenMod is Len mod 2,
    LenMod =:= 1,
    N is (Len + 1) // 2,
    nth1(N, L, X).
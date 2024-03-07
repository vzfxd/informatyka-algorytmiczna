is_prime(N) :-
    N > 1,
    N1 is N - 1,
    \+ (between(2, N1, X), N mod X =:= 0).

prime(LO, HI, N) :-
    between(LO, HI, N),
    is_prime(N).
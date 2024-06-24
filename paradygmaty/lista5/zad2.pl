split(X, L1, L2):-
    length(X, Len),
    Len1 is Len // 2,
    Len2 is Len - Len1,
    length(L1, Len1),
    length(L2, Len2),
    append(L1, L2, X).


merge(X, [], X).
merge([], Y, Y).
merge([X|Xs], [Y|Ys], [X|Z]) :-
    X =< Y, 
    merge(Xs, [Y|Ys], Z).
merge([X|Xs], [Y|Ys], [Y|Z]) :-
    X > Y, 
    merge([X|Xs], Ys, Z).


mergesort([X], [X]) :- !.
mergesort(X, Sorted) :-
    split(X, L1, L2),
    mergesort(L1, SortedL1),
    mergesort(L2, SortedL2),
    merge(SortedL1, SortedL2, Sorted).



extend_gcd(A, B, 1, 0, A ) :-
    B =:= 0.
extend_gcd(A, B, X, Y, GCD) :-
    B > 0,
    C is A mod B,
    extend_gcd(B, C, X1, Y1, GCD),
    X is Y1,
    Y is X1 - Y1 * (A // B).

de(A, B, X, Y, GCD) :-
    extend_gcd(A, B, X, Y, GCD).
 


prime_factors(N, X) :-
    prime_factors(N, 2, X).

prime_factors(1, _, []) :- !.
prime_factors(N, D, [D|X]) :-
    N mod D =:= 0,
    N1 is N // D,
    prime_factors(N1, D, X).

prime_factors(N, D, X) :-
    N mod D =\= 0,
    D1 is D + 1,
    prime_factors(N, D1, X).



totient(N, T) :-
    prime_factors(N, X),
    totient(X,1,T). % oblicza phi uzywajac rozkladu na czynniki pierwsze

% phi(n) = (p1^(k1 -1)) * (p1 - 1) * (p2^(k2 -1)) * (p2 - 1) *    ... * (pr^(kr -1)) * (pr - 1)

totient([], Phi, Phi).

totient([X], Acc, Phi) :-
    NewAcc is Acc * (X - 1),
    totient([], NewAcc, Phi).

totient([X, X|T], Acc, Phi) :-
    NewAcc is Acc * X,
    totient([X|T], NewAcc, Phi).

totient([X, Y|T], Acc, Phi) :-
    X \= Y,
    NewAcc is Acc * (X - 1),
    totient([Y|T], NewAcc, Phi).




is_prime(2).
is_prime(N) :-
    N > 1,
    N mod 2 =\= 0,
    S is floor(sqrt(N)),
    \+ (between(2, S, X), N mod X =:= 0).

prime(HI, N) :-
    between(2, HI, N),
    is_prime(N).

primes(HI, X) :-
    findall(N, prime(HI, N), X).
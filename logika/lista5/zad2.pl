board(L) :-
    length(L, N),
    draw(L, 1, N).

draw(_, Row, N) :-
    Row > N,
    loop(N),
    nl,
    !.

draw(L, R, N) :-
    loop(N),
    Rev is N + 1 - R,
    row(Rev, 1, L, N),
    row(Rev, 1, L, N),
    Rn is R + 1,
    draw(L, Rn, N),
    !.

row(_, Col, _, N) :-
    N + 1 =:= Col,
    write('|'),
    nl,
    !.

row(Row, Col, Tab, N) :-
    nth1(Col, Tab, Row),
    (   
        (
        ((Row + N + 1) mod 2 =:= 0, Col mod 2 =:= 1);
        ((Row + N + 1) mod 2 =:= 1, Col mod 2 =:= 0)
        ) -> write('|:###:') ; write('| ### ')
    ),
    Col1 is Col + 1,
    row(Row, Col1, Tab,N),
    !.

row(Row, Col, Tab, N) :-
    (   
        (
        ((Row + N + 1) mod 2 =:= 0, Col mod 2 =:= 1);
        ((Row + N + 1) mod 2 =:= 1, Col mod 2 =:= 0)
        ) -> write('|:::::') ; write('|     ')
    ),
    Col1 is Col + 1,
    row(Row, Col1, Tab,N),
    !.

loop(0) :-
    write('+'),
    nl,
    !.

loop(N) :-
    write('+-----'),
    N1 is N - 1,
    loop(N1).

hetmany(N, P) :-
    numlist(1, N, L),
    permutation(L,P),
    dobra(P).

dobra(P) :-
    \+ zla(P).

zla(P) :-
    append(_, [Wi | L1], P),
    append(L2, [Wj | _ ], L1),
    length(L2, K),
    abs(Wi - Wj) =:= K + 1.
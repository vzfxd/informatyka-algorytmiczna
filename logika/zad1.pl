option(EXPRESSION, EXP1, EXP2) :- EXPRESSION = EXP1 + EXP2.
option(EXPRESSION, EXP1, EXP2) :- EXPRESSION = EXP1 - EXP2.
option(EXPRESSION, EXP1, EXP2) :- EXPRESSION = EXP1 * EXP2.

combinations([EXPRESSION], EXPRESSION).
combinations(L, EXPRESSION) :-
	append(L1, L2, L),
	\+ length(L1, 0),
	\+ length(L2, 0),
	combinations(L1, EXP1),
	combinations(L2, EXP2),
	option(EXPRESSION, EXP1, EXP2).

wyrażenie(NUMBERS, VALUE, EXPRESSION) :-
	combinations(NUMBERS, EXP),
	VALUE is EXP,
	EXPRESSION = EXP.
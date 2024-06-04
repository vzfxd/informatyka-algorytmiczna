:- ['scaner.pl','zad1.pl','interpreter.pl'].

wykonaj(N) :- 
	open(N,read,X),
	scanner(X,Y),
	close(X),
	phrase(program(PROGRAM),Y), 
	interpreter(PROGRAM).
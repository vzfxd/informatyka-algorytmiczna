main: parser.y lexer.l
	bison -d -o parser.cpp parser.y
	flex -o lexer.c lexer.l
	g++ -o zad1 parser.cpp lexer.c

clean:
	rm parser.cpp parser.hpp lexer.c zad1
all: ada_test c_test

ada_test: libs/biblio.adb
	gnatmake -Ilibs/ -o ada_test.out tests.adb

c_test: libs/biblio.h tests.c
	gcc -Wall tests.c libs/biblio.h -o c_test.out

clean:
	@rm -rf *.out *.ali *.o b~* *.so
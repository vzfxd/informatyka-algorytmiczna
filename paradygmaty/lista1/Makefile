all: ada_test c_test c_in_python

ada_test: libs/biblio.adb
	gnatmake -Ilibs/ -o ada_test.out tests.adb

c_test: libs/biblio.c tests.c
	gcc -Wall tests.c libs/biblio.c -o c_test.out

c_in_python: libs/biblio.h
	gcc -fPIC -shared libs/biblio.c -o c.so 

clean:
	@rm -rf *.out *.ali *.o b~* *.so
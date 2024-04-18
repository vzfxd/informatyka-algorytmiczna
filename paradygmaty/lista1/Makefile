all: ada_test c_test c_in_python ada_in_c

ada_test: libs/biblio.adb
	gnatmake -Ilibs/ -o ada_test.out tests.adb

c_test: libs/biblio.c tests.c
	gcc -Wall tests.c libs/biblio.c -o c_test.out

c_in_python: libs/biblio.h
	gcc -fPIC -shared libs/biblio.c -o c.so

ada_in_c: ada_in_c.c libs/biblio.adb
	gcc -c ada_in_c.c  
	gnatmake -c libs/biblio.adb
	gnatbind -n biblio.ali
	gnatlink biblio.ali ada_in_c.o -o ada_in_c.out

clean:
	@rm -rf *.out *.ali *.o b~* *.so
../c_api : c_api.o set_2.o
				gcc -o $@ $^

c_api.o : c_api.c c_api.h sets.h set_2.h
				gcc -c c_api.c

set_2.o : set_2.c sets.h set_2.h
				gcc -c set_2.c

clean :
				rm ../c_api c_api.o set_2.o

../c_api : c_api.o set_2.o set_7.o
				gcc -o $@ $^

c_api.o : c_api.c errors.c errors.h sets.h set_2.h set_7.h c_api.h
				gcc -c c_api.c

set_2.o : set_2.c errors.c errors.h sets.h set_2.h
				gcc -c set_2.c

set_7.o : set_7.c errors.c errors.h sets.h set_7.h
				gcc -c set_7.c

errors.o: errors.c errors.h
				gcc -c errors.c

clean :
				rm ../c_api c_api.o set_2.o set_7.o errors.o

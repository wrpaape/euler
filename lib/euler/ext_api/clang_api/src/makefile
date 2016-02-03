../clang_api : clang_api.o set_2.o
				gcc -o $@ $^

clang_api.o : clang_api.c clang_api.h sets.h set_2.h
				gcc -c clang_api.c

set_2.o : set_2.c sets.h set_2.h
				gcc -c set_2.c

clean :
				rm ../clang_api clang_api.o set_2.o

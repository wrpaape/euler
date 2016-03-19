CC = gcc
CFLAGS = -c
LIBS = -lgmp

../c_api : c_api.o sets.o set_2.o set_3.o set_4.o set_5.o set_7.o
	$(CC) $(LIBS) -o $@ $^

c_api.o : c_api.c c_api.h sets.h set_2.h set_3.h set_4.h set_7.h
	$(CC) $(CFLAGS) c_api.c

set_2.o : set_2.c set_2.h sets.h
	$(CC) $(CFLAGS) set_2.c

set_3.o : set_3.c set_3.h sets.h
	$(CC) $(CFLAGS) set_3.c

set_4.o : set_4.c set_4.h sets.h
	$(CC) $(CFLAGS) set_4.c

set_5.o : set_5.c set_5.h sets.h
	$(CC) $(CFLAGS) set_5.c

set_7.o : set_7.c set_7.h sets.h
	$(CC) $(CFLAGS) set_7.c

sets.o: sets.c sets.h
	$(CC) $(CFLAGS) sets.c

clean :
	$(RM) ../c_api *.o *~

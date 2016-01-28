IDIR    = ../include
ODIR	 = obj
LDIR 	 = ../lib
CC		  = gcc
DEBUG = -g
CFLAGS  =-Wall -c $(DEBUG) -D__USE_FIXED_PROTOTYPES__ -ansi -I$(IDIR)
LFLAGS = -Wall $(DEBUG)

# SOURCES = clang_api.c prob_set.c set2.c

_DEPS  = prob_set.h
DEPS   = $(patsubst %,$(IDIR)/%,$(_DEPS))

_OBJ   = prob_set.o
OBJ    = $(patsubst %,$(ODIR)/%,$(_OBJ))


$(ODIR)/%.o: $(LDIR)/%.c $(DEPS)
		$(CC) -c -o $@ $< $(CFLAGS)

prob_set: $(OBJ)
		$(CC) -o $@ $^ $(CFLAGS)

.PHONY: clean

clean:
		\rm -f $(ODIR)/*.o *~ core $(IDIR)/*~ 

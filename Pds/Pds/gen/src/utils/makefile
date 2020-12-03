# for use on sequent

########
# Flags.
########

CFLAGS = -g

SRC = main.c \
	getline.c \
	singleCom.c \
	arcCheck.c \
	nodeCheck.c \
	mutlCheck.c \
	cordForm.c
OBJ = main.o \
	getline.o \
	singleCom.o \
	arcCheck.o \
	nodeCheck.o \
	mutlCheck.o \
	cordForm.o

#########
# The execuatble.
#########

util: $(OBJ)
	cc $(CFLAGS) -o util $(OBJ)

archive: makefile $(SRC)
	shar -v -c -b -p UTIL makefile $(SRC) > archive
	wc archive

.c.o:
	cc $(CFLAGS) -c $*.c

lint:
	lint $(SRC)


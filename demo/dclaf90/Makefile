#!Makefile
include ../../Mkinclude

LIBNM	= dclaf90

SRC	=  \
	  dcl_auto.f90
OBJ	=  \
	  dcl_auto.o
MOD	=  \
	  dcl_automatic.mod

all: lib$(LIBNM).a

lib$(LIBNM).a: $(OBJ)
	$(AR) r $@ $(OBJ)
#	cp $@ ${LIBDIR}
#	cp $(MOD) ${MODINC}

$(OBJ): %.o: %.f90
	$(DCLFC) ${FFLAGS} -c $<

clean:
	rm -rf *.o *.mod *.a

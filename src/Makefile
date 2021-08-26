# Makefile
include ../Mkinclude

STLIB	= ${STPKLIB}
STDIR	= ${STPKLD}/lib
STINC	= ${STPKLD}/include

LIBNM	= ToRMHOWe

SRC	=  \
	  sub_mod.f90 \
	  main_mod_latest.f90
OBJ	=  \
	  sub_mod.o \
	  main_mod_latest.o

MOD	=  \
	  ToRMHOWe_sub.mod \
	  ToRMHOWe_main.mod

all: lib$(LIBNM).a

lib$(LIBNM).a: $(OBJ)
	$(AR) r $@ $(OBJ)
#	cp $@ ${LIBDIR}
#	cp $(MOD) ${MODINC}

$(OBJ): %.o: %.f90
	$(FC) ${FFLAGS} -I${STINC} -c $< -L${STDIR} -l${STLIB}

clean:
	rm -rf *.o *.mod *.a


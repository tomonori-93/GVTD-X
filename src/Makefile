# Makefile
include ../Mkinclude

STLIB	= ${STPKLIB}
STDIR	= ${STPKLD}/lib
STINC	= ${STPKLD}/include

LIBNM	= ToRMHOWe

SRC	=  \
	  sub_mod.f90 \
	  main_mod_latest.f90 \
	  main_mod2.f90 \
	  gvtd_main_mod.f90
OBJ	=  \
	  sub_mod.o \
	  main_mod_latest.o \
	  main_mod2.o \
	  gvtd_main_mod.o

MOD	=  \
	  ToRMHOWe_sub.mod \
	  ToRMHOWe_main.mod \
	  ToRMHOWe_main2.mod \
	  GVTD_main.mod

all: lib$(LIBNM).a

lib$(LIBNM).a: $(OBJ)
	$(AR) r $@ $(OBJ)
#	cp $@ ${LIBDIR}
#	cp $(MOD) ${MODINC}

$(OBJ): %.o: %.f90
	$(FC) ${FFLAGS} -I${STINC} -c $< -L${STDIR} -l${STLIB}

clean:
	rm -rf *.o *.mod *.a


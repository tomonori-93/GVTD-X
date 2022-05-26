# Makefile
include ../Mkinclude

LIBNM	= ToRMHOWe

SRC	=  \
	  sub_mod.f90 \
	  main_mod.f90 \
	  main_mod2.f90 \
	  gbvtd_main_mod.f90 \
	  gvtd_main_mod.f90

OBJ	=  \
	  sub_mod.o \
	  main_mod.o \
	  main_mod2.o \
	  gbvtd_main_mod.o \
	  gvtd_main_mod.o

MOD	=  \
	  ToRMHOWe_sub.mod \
	  ToRMHOWe_main.mod \
	  ToRMHOWe_main2.mod \
	  GBVTD_main.mod \
	  GVTD_main.mod

all: lib$(LIBNM).a

lib$(LIBNM).a: $(OBJ)
	$(AR) r $@ $(OBJ)
#	cp $@ ${LIBDIR}
#	cp $(MOD) ${MODINC}

$(OBJ): %.o: %.f90
	$(FC) ${FFLAGS} -c $<

clean:
	rm -rf *.o *.mod *.a


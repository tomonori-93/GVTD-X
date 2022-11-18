# Makefile
include ../Mkinclude

LIBNM	= GVTDX

SRC	=  \
	  sub_mod.f90 \
	  gvtdx_main_mod.f90 \
	  gvtdx_main_mod2.f90 \
	  gbvtd_main_mod.f90 \
	  gvtd_main_mod.f90

OBJ	=  \
	  sub_mod.o \
	  gvtdx_main_mod.o \
	  gvtdx_main_mod2.o \
	  gbvtd_main_mod.o \
	  gvtd_main_mod.o

MOD	=  \
	  GVTDX_sub.mod \
	  GVTDX_main.mod \
	  GVTDX_main2.mod \
	  GBVTD_main.mod \
	  GVTD_main.mod

all: lib$(LIBNM).a

lib$(LIBNM).a: $(OBJ)
	$(AR) r $@ $(OBJ)

$(OBJ): %.o: %.f90
	$(FC) ${FFLAGS} -c $<

clean:
	rm -rf *.o *.mod *.a


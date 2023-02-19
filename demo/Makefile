#!Makefile
include ../Mkinclude

SRC	=  \
	  dcl_auto.f90 dcl_thermo_graph.f90
OBJ	=  \
	  dcl_auto.o dcl_thermo_graph.o
MOD	=  \
	  dcl_automatic.mod dcl_thermo_graph.mod

all: lib$(LIBNM).a

lib$(LIBNM).a: $(OBJ)
	$(AR) r $@ $(OBJ)
	cp $@ ${LIBDIR}
	cp $(MOD) ${MODINC}

$(OBJ): %.o: %.f90
	$(FC) ${FFLAGS} -I${INCSTDIR} -c $< -L${LIBSTDIR} -l$(LIBSTPK)

clean:
	rm -rf *.o *.mod *.a

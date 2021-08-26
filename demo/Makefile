# Makefile
include Mkinclude

STLIB	= ${STPKLIB}
STDIR	= ${STPKLD}/lib
STINC	= ${STPKLD}/include

DCLIB	= ${AF90LIB}
DCDIR	= ${AF90LD}/lib
DCINC	= ${AF90LD}/include

GTDIR	= ${GTHOME}/lib
GTINC	= ${GTHOME}/include

NCDIR	= ${NCHOME}/lib
NCINC	= ${NCHOME}/include

SRC	= calc_vtg2p.f90 #\
	  conv_himawari8_ceres.f90
TARGET	= calc_vtg2p #\
	  conv_himawari8_ceres

TGSRC1	=  \
	  test_Rankine.f90
TGSRC2	=  \
	  test_Rankine2.f90
TGSRC3	=  \
	  test_draw.f90
SRC1	=  \
	  sub_mod.f90 \
	  main_mod.f90
OBJ1	=  \
	  sub_mod.o \
	  main_mod.o
TARGET1	=  \
	  test_Rankine
TARGET2	=  \
	  test_Rankine2
TARGET3	=  \
	  test_draw

all: $(TARGET) $(TARGET1) $(TARGET2) $(TARGET3)
$(TARGET): %:%.f90
	$(FC) $(FFLAGS) -I${STINC} $< -o $@ -L${STDIR} -l${STLIB}

$(TARGET1): $(TGSRC1) $(OBJ1)
	$(DCLFC) $(FFLAGS) -I${STINC} -I${DCINC} $< $(OBJ1) -o $@ -L${STDIR} -l${STLIB} -L${DCDIR} -l${DCLIB}

$(TARGET2): $(TGSRC2) $(OBJ1)
	$(DCLFC) $(FFLAGS) -I${STINC} -I${DCINC} $< $(OBJ1) -o $@ -L${STDIR} -l${STLIB} -L${DCDIR} -l${DCLIB}

$(TARGET3): $(TGSRC3) $(OBJ1)
	$(DCLFC) $(FFLAGS) -I${STINC} -I${DCINC} $< $(OBJ1) -o $@ -L${STDIR} -l${STLIB} -L${DCDIR} -l${DCLIB}

$(OBJ1): %.o:%.f90
	$(DCLFC) $(FFLAGS) -c -I${STINC} -I${DCINC} $< -o $@ -L${STDIR} -l${STLIB} -L${DCDIR} -l${DCLIB}

clean:
	rm -rf $(TARGET) $(TARGET1) *.mod *.o

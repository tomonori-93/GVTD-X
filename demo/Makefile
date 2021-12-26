# Makefile
include ../Mkinclude

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

TMWDIR	= ../src
TMWINC	= ${TMWDIR}
TMWLIB	= ToRMHOWe

SRC	= calc_vtg2p.f90 #\
	  conv_himawari8_ceres.f90
TARGET	= calc_vtg2p #\
	  conv_himawari8_ceres

TGSRC1	=  \
	  test_Rankine_xy1.f90
TGSRC2	=  \
	  test_Rankine_xyL06.f90
TGSRC3	=  \
	  test_Rankine2.f90
TGSRC4	=  \
	  test_draw.f90
#SRC1	=  \
#	  sub_mod.f90 \
#	  main_mod.f90
#OBJ1	=  \
#	  sub_mod.o \
#	  main_mod.o
TARGET1	=  \
	  test_Rankine_xy1
TARGET2	=  \
	  test_Rankine_xyL06
TARGET3	=  \
	  test_Rankine2
TARGET4	=  \
	  test_draw

all: $(TARGET) $(TARGET1) $(TARGET2) $(TARGET3) $(TARGET4)
$(TARGET): %:%.f90
	$(FC) $(FFLAGS) -I${STINC} $< -o $@ -L${STDIR} -l${STLIB}

$(TARGET1): $(TGSRC1) $(OBJ1)
	$(DCLFC) $(FFLAGS) -I${STINC} -I${DCINC} -I${TMWINC} $< -o $@ -L${STDIR} -l${STLIB} -L${DCDIR} -l${DCLIB} -L${TMWDIR} -l${TMWLIB}

$(TARGET2): $(TGSRC2) $(OBJ1)
	$(DCLFC) $(FFLAGS) -I${STINC} -I${DCINC} -I${TMWINC} $< -o $@ -L${STDIR} -l${STLIB} -L${DCDIR} -l${DCLIB} -L${TMWDIR} -l${TMWLIB}

$(TARGET3): $(TGSRC3) $(OBJ1)
	$(DCLFC) $(FFLAGS) -I${STINC} -I${DCINC} -I${TMWINC} $< -o $@ -L${STDIR} -l${STLIB} -L${DCDIR} -l${DCLIB} -L${TMWDIR} -l${TMWLIB}

$(TARGET4): $(TGSRC4) $(OBJ1)
	$(DCLFC) $(FFLAGS) -I${STINC} -I${DCINC} -I${TMWINC} $< -o $@ -L${STDIR} -l${STLIB} -L${DCDIR} -l${DCLIB} -L${TMWDIR} -l${TMWLIB}

#$(OBJ1): %.o:%.f90
#	$(DCLFC) $(FFLAGS) -c -I${STINC} -I${DCINC} $< -o $@ -L${STDIR} -l${STLIB} -L${DCDIR} -l${DCLIB}

clean:
	rm -rf $(TARGET) $(TARGET1) $(TARGET2) #*.mod *.o

# Makefile
include ../Mkinclude

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

SRCD	=  \
	  test_Rankine_xy1_dcl.f90
TGSRC2	=  \
	  test_Rankine_xyL06.f90
TGSRC3	=  \
	  test_Rankine2_dcl.f90
TGSRC4	=  \
	  test_Rankine3_dcl.f90
TGSRC5	=  \
	  test_Rankine4_dcl.f90
TGSRC6	=  \
	  test_Rankine6_dcl.f90
TGSRC7	=  \
	  test_draw.f90

TARGETD	=  \
	  test_Rankine1_dcl \
	  test_Rankine2_dcl \
	  test_Rankine3_dcl \
	  test_Rankine4_dcl \
	  test_Rankine6_dcl \
TARGETN	=  \
	  test_Rankine1_nc \
	  test_Rankine2_nc \
	  test_Rankine3_nc \
	  test_Rankine4_nc \
	  test_Rankine6_nc \
TARGET7	=  \
	  test_draw

#all: $(TARGET) $(TARGET1) $(TARGET2) $(TARGET3) $(TARGET4)
all: $(TARGET1) $(TARGET3) $(TARGET4) $(TARGET5) $(TARGET6) $(TARGET7)
$(TARGET): %:%.f90
	$(FC) $(FFLAGS) $< -o $@

$(TARGET1): $(TGSRC1) $(OBJ1)
	$(DCLFC) $(FFLAGS) -I${DCINC} -I${TMWINC} $< -o $@ -L${DCDIR} -l${DCLIB} -L${TMWDIR} -l${TMWLIB}

$(TARGET2): $(TGSRC2) $(OBJ1)
	$(DCLFC) $(FFLAGS) -I${DCINC} -I${TMWINC} $< -o $@ -L${DCDIR} -l${DCLIB} -L${TMWDIR} -l${TMWLIB}

$(TARGET3): $(TGSRC3) $(OBJ1)
	$(DCLFC) $(FFLAGS) -I${DCINC} -I${TMWINC} $< -o $@ -L${DCDIR} -l${DCLIB} -L${TMWDIR} -l${TMWLIB}

$(TARGET4): $(TGSRC4) $(OBJ1)
	$(DCLFC) $(FFLAGS) -I${DCINC} -I${TMWINC} $< -o $@ -L${DCDIR} -l${DCLIB} -L${TMWDIR} -l${TMWLIB}

$(TARGET5): $(TGSRC5) $(OBJ1)
	$(DCLFC) $(FFLAGS) -I${DCINC} -I${TMWINC} $< -o $@ -L${DCDIR} -l${DCLIB} -L${TMWDIR} -l${TMWLIB}

$(TARGET6): $(TGSRC6) $(OBJ1)
	$(DCLFC) $(FFLAGS) -I${DCINC} -I${TMWINC} $< -o $@ -L${DCDIR} -l${DCLIB} -L${TMWDIR} -l${TMWLIB}

$(TARGET7): $(TGSRC7) $(OBJ1)
	$(DCLFC) $(FFLAGS) -I${DCINC} -I${TMWINC} $< -o $@ -L${DCDIR} -l${DCLIB} -L${TMWDIR} -l${TMWLIB}

#$(OBJ1): %.o:%.f90
#	$(DCLFC) $(FFLAGS) -c -I${STINC} -I${DCINC} $< -o $@ -L${STDIR} -l${STLIB} -L${DCDIR} -l${DCLIB}

clean:
	rm -rf $(TARGET) $(TARGET1) $(TARGET2) $(TARGET3) $(TARGET4) $(TARGET5) $(TARGET6) #*.mod *.o

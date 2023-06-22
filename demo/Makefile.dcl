# Makefile
include ../Mkinclude

MPIFC	= mpif90

DCDIR	= dclaf90
DCLIB	= dclaf90

GTDIR	= ${GTHOME}/lib
GTINC	= ${GTHOME}/include

NCDIR	= ${NCHOME}/lib
NCFDIR	= ${NCHOME}/lib
NCINC	= ${NCHOME}/include
NCFINC	= ${NCHOME}/include

TMWDIR	= ../src
TMWINC	= ${TMWDIR}
TMWLIB	= GVTDX

# You must independently build the following library 
MTDIR	= ./mt_mod
MTOBJ	= $(MTDIR)/f_get_coeff.o $(MTDIR)/gf2xe.o $(MTDIR)/mt_stream.o

SRC	= calc_vtg2p.f90 #\
	  conv_himawari8_ceres.f90
TARGET	= calc_vtg2p #\
	  conv_himawari8_ceres

SRCD	=  \
	  test_Rankine1_dcl.f90 \
	  test_Rankine2_dcl.f90 \
	  test_Rankine3_dcl.f90 \
	  test_Rankine4_dcl.f90 \
	  test_Rankine6_dcl.f90
#SRCN	=  \
#	  test_Rankine1_nc.f90 \
#	  test_Rankine2_nc.f90 \
#	  test_Rankine3_nc.f90 \
#	  test_Rankine4_nc.f90 \
#	  test_Rankine6_nc.f90

TGSRC7	=  \
	  test_Rankine7_dclmt.f90 \
	  test_draw.f90

TARGETD	=  \
	  test_Rankine1_dcl \
	  test_Rankine2_dcl \
	  test_Rankine3_dcl \
	  test_Rankine4_dcl \
	  test_Rankine6_dcl
#TARGETN	=  \
#	  test_Rankine1_nc \
#	  test_Rankine2_nc \
#	  test_Rankine3_nc \
#	  test_Rankine4_nc \
#	  test_Rankine6_nc
TARGET7	=  \
	  test_Rankine7_dclmt \
	  test_draw

all: $(TARGETD) $(TARGETN) $(TARGET7) $(TARGET)
use_dcl: $(TARGETD)
$(TARGETD): %:%.f90
	@cd $(DCDIR) ; \
	$(MAKE) ; \
	cd ../ ; \
	$(DCLFC) $(FFLAGS) -I${DCDIR} -I${TMWINC} $< -o $@ -L${DCDIR} -l${DCLIB} -L${TMWDIR} -l${TMWLIB}

use_dclmt: $(TARGET7)
$(TARGET7): %:%.f90
	@cd $(DCDIR) ; \
	$(MAKE) ; \
	cd ../ ; \
	$(DCLFC) $(FFLAGS) -I${DCDIR} -I${TMWINC} -I${MTDIR} $< -o $@ -L${DCDIR} -l${DCLIB} -L${TMWDIR} -l${TMWLIB} ${MTOBJ}

#use_nc: $(TARGETN)
#$(TARGETN): %:%.f90
#	$(GTFC) $(FFLAGS) -I${TMWINC} $< -o $@ -L${TMWDIR} -l${TMWLIB}

#$(OBJ1): %.o:%.f90
#	$(DCLFC) $(FFLAGS) -c -I${STINC} -I${DCINC} $< -o $@ -L${STDIR} -l${STLIB} -L${DCDIR} -l${DCLIB}

clean:
	@cd $(DCDIR) ; \
	$(MAKE) clean ; \
	cd ../ ; \
	rm -rf $(TARGET) $(TARGETD) $(TARGETN) $(TARGET7)

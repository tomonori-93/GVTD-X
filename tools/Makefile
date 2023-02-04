# Makefile
include ../Mkinclude

DCLIB	= ${AF90LIB}
DCDIR	= ${AF90LD}/lib
DCINC	= ${AF90LD}/include

GTDIR	= ${GTHOME}/lib
GTINC	= ${GTHOME}/include

NCDIR	= ${NCHOME}/lib
NCFDIR	= ${NCHOME}/lib
NCINC	= ${NCHOME}/include
NCFINC	= ${NCHOME}/include

TMWDIR	= ../src
TMWINC	= ${TMWDIR}
TMWLIB	= GVTDX

SRC	= GVTDX_Dradar.f90
TARGET	= GVTDX_Dradar

all: $(TARGET)
$(TARGET): %:%.f90
	$(FC) $(FFLAGS) -I${TMWINC} $< -o $@ -L${TMWDIR} -l${TMWLIB}

clean:
	rm -rf $(TARGET)

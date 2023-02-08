# Makefile
include ../Mkinclude

TMWDIR	= ../src
TMWINC	= ${TMWDIR}
TMWLIB	= GVTDX

SRC	= test_Rankine_ax.f90
TARGET	= test_Rankine_ax

all: $(TARGET)
$(TARGET): %:%.f90
	$(FC) $(FFLAGS) -I${TMWINC} $< -o $@ -L${TMWDIR} -l${TMWLIB}

clean:
	rm -rf $(TARGET)

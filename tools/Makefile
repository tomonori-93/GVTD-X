# Makefile
include ../Mkinclude

TMWDIR	= ../src
TMWINC	= ${TMWDIR}
TMWLIB	= GVTDX

SRC	= GVTDX_Dradar.f90 GVTDX_budgets.f90
TARGET	= GVTDX_Dradar GVTDX_budgets

all: $(TARGET)
$(TARGET): %:%.f90
	$(FC) $(FFLAGS) -I${TMWINC} $< -o $@ -L${TMWDIR} -l${TMWLIB}

clean:
	rm -rf $(TARGET)

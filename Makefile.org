# Makefile

include ./Mkinclude

SUBDIR1	= src demo

all:
	@for i in $(SUBDIR1) ; do \
	  cd $$i ; \
	  $(MAKE) ; \
	  cd ../ ; \
	done

clean:
	@for i in $(SUBDIR1) ; do \
	  cd $$i ; \
	  $(MAKE) clean; \
	  cd ../ ; \
	done

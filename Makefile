# GNU Makefile template for user ESMF application

################################################################################
################################################################################
## This Makefile must be able to find the "esmf.mk" Makefile fragment in the  ##
## 'include' line below. Following the ESMF User's Guide, a complete ESMF     ##
## installation should ensure that a single environment variable "ESMFMKFILE" ##
## is made available on the system. This variable should point to the         ##
## "esmf.mk" file.                                                            ##
##                                                                            ##
## This example Makefile uses the "ESMFMKFILE" environment variable.          ##
##                                                                            ##
## If you notice that this Makefile cannot find variable ESMFMKFILE then      ##
## please contact the person responsible for the ESMF installation on your    ##
## system.                                                                    ##
## As a work-around you can simply hardcode the path to "esmf.mk" in the      ##
## include line below. However, doing so will render this Makefile a lot less ##
## flexible and non-portable.                                                 ##
################################################################################

ifneq ($(origin ESMFMKFILE), environment)
$(error Environment variable ESMFMKFILE was not set.)
endif

include $(ESMFMKFILE)

target  = libdatapoll.a
objects = datapollCap.o
mkfile  = datapoll.mk

LOCAL_FCFLAGS = -O2 -g

################################################################################

.SUFFIXES: .F90

%.o : %.F90
	$(ESMF_F90COMPILER) -c $(LOCAL_FCFLAGS) $(ESMF_F90COMPILEPATHS) $<
        
# -----------------------------------------------------------------------------

.PHONY: all clean default distclean info install nuopcinstall

default: all

distclean: clean

install: nuopcinstall

info:
	@echo ==================================================================
	@echo ESMFMKFILE=$(ESMFMKFILE)
	@echo ==================================================================
	@cat $(ESMFMKFILE)
	@echo ==================================================================

# DO NOT DELETE
#
# ------------------------------------------------------------------------------
# --- NUOPC additions ----------------------------------------------------------

$(mkfile) : $(target)
	@echo "# ESMF self-describing build dependency makefile fragment" > $@
	@echo >> $@
	@echo "ESMF_DEP_FRONT     = DATAPOLL"               >> $@
	@echo "ESMF_DEP_INCPATH   = `pwd`"                  >> $@
	@echo "ESMF_DEP_CMPL_OBJS = `pwd`/"$<               >> $@
	@echo "ESMF_DEP_LINK_OBJS = `pwd`/$(target)"        >> $@
	@echo "ESMF_DEP_SHRD_PATH = "                       >> $@
	@echo "ESMF_DEP_SHRD_LIBS = "                       >> $@
	@echo
	@echo "Finished generating ESMF self-describing build dependency makefile fragment:" $@
	@echo

$(target): $(objects)
	ar cr $@ *.o

all: $(target)

clean:
	rm -f *.o *.mod *.a *.mk

PWDIR := `pwd`

ifndef DESTDIR
DESTDIR := $(PWDIR)
endif

INSTDATE := $(shell date '+%Y-%m-%d-%H-%M-%S')
ifndef INSTDIR
INSTDIR  := DATAPOLL_$(INSTDATE)
endif

nuopcinstall: $(mkfile)
	@mkdir -p $(DESTDIR)/$(INSTDIR)
	@cp $(target) *.mod $(DESTDIR)/$(INSTDIR)
	@sed -e 's;'$(PWDIR)';'$(DESTDIR)/$(INSTDIR)';g' $(mkfile) > $(DESTDIR)/$(INSTDIR)/$(mkfile)
	@echo Installation into \"$(DESTDIR)/$(INSTDIR)\" complete!
	@echo
# ------------------------------------------------------------------------------
#

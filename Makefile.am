SUBDIRS = src

dist_doc_DATA = README.md

nodist_include_HEADERS = src/datapoll.$(FC_MODEXT)
nodist_data_DATA = datapoll.mk

ACLOCAL_AMFLAGS = -I m4

datapoll.mk:
	@echo "# ESMF self-describing build dependency makefile fragment" > $(@)
	@echo >> $(@)
	@echo "ESMF_DEP_FRONT     = datapoll"                      >> $(@)
	@echo "ESMF_DEP_INCPATH   = $(includedir)"                 >> $(@)
	@echo "ESMF_DEP_CMPL_OBJS = "                              >> $(@)
	@echo "ESMF_DEP_LINK_OBJS = $(DATAPOLL_DEP_LINK_OBJS)"     >> $(@)
	@echo "ESMF_DEP_SHRD_PATH = $(DATAPOLL_DEP_SHRD_PATH)"     >> $(@)
	@echo "ESMF_DEP_SHRD_LIBS = $(DATAPOLL_DEP_SHRD_LIBS)"     >> $(@)
	@echo
	@echo "Finished generating ESMF self-describing build dependency makefile fragment:" $(@)
	@echo

uninstall-local:
	-rm -f $(datarootdir)/datapoll.mk

CLEANFILES = datapoll.mk

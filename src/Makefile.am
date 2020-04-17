lib_LIBRARIES = libdatapoll.a

libdatapoll_a_SOURCES = datapollCap.F90
libdatapoll_a_FCFLAGS =

.PHONY: clean-modules

clean-modules:
	-test -z "$(FC_MODEXT)" || rm -f *.$(FC_MODEXT)

clean-local: clean-modules

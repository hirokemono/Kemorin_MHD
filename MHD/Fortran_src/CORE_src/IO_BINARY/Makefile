#
#
#

BINARY_IO_DIR = $$(CORE_SRCDIR)/IO_BINARY
SOURCES = $(shell ls *.f90 *.F90)
MOD_BINARY_IO = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'BINARY_IO_DIR = $(BINARY_IO_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_BINARY_IO)' >> $(MAKENAME)

mod_list:
	@echo  MOD_BINARY_IO=  \\ >> $(MAKENAME)
	@echo  $(MOD_BINARY_IO)   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(BINARY_IO_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

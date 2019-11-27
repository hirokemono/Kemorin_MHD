#
#
#

BINARY_IO_DIR = $$(CORE_SRCDIR)/IO_BINARY
SRC_BINARY_IO = $(shell ls *.f90 *.F90)
MOD_BINARY_IO = $(addsuffix .o,$(basename $(SRC_BINARY_IO)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'BINARY_IO_DIR = $(BINARY_IO_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_BINARY_IO)' >> $(MAKENAME)

mod_list:
	@echo  MOD_BINARY_IO=  \\ >> $(MAKENAME)
	@echo  $(MOD_BINARY_IO)   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(BINARY_IO_DIR)' $(SRC_BINARY_IO)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

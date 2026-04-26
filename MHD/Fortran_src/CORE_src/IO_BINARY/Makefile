#
#
#

BINARY_IO_DIR = $$(CORE_SRCDIR)/IO_BINARY
SOURCES = $(shell ls *.f90 *.F90)

MOD_BINARY_IO = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_BINARY_IO'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'BINARY_IO_DIR = $(BINARY_IO_DIR)'  >> $(MAKENAME)

lib_name:

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_BINARY_IO)'     >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(BINARY_IO_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

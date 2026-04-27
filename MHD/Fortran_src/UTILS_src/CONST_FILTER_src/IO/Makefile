#
#
#

FILTER_3D_IODIR =  $$(FILTER_SRCDIR)/IO
SOURCES = $(shell ls *.f90)

MOD_GEN_FILTER_IO = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_GEN_FILTER_IO'
UPPER_MODULES=

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'FILTER_3D_IODIR =  $(FILTER_3D_IODIR)'          >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_GEN_FILTER_IO)' >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(FILTER_3D_IODIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

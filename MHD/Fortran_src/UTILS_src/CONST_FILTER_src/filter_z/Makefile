#
#
#

FILTER_Z_MATDIR = $$(FILTER_SRCDIR)/filter_z
SOURCES = $(shell ls *.f90)

MOD_Z_FILTER_MAT = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_Z_FILTER_MAT'
UPPER_MODULES=

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'FILTER_Z_MATDIR = $(FILTER_Z_MATDIR)'   >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_Z_FILTER_MAT)' >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(FILTER_Z_MATDIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

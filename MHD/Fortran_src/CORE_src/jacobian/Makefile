#
#
#

JACOBIDIR = $$(CORE_SRCDIR)/jacobian

SOURCES = $(shell ls *.f90)

MOD_JACOBI = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_JACOBI'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'JACOBIDIR = $(JACOBIDIR)'  >> $(MAKENAME)

lib_name:

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_JACOBI)'        >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(JACOBIDIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

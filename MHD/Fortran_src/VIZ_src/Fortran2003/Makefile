#
#
#

VIZ_F2003_DIR = $$(VIZ_SRCDIR)/Fortran2003
SOURCES = $(shell ls *.f90)

MOD_VIZ_F2003 = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_VIZ_F2003'
UPPER_MODULES=

#
#  --------------------------------------------------------------------
#

dir_list:
	@echo 'VIZ_F2003_DIR = $(VIZ_F2003_DIR)' >> $(MAKENAME)

lib_name:

lib_tasks: lib_archve
	@echo '	''$$(RANLIB) $$@' >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)



mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_VIZ_F2003)'     >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(VIZ_F2003_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

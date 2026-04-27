#
#
#

MHD_SOLVER_DIR = $$(MHD_SRCDIR)/solver_vector_smp
SOURCES = $(shell ls *.f90)

MOD_MHD_SOLVE = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_MHD_SOLVE'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_SOLVER_DIR = $(MHD_SOLVER_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)

mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_MHD_SOLVE)'   >> $(MAKENAME)
	@echo  '#'>> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(MHD_SOLVER_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a
	rm -f */*.o */*.mod */*~ */*.par */*.diag */*.a

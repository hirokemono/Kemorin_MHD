#
#
#

DJDS_ORDER_DIR = $$(PARA_SRCDIR)/ORDERING_4_DJDS
SOURCES = $(shell ls *.f90)

MOD_SOLVER_SET_ORDER = $(addsuffix .o,$(basename $(SOURCES)) )
MODULE_LIST=   'MOD_SOLVER_SET_ORDER'
UPPER_MODULES=

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'DJDS_ORDER_DIR = $(DJDS_ORDER_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$('$(MODULE_LIST)')' >> $(MAKENAME)


mod_list:
	@echo  '$(UPPER_MODULES)+= $$('$(MODULE_LIST)')' >> $(MAKENAME)
	@echo  '$(MODULE_LIST)=  ' \\ >> $(MAKENAME)
	@echo  '$(MOD_SOLVER_SET_ORDER)'     >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)
	@echo >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(DJDS_ORDER_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

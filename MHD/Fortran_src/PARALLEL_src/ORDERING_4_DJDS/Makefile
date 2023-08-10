#
#
#

DJDS_ORDER_DIR = $$(PARA_SRCDIR)/ORDERING_4_DJDS
SOURCES = $(shell ls *.f90)
MOD_SOLVER_SET_ORDER = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'DJDS_ORDER_DIR = $(DJDS_ORDER_DIR)'  >> $(MAKENAME)

libtarget:

lib_archve: libtarget
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_SOLVER_SET_ORDER)' >> $(MAKENAME)


mod_list:
	@echo MOD_SOLVER_SET_ORDER= \\  >> $(MAKENAME)
	@echo $(MOD_SOLVER_SET_ORDER) >> $(MAKENAME)
	@echo >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(DJDS_ORDER_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

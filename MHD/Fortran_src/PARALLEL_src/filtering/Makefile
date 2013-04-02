#
#
#

FILTER_DIR = $$(PARA_SRCDIR)/filtering
SRC_FILTERING = $(shell ls *.f90)
MOD_FILTERING = $(addsuffix .o,$(basename $(SRC_FILTERING)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'FILTER_DIR = $(FILTER_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_FILTERING)' >> $(MAKENAME)


mod_list:
	@echo MOD_FILTERING=  \\ >> $(MAKENAME)
	@echo $(MOD_FILTERING) >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(FILTER_DIR)' $(SRC_FILTERING)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

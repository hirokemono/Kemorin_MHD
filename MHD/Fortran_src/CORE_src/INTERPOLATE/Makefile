#
#
#

INTERPOLATE_DIR = $$(CORE_SRCDIR)/INTERPOLATE
SRC_ITP = $(shell ls *.f90)
MOD_ITP = $(addsuffix .o,$(basename $(SRC_ITP)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'INTERPOLATE_DIR = $(INTERPOLATE_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_ITP)' >> $(MAKENAME)

mod_list:
	@echo  MOD_ITP=  \\ >> $(MAKENAME)
	@echo  $(MOD_ITP) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(INTERPOLATE_DIR)' $(SRC_ITP)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

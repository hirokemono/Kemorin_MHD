#
#
#

MHD_SGS_DYNAMIC_DIR = $$(MHD_SRCDIR)/sgs_dynamic
SRC_SGS_DYNAMIC = $(shell ls *.f90)
MOD_SGS_DYNAMIC = $(addsuffix .o,$(basename $(SRC_SGS_DYNAMIC)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_SGS_DYNAMIC_DIR = $(MHD_SGS_DYNAMIC_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	''$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_SGS_DYNAMIC)' \
	>> $(MAKENAME)

mod_list:
	@echo MOD_SGS_DYNAMIC=  \\ >> $(MAKENAME)
	@echo $(MOD_SGS_DYNAMIC) >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MHD_SGS_DYNAMIC_DIR)' $(SRC_SGS_DYNAMIC)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

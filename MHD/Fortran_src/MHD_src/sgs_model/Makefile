#
#
#

MHD_SGS_DIR = $$(MHD_SRCDIR)/sgs_model
SRC_SGS = $(shell ls *.f90)
MOD_SGS = $(addsuffix .o,$(basename $(SRC_SGS)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_SGS_DIR = $(MHD_SGS_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	''$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_SGS)' \
	>> $(MAKENAME)

mod_list:
	@echo MOD_SGS=  \\ >> $(MAKENAME)
	@echo $(MOD_SGS) >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MHD_SGS_DIR)' $(SRC_SGS)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

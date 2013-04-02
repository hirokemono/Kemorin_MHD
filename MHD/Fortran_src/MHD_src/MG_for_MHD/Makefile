#
#
#

MHD_AMG_DIR = $$(MHD_SRCDIR)/MG_for_MHD
SRC_MG_4_MHD = $(shell ls *.f90)
MOD_MG_4_MHD = $(addsuffix .o,$(basename $(SRC_MG_4_MHD)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_AMG_DIR = $(MHD_AMG_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_MG_4_MHD)' \
	>> $(MAKENAME)

mod_list:
	@echo MOD_MG_4_MHD=  \\ >> $(MAKENAME)
	@echo $(MOD_MG_4_MHD)   >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MHD_AMG_DIR)' $(SRC_MG_4_MHD)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

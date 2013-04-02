#
#
#

MHD_INI_DIR = $$(MHD_SRCDIR)/initial_data
SRC_KEMO = $(shell ls *.f90)
MOD_KEMO = $(addsuffix .o,$(basename $(SRC_KEMO)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_INI_DIR = $(MHD_INI_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_KEMO)' \
	>> $(MAKENAME)

mod_list:
	@echo MOD_KEMO=  \\ >> $(MAKENAME)
	@echo $(MOD_KEMO) >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MHD_INI_DIR)' $(SRC_KEMO)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

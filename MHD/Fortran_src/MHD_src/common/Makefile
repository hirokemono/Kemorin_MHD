#
#
#

MHD_COMMON_DIR = $$(MHD_SRCDIR)/common
SRC_COMMON = $(shell ls *.f90)
MOD_COMMON = $(addsuffix .o,$(basename $(SRC_COMMON)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_COMMON_DIR = $(MHD_COMMON_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_COMMON)' \
	>> $(MAKENAME)

mod_list:
	@echo  MOD_COMMON=  \\ >> $(MAKENAME)
	@echo  $(MOD_COMMON) >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MHD_COMMON_DIR)' $(SRC_COMMON)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

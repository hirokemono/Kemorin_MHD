#
#
#

MHD_SF_BC_DIR = $$(MHD_SRCDIR)/surface_boundary
SRC_SF_BD = $(shell ls *.f90)
MOD_SF_BD = $(addsuffix .o,$(basename $(SRC_SF_BD)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_SF_BC_DIR = $(MHD_SF_BC_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_SF_BD)' \
	>> $(MAKENAME)

mod_list:
	@echo MOD_SF_BD=  \\ >> $(MAKENAME)
	@echo $(MOD_SF_BD) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MHD_SF_BC_DIR)' $(SRC_SF_BD)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

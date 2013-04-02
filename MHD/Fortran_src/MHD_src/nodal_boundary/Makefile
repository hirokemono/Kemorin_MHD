#
#
#

MHD_NOD_BC_DIR = $$(MHD_SRCDIR)/nodal_boundary
SRC_NOD_BD = $(shell ls *.f90)
MOD_NOD_BD = $(addsuffix .o,$(basename $(SRC_NOD_BD)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MHD_NOD_BC_DIR = $(MHD_NOD_BC_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	''$$(AR)' '$$(ARFLUGS)' rcsv '$$@' '$$(MOD_NOD_BD)'  \
	>> $(MAKENAME)

mod_list:
	@echo MOD_NOD_BD=  \\ >> $(MAKENAME)
	@echo $(MOD_NOD_BD) >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MHD_NOD_BC_DIR)' $(SRC_NOD_BD)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

#
#
#

MESH_1_DIR = $$(PARA_SRCDIR)/MESH_1st
SRC_MESH1 = $(shell ls *.f90 *.F90)
MOD_MESH1 = $(addsuffix .o,$(basename $(SRC_MESH1)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MESH_1_DIR = $(MESH_1_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_MESH1)' >> $(MAKENAME)


mod_list:
	@echo  MOD_MESH1=  \\ >> $(MAKENAME)
	@echo  $(MOD_MESH1) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MESH_1_DIR)' $(SRC_MESH1)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

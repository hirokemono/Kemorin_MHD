#
#
#

MESH_2_DIR = $$(CORE_SRCDIR)/MESH_2nd
SRC_MESH2 = $(shell ls *.f90)
MOD_MESH2 = $(addsuffix .o,$(basename $(SRC_MESH2)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MESH_2_DIR = $(MESH_2_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_MESH2)' >> $(MAKENAME)

mod_list:
	@echo  MOD_MESH2=  \\ >> $(MAKENAME)
	@echo  $(MOD_MESH2)   >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MESH_2_DIR)' $(SRC_MESH2)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

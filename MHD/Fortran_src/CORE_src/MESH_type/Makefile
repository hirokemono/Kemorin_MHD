#
#
#

MESH_TYPE_DIR = $$(CORE_SRCDIR)/MESH_type
SRC_MESH_T = $(shell ls *.f90)
MOD_MESH_T = $(addsuffix .o,$(basename $(SRC_MESH_T)) )


#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'MESH_TYPE_DIR = $(MESH_TYPE_DIR)'  >> $(MAKENAME)

lib_archve:
	@echo '	$$(AR) $$(ARFLUGS) rcsv $$@ $$(MOD_MESH_T)' >> $(MAKENAME)


mod_list:
	@echo  MOD_MESH_T=  \\ >> $(MAKENAME)
	@echo  $(MOD_MESH_T) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MESH_TYPE_DIR)' $(SRC_MESH_T)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

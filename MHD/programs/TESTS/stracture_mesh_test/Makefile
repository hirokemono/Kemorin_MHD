#
#
#

MESH_TEST_T_DIR = $$(TESTS_DIR)/stracture_mesh_test

TARGET_T_MESH_TEST = mesh_type_test

SRC_T_MESH_TEST = $(shell ls *.f90)
MOD_T_MESH_TEST = $(addsuffix .o,$(basename $(SRC_MESH1)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'MESH_TEST_T_DIR = $(MESH_TEST_T_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_T_MESH_TEST = $$(BUILDDIR)/$(TARGET_T_MESH_TEST)' \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_T_MESH_TEST): $$(MOD_T_MESH_TEST) $$(LIB_MESH_FILE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_T_MESH_TEST)' \
	'$$(MOD_T_MESH_TEST)' \
	'-L. $$(LIB_MESH)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_T_MESH_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_T_MESH_TEST) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(MESH_TEST_T_DIR)' $(SRC_T_MESH_TEST)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

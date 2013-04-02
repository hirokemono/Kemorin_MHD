#
#
#

SMDT_POLY_TEST_DIR = $$(TESTS_DIR)/test_schmidt_poly

TARGET_SMDT_POLY = schmidt_poly_test
TARGET_SPH_TRIINT =    gaunt_all

SOURCES = $(shell ls *.f90)

MOD_SMDT_POLY_TEST = \
test_schmidt_poly_med.o \
schmidt_poly_on_meridian.o

MOD_SPH_TRIINT = \
giroop.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'SMDT_POLY_TEST_DIR = $(SMDT_POLY_TEST_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_SMDT_POLY = $$(BUILDDIR)/$(TARGET_SMDT_POLY)' \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo lag_tests: \
	'$$(TARGET_SMDT_POLY) $$(TARGET_SPH_TRIINT)' \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SMDT_POLY)': '$$(MOD_SMDT_POLY_TEST)' \
	'$$(LIB_MESH_FILE)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SMDT_POLY)' \
	'$$(MOD_SMDT_POLY_TEST)  -L. $$(LIB_MESH)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_TRIINT):' \
	'$$(MOD_SPH_TRIINT) $$(LIB_FILES_SPH_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SPH_TRIINT)' \
	'$$(MOD_SPH_TRIINT)' \
	'-L. $$(LIBS_SPH_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_SMDT_POLY_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_SMDT_POLY_TEST) >> $(MAKENAME)
	@echo  MOD_SPH_TRIINT=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_TRIINT) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(SMDT_POLY_TEST_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

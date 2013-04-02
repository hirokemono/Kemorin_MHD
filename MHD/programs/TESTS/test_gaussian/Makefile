#
#
#

GAUSS_TEST_DIR =     $$(TESTS_DIR)/test_gaussian

TARGET_DIGIT_TEST =    digit_test
TARGET_GAUSS_TEST =    gauss_points

SOURCES = $(shell ls *.f90)

MOD_DIGIT_TEST = \
digit_test.o

MOD_GAUSS_TEST = \
gaussian_points.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'GAUSS_TEST_DIR = $(GAUSS_TEST_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_GAUSS_TEST =    $$(BUILDDIR)/$(TARGET_GAUSS_TEST)'   \
	>> $(MAKENAME)
	@echo 'TARGET_DIGIT_TEST =    $$(BUILDDIR)/$(TARGET_DIGIT_TEST)'   \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo gauss_tests:  \
	'$$(TARGET_GAUSS_TEST)      $$(TARGET_DIGIT_TEST) '     \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_DIGIT_TEST)': '$$(MOD_DIGIT_TEST)' \
	'$$(LIB_MESH_FILE)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_DIGIT_TEST)' \
	'$$(MOD_DIGIT_TEST)  -L. $$(LIB_MESH)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '$$(TARGET_GAUSS_TEST)': '$$(MOD_GAUSS_TEST)' \
	'$$(LIB_MESH_FILE)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_GAUSS_TEST)' \
	'$$(MOD_GAUSS_TEST) -L. $$(LIB_MESH) $$(F90LIBS)'  >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_DIGIT_TEST=    \\  >> $(MAKENAME)
	@echo  $(MOD_DIGIT_TEST)      >> $(MAKENAME)
	@echo  MOD_GAUSS_TEST=    \\  >> $(MAKENAME)
	@echo  $(MOD_GAUSS_TEST)      >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(GAUSS_TEST_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

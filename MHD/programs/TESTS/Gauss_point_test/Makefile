#
#
#

GAUSS_TEST_DIR =     $$(TESTS_DIR)/Gauss_point_test

TARGET_DIGIT_TEST =     digit_test
TARGET_GAUSS_TEST =     gauss_points
TARGET_CMP_GAUSS_TEST = compare_gauss_points

CURRENTDIR = $(shell pwd)
SOURCES = $(shell ls *.f90)

MOD_DIGIT_TEST = \
digit_test.o

MOD_GAUSS_TEST = \
test_gauss_points.o \
gauss_points_IO.o 

MOD_CMP_GAUSS_TEST = \
compare_gauss_points.o \
gauss_points_IO.o 


#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'GAUSS_TEST_DIR = $(GAUSS_TEST_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_GAUSS_TEST =    $$(TEST_BUILD_DIR)/$(TARGET_GAUSS_TEST)'   \
	>> $(MAKENAME)
	@echo 'TARGET_DIGIT_TEST =    $$(TEST_BUILD_DIR)/$(TARGET_DIGIT_TEST)'   \
	>> $(MAKENAME)
	@echo 'TARGET_CMP_GAUSS_TEST = $$(TEST_BUILD_DIR)/$(TARGET_CMP_GAUSS_TEST)'   \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo gauss_tests:  \
	'$$(TARGET_GAUSS_TEST)       $$(TARGET_DIGIT_TEST) '     \
	'$$(TARGET_CMP_GAUSS_TEST)'  >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_DIGIT_TEST)': '$$(MOD_DIGIT_TEST)' \
	'$$(LIB_FILES_MESH_TEST)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_DIGIT_TEST) $$(MOD_DIGIT_TEST) ' \
	'-L. $$(LIBS_MESH_TEST) $$(F90LIBS)' >> $(MAKENAME)
	@echo '$$(TARGET_GAUSS_TEST)': '$$(MOD_GAUSS_TEST)' \
	'$$(LIB_FILES_MESH_TEST)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	'-o $$(TARGET_GAUSS_TEST)  $$(MOD_GAUSS_TEST)' \
	'-L. $$(LIBS_MESH_TEST) $$(F90LIBS)'  >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_CMP_GAUSS_TEST)': '$$(MOD_CMP_GAUSS_TEST)' \
	'$$(LIB_FILES_MESH_TEST)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	'-o $$(TARGET_CMP_GAUSS_TEST)  $$(MOD_CMP_GAUSS_TEST)' \
	'-L. $$(LIBS_MESH_TEST) $$(F90LIBS)'  >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_DIGIT_TEST=    \\  >> $(MAKENAME)
	@echo  $(MOD_DIGIT_TEST)      >> $(MAKENAME)
	@echo  MOD_GAUSS_TEST=    \\  >> $(MAKENAME)
	@echo  $(MOD_GAUSS_TEST)      >> $(MAKENAME)
	@echo  MOD_CMP_GAUSS_TEST=    \\  >> $(MAKENAME)
	@echo  $(MOD_CMP_GAUSS_TEST)      >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(GAUSS_TEST_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

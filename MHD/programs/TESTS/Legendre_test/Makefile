#
#
#

SMDT_POLY_TEST_DIR = $$(TESTS_DIR)/Legendre_test

TARGET_SMDT_POLY =    test_schmidt_Legendre
TARGET_MAG_SMDT =     test_mag_schmidt_legendre
TARGET_MAG16_SMDT =   test_mag16_schmidt_legendre
TARGET_ACDC_SMDT =    test_rayleigh_schmidt_legendre
TARGET_SPH_TRIINT =   gaunt_all

CURRENTDIR = $(shell pwd)
SOURCES = $(shell ls *.f90)

MOD_SMDT_POLY_TEST = \
test_schmidt_poly_med.o \
schmidt_poly_on_meridian.o

MOD_MAG_SMDT = \
test_legendre_by_MAG.o

MOD_MAG16_SMDT = \
test_legendre_by_MAG16.o

MOD_ACDC_SMDT = \
test_legendre_by_ACDC.o

MOD_SPH_TRIINT = \
giroop.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'SMDT_POLY_TEST_DIR = $(SMDT_POLY_TEST_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_SMDT_POLY =   $$(TEST_BUILD_DIR)/$(TARGET_SMDT_POLY)'  >> $(MAKENAME)
	@echo 'TARGET_MAG_SMDT =    $$(TEST_BUILD_DIR)/$(TARGET_MAG_SMDT)'  >> $(MAKENAME)
	@echo 'TARGET_ACDC_SMDT =   $$(TEST_BUILD_DIR)/$(TARGET_ACDC_SMDT)'  >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo leg_tests: \
	'$$(TARGET_SMDT_POLY) $$(TARGET_SPH_TRIINT)' \
    '$$(TARGET_MAG_SMDT)  $$(TARGET_ACDC_SMDT)' \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SMDT_POLY)': '$$(MOD_SMDT_POLY_TEST)' \
	'$$(LIB_FILES_MESH_TEST)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SMDT_POLY)' \
	'$$(MOD_SMDT_POLY_TEST)  -L. $$(LIBS_MESH_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SPH_TRIINT):' \
	'$$(MOD_SPH_TRIINT) $$(LIB_FILES_MESH_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SPH_TRIINT)' \
	'$$(MOD_SPH_TRIINT)' \
	'-L. $$(LIBS_MESH_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_MAG_SMDT):' \
	'$$(MOD_MAG_SMDT) $$(LIB_FILES_MESH_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_MAG_SMDT)' \
	'$$(MOD_MAG_SMDT)' \
	'-L. $$(LIBS_MESH_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_ACDC_SMDT):' \
	'$$(MOD_ACDC_SMDT) $$(LIB_FILES_MESH_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_ACDC_SMDT)' \
	'$$(MOD_ACDC_SMDT)' \
	'-L. $$(LIBS_MESH_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_SMDT_POLY_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_SMDT_POLY_TEST) >> $(MAKENAME)
	@echo  MOD_SPH_TRIINT=  \\ >> $(MAKENAME)
	@echo  $(MOD_SPH_TRIINT) >> $(MAKENAME)
	@echo  MOD_MAG_SMDT=  \\ >> $(MAKENAME)
	@echo  $(MOD_MAG_SMDT) >> $(MAKENAME)
	@echo  MOD_MAG16_SMDT=  \\ >> $(MAKENAME)
	@echo  $(MOD_MAG16_SMDT) >> $(MAKENAME)
	@echo  MOD_ACDC_SMDT=  \\ >> $(MAKENAME)
	@echo  $(MOD_ACDC_SMDT) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(SMDT_POLY_TEST_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

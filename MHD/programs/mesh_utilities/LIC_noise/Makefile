#
#
#

LIC_NOISE_DIR =  $$(MESHER_DIR)/LIC_noise

TARGET_GEN_CUBE_NOISE =      gen_cube_noise
TARGET_MAKE_NOISE_2_VTK =    cube_noise_to_VTK
TARGET_TEST_OLD_NOISE =      old_noise_to_VTK

SOURCES = $(shell ls *.f90)

MOD_GEN_CUBE_NOISE = \
noise_on_cube.o

MOD_MAKE_NOISE_2_VTK = \
noise_to_VTK.o

MOD_TEST_OLD_NOISE = \
t_lic_noise_generator.o \
test_old_noise.o

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'LIC_NOISE_DIR = $(LIC_NOISE_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_GEN_CUBE_NOISE =     $$(BUILDDIR)/$(TARGET_GEN_CUBE_NOISE)'   \
	>> $(MAKENAME)
	@echo 'TARGET_MAKE_NOISE_2_VTK = $$(BUILDDIR)/$(TARGET_MAKE_NOISE_2_VTK)' \
	>> $(MAKENAME)
	@echo 'TARGET_TEST_OLD_NOISE = $$(BUILDDIR)/$(TARGET_TEST_OLD_NOISE)'     \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo noise_utils:   \
	'$$(TARGET_GEN_CUBE_NOISE)   $$(TARGET_MAKE_NOISE_2_VTK)'   \
	'$$(TARGET_TEST_OLD_NOISE)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_GEN_CUBE_NOISE)': '$$(MOD_GEN_CUBE_NOISE)' \
	'$$(LIB_FILES_CUBE_NOISE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_GEN_CUBE_NOISE)' \
	'$$(MOD_GEN_CUBE_NOISE)' \
	'-L. $$(LIBS_CUBE_NOISE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_MAKE_NOISE_2_VTK): $$(MOD_MAKE_NOISE_2_VTK)' \
	'$$(LIB_FILES_CUBE_NOISE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_MAKE_NOISE_2_VTK)' \
	'$$(MOD_MAKE_NOISE_2_VTK)' \
	'-L. $$(LIBS_CUBE_NOISE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_TEST_OLD_NOISE):' \
	'$$(MOD_TEST_OLD_NOISE) $$(LIB_FILES_CUBE_NOISE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_TEST_OLD_NOISE)' \
	'$$(MOD_TEST_OLD_NOISE)' \
	'-L. $$(LIBS_CUBE_NOISE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_GEN_CUBE_NOISE=  \\ >> $(MAKENAME)
	@echo  $(MOD_GEN_CUBE_NOISE) >> $(MAKENAME)
	@echo  MOD_MAKE_NOISE_2_VTK=  \\ >> $(MAKENAME)
	@echo  $(MOD_MAKE_NOISE_2_VTK) >> $(MAKENAME)
	@echo  MOD_TEST_OLD_NOISE=  \\ >> $(MAKENAME)
	@echo  $(MOD_TEST_OLD_NOISE) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(LIC_NOISE_DIR)' $(SOURCES)

#
#
#

ITP_TEST_DIR =  $$(ITP_PLUG_DIR)/test_interpolate

TARGET_ITP_TEST =     interpolate_test
TARGET_ITP_ELE_TEST = interpolate_ele_test

SOURCES = $(shell ls *.f90)

MOD_ITP_MESH = \
main_test_itp_tbl.o \
analyzer_test_table.o \
\
check_ineterppolated_mesh.o \
input_control_itp_mesh.o \
mesh_interpolation.o \
interpolate_position.o


MOD_ITP_ELE_MESH = \
main_test_itp_ele.o \
analyzer_test_ele_tbl.o \
\
check_ineterppolated_mesh.o \
input_control_itp_mesh.o \
mesh_interpolation.o \
interpolate_position.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'ITP_TEST_DIR =  $(ITP_TEST_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_ITP_TEST =     $$(BUILDDIR)/$(TARGET_ITP_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_ITP_ELE_TEST = $$(BUILDDIR)/$(TARGET_ITP_ELE_TEST)' \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_ITP_TEST)': \
	'$$(MOD_ITP_MESH)' '$$(LIB_FILES_INTERPOLATE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_ITP_TEST)' \
	'$$(MOD_ITP_MESH)' \
	'-L. $$(LIBS_INTERPOLATE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	
	@echo '$$(TARGET_ITP_ELE_TEST)': \
	'$$(MOD_ITP_ELE_MESH)' '$$(LIB_FILES_INTERPOLATE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_ITP_ELE_TEST)' \
	'$$(MOD_ITP_ELE_MESH)' \
	'-L. $$(LIBS_INTERPOLATE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_ITP_MESH=  \\ >> $(MAKENAME)
	@echo  $(MOD_ITP_MESH) >> $(MAKENAME)
	@echo  MOD_ITP_ELE_MESH=  \\ >> $(MAKENAME)
	@echo  $(MOD_ITP_ELE_MESH) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(ITP_TEST_DIR)' $(SOURCES)

#
#
#

ITP_FIELD_DIR = $$(ITP_PLUG_DIR)/interpolate

TARGET_ITP_UDT = interpolate_udt
TARGET_ITP_RST = interpolate_rst

TARGET_ITP_TEST =     interpolate_test
TARGET_ITP_ELE_TEST = interpolate_ele_test

SOURCES = $(shell ls *.f90)

MOD_ITP_FIELDS = \
t_structure_4_interolation.o \
analyzer_interpolate_udt.o \
main_interpolate_udt.o

MOD_ITP_RESTART = \
t_structure_4_interolation.o \
analyzer_interpolate_rst.o \
main_interpolate_rst.o


MOD_ITP_MESH = \
main_test_itp_tbl.o \
analyzer_test_table.o \
\
check_ineterppolated_mesh.o \
input_control_interpolate.o \
mesh_interpolation.o \
interpolate_position.o

MOD_ITP_ELE_MESH = \
main_test_itp_ele.o \
analyzer_test_ele_tbl.o \
\
check_ineterppolated_mesh.o \
input_control_interpolate.o \
mesh_interpolation.o \
interpolate_position.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'ITP_FIELD_DIR = $(ITP_FIELD_DIR)'      >> $(MAKENAME)

target_list:
	@echo 'TARGET_ITP_UDT =      $$(BUILDDIR)/$(TARGET_ITP_UDT)' \
	>> $(MAKENAME)
	@echo 'TARGET_ITP_RST =      $$(BUILDDIR)/$(TARGET_ITP_RST)' \
	>> $(MAKENAME)
	@echo 'TARGET_ITP_TEST =     $$(BUILDDIR)/$(TARGET_ITP_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_ITP_ELE_TEST = $$(BUILDDIR)/$(TARGET_ITP_ELE_TEST)' \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_ITP_UDT)': '$$(MOD_ITP_FIELDS)' '$$(LIB_FILES_INTERPOLATE)' \
	'$$(MOD_ITP_MESH)' '$$(LIB_FILES_INTERPOLATE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_ITP_UDT)' '$$(MOD_ITP_FIELDS)' \
	'-L. $$(LIBS_INTERPOLATE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_ITP_RST)': '$$(MOD_ITP_RESTART)' '$$(LIB_FILES_INTERPOLATE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_ITP_RST)' '$$(MOD_ITP_RESTART)' \
	'-L. $$(LIBS_INTERPOLATE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_ITP_TEST)': \
	'$$(MOD_ITP_MESH)' '$$(LIB_FILES_INTERPOLATE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_ITP_TEST)' \
	'$$(MOD_ITP_MESH)' \
	'-L. $$(LIBS_INTERPOLATE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_ITP_ELE_TEST)': \
	'$$(MOD_ITP_ELE_MESH)' '$$(LIB_FILES_INTERPOLATE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_ITP_ELE_TEST)' \
	'$$(MOD_ITP_ELE_MESH)' \
	'-L. $$(LIBS_INTERPOLATE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_ITP_FIELDS=  \\ >> $(MAKENAME)
	@echo  $(MOD_ITP_FIELDS)   >> $(MAKENAME)
	@echo  MOD_ITP_RESTART=  \\ >> $(MAKENAME)
	@echo  $(MOD_ITP_RESTART)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(ITP_FIELD_DIR)' $(SOURCES)

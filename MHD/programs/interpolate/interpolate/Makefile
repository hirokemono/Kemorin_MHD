#
#
#

ITP_FIELD_DIR = $$(ITP_PLUG_DIR)/interpolate

TARGET_ITP_UDT = interpolate_udt
TARGET_ITP_RST = interpolate_rst

SOURCES = $(shell ls *.f90)

MOD_ITP_FIELDS = \
input_control_interpolate.o \
analyzer_interpolate_udt.o \
main_interpolate_udt.o

MOD_ITP_RESTART = \
input_control_interpolate.o \
analyzer_interpolate_rst.o \
main_interpolate_rst.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'ITP_FIELD_DIR = $(ITP_FIELD_DIR)'      >> $(MAKENAME)

target_list:
	@echo 'TARGET_ITP_UDT = $$(BUILDDIR)/$(TARGET_ITP_UDT)' \
	>> $(MAKENAME)
	@echo 'TARGET_ITP_RST = $$(BUILDDIR)/$(TARGET_ITP_RST)' \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_ITP_UDT)': '$$(MOD_ITP_FIELDS)' '$$(LIB_FILES_INTERPOLATE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_ITP_UDT)' '$$(MOD_ITP_FIELDS)' \
	'-L. $$(LIBS_INTERPOLATE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_ITP_RST)': '$$(MOD_ITP_RESTART)' '$$(LIB_FILES_INTERPOLATE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_ITP_RST)' '$$(MOD_ITP_RESTART)' \
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
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(ITP_FIELD_DIR)' $(SOURCES)

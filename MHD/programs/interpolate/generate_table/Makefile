#
#
#

GEN_TABLE_DIR = $$(ITP_PLUG_DIR)/generate_table

TARGET_GEN_TABLE =     const_interpolate_table
TARGET_GEN_ELE_TABLE = const_ele_interpolate_tbl

SOURCES = $(shell ls *.f90)

MOD_GEN_TABLE = \
main_gen_itp_tbl.o \
analyzer_gen_table.o

MOD_GEN_ELE_TABLE = \
main_gen_ele_itp_tbl.o \
analyzer_gen_ele_table.o


#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'GEN_TABLE_DIR = $(GEN_TABLE_DIR)'   >> $(MAKENAME)

target_list:
	@echo 'TARGET_GEN_TABLE =     $$(BUILDDIR)/$(TARGET_GEN_TABLE)' \
	>> $(MAKENAME)
	@echo 'TARGET_GEN_ELE_TABLE = $$(BUILDDIR)/$(TARGET_GEN_ELE_TABLE)' \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_GEN_TABLE)': '$$(MOD_GEN_TABLE)' '$$(LIB_FILES_INTERPOLATE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_GEN_TABLE)' \
	'$$(MOD_GEN_TABLE)' '-L. $$(LIBS_INTERPOLATE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)

	@echo '$$(TARGET_GEN_ELE_TABLE)': '$$(MOD_GEN_ELE_TABLE)' '$$(LIB_FILES_INTERPOLATE)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_GEN_ELE_TABLE)' \
	'$$(MOD_GEN_ELE_TABLE)'  '-L. $$(LIBS_INTERPOLATE)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_GEN_TABLE=  \\ >> $(MAKENAME)
	@echo  $(MOD_GEN_TABLE) >> $(MAKENAME)
	@echo  MOD_GEN_ELE_TABLE=  \\ >> $(MAKENAME)
	@echo  $(MOD_GEN_ELE_TABLE) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(GEN_TABLE_DIR)' $(SOURCES)

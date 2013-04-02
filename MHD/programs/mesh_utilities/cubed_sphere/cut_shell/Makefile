#
#
#

CUTSHELL_DIR = $$(CUBE_SPH_DIR)/cut_shell

TARGET_CUTSHELL = cutshell_kemo

SRC_CUTSHELL = $(shell ls *.f90)
MOD_CUTSHELL = $(addsuffix .o,$(basename $(SRC_CUTSHELL)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'CUTSHELL_DIR = $(CUTSHELL_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_CUTSHELL = $$(BUILDDIR)/$(TARGET_CUTSHELL)'    \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_CUTSHELL)': '$$(MOD_CUTSHELL)'  '$$(LIB_FILES_CUBED_SPH)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)  $$(F90FLAGS) -o  $$(TARGET_CUTSHELL)' \
	'$$(MOD_CUTSHELL)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_CUTSHELL=  \\ >> $(MAKENAME)
	@echo  $(MOD_CUTSHELL) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)

module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(CUTSHELL_DIR)' $(SRC_CUTSHELL)

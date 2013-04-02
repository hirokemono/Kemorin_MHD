#
#
#

CUBE_ELIP_DIR = $$(CUBE_SPH_DIR)/elipsoid

TARGET_ELIPSOID = cutshell_kemo

SRC_ELIPSOID = $(shell ls *.f90)
MOD_ELIPSOID = $(addsuffix .o,$(basename $(SRC_ELIPSOID)) )

#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'CUBE_ELIP_DIR = $(CUBE_ELIP_DIR)'    >> $(MAKENAME)
	@echo '#' >> $(MAKENAME)

target_list:
	@echo 'TARGET_ELIPSOID = $$(BUILDDIR)/$(TARGET_ELIPSOID)'    \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_ELIPSOID)': '$$(MOD_ELIPSOID)'  '$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)  $$(F90FLAGS) -o  $$(TARGET_ELIPSOID)' \
	'$$(MOD_ELIPSOID)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_ELIPSOID=  \\ >> $(MAKENAME)
	@echo  $(MOD_ELIPSOID) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)



module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(CUTSHELL_DIR)' $(SRC_ELIPSOID)

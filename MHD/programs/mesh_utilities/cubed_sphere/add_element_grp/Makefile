#
#
#

ELE_GRP_SPH_DIR = $$(CUBE_SPH_DIR)/add_element_grp

TARGET_ADDGRP_SPH =      add_egrp_cubed_sph
TARGET_PARA_ADDGRP_SPH = parapllel_add_egrp_mesh

SRC_ADDGRP_SPH = $(shell ls *.f90)
MOD_ADDGRP_SPH = \
add_elegrp_sph.o \
\
m_control_data_add_ele_grp.o \
m_add_ele_grp_parameter.o \
m_work_4_add_egrp_sph.o \
\
analyzer_add_ele_group.o \
set_control_add_2d_egrp.o \
set_new_2d_element_group.o \
set_ele_grp2_by_2d.o \
start_end_4_2d_ele_grping.o \
set_grouping_geometry.o


#
#  -------------------------------------------------------------------------
#

dir_list:
	@echo 'ELE_GRP_SPH_DIR = $(ELE_GRP_SPH_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_ADDGRP_SPH =      $$(BUILDDIR)/$(TARGET_ADDGRP_SPH)'    \
    @echo 'TARGET_PARA_ADDGRP_SPH = $$(BUILDDIR)/$(TARGET_PARA_ADDGRP_SPH)'    \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_ADDGRP_SPH)': '$$(MOD_ADDGRP_SPH)' \
	 '$$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90) $$(F90FLAGS) -o $$(TARGET_ADDGRP_SPH)' \
	'$$(MOD_ADDGRP_SPH)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

mod_list:
	@echo  MOD_ADDGRP_SPH=  \\ >> $(MAKENAME)
	@echo  $(MOD_ADDGRP_SPH) >> $(MAKENAME)
	@echo  '#'  >> $(MAKENAME)



module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(ELE_GRP_SPH_DIR)' $(SRC_ADDGRP_SPH)

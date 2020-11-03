#
#
#

NODE_CLUSTERING_TEST_DIR = $$(TESTS_DIR)/node_clustering_test

TARGET_GROUPING_BY_VOLUME = grouping_by_volume
TARGET_VOLUME_REPARTITION = repartition_by_volume

LIBS_NODE_CLUSTERING_TEST = \
'$$(LIB_UTILS)'   \
'$$(LIB_PARA)'   \
'$$(LIB_SOLVER)' \
'$$(LIB_MESH)'

LIB_FILES_NODE_CLUSTERING_TEST = \
'$$(LIB_UTILS_FILE)'   \
'$$(LIB_PARA_FILE)'   \
'$$(LIB_SOLVER_FILE)' \
'$$(LIB_MESH_FILE)'

ifdef FORTRAN_C_CONNECT
  LIBS_NODE_CLUSTERING_TEST+=        '$$(LIB_KEMO_C)'
  LIB_FILES_NODE_CLUSTERING_TEST+=   '$$(LIB_KEMO_C_FILE)'
endif

MOD_GROUPING_BY_VOLUME = \
main_volume_grouping.o \
analyzer_volume_grouping.o \
\
t_1d_repartitioning_work.o \
t_control_param_vol_grping.o \
t_ctl_data_volume_grouping.o \
t_repartition_by_volume.o \
\
set_istack_4_domain_block.o \
repart_in_xyz_by_volume.o \
set_repartition_group_name.o \
xyz_block_id_by_nod_vol.o

MOD_VOLUME_REPARTITION = \
main_volume_repart_test.o \
analyzer_volume_repart_test.o \
\
t_1d_repartitioning_work.o \
t_control_param_vol_grping.o \
t_ctl_data_volume_grouping.o \
t_repartition_by_volume.o \
\
set_istack_4_domain_block.o \
repart_in_xyz_by_volume.o \
set_repartition_group_name.o \
xyz_block_id_by_nod_vol.o

SOURCES = $(shell ls *.f90)

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'NODE_CLUSTERING_TEST_DIR = $(NODE_CLUSTERING_TEST_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_GROUPING_BY_VOLUME = $$(BUILDDIR)/$(TARGET_GROUPING_BY_VOLUME)' >> $(MAKENAME)
	@echo 'TARGET_VOLUME_REPARTITION = $$(BUILDDIR)/$(TARGET_VOLUME_REPARTITION)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo node_clustring_test: \
	'$$(TARGET_GROUPING_BY_VOLUME) ' \
	'$$(TARGET_VOLUME_REPARTITION) '  >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_GROUPING_BY_VOLUME): $$(MOD_GROUPING_BY_VOLUME) $$(LIB_FILES_NODE_CLUSTERING_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_GROUPING_BY_VOLUME)' \
	'$$(MOD_GROUPING_BY_VOLUME)' \
	'-L. $$(LIBS_NODE_CLUSTERING_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_VOLUME_REPARTITION): $$(MOD_VOLUME_REPARTITION) $$(LIB_FILES_NODE_CLUSTERING_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_VOLUME_REPARTITION)' \
	'$$(MOD_VOLUME_REPARTITION)' \
	'-L. $$(LIBS_NODE_CLUSTERING_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)



lib_name:
	@echo  LIBS_NODE_CLUSTERING_TEST=  \\          >> $(MAKENAME)
	@echo  $(LIBS_NODE_CLUSTERING_TEST)            >> $(MAKENAME)
	@echo  LIB_FILES_NODE_CLUSTERING_TEST=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_NODE_CLUSTERING_TEST)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

mod_list:
	@echo  MOD_GROUPING_BY_VOLUME=  \\ >> $(MAKENAME)
	@echo  $(MOD_GROUPING_BY_VOLUME) >> $(MAKENAME)
	@echo  MOD_VOLUME_REPARTITION=  \\ >> $(MAKENAME)
	@echo  $(MOD_VOLUME_REPARTITION) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(NODE_CLUSTERING_TEST_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

#
#
#

NODE_CLUSTERING_TEST_DIR = $$(TESTS_DIR)/node_clustering_test

TARGET_GROUPING_BY_VOLUME = grouping_by_volume

MOD_GROUPING_BY_VOLUME = \
main_volume_grouping.o \
analyzer_volume_grouping.o \
\
t_ctl_file_volume_grouping.o \
t_control_param_repartition.o

SOURCES = $(shell ls *.f90)

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'NODE_CLUSTERING_TEST_DIR = $(NODE_CLUSTERING_TEST_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_GROUPING_BY_VOLUME = $$(TEST_BUILD_DIR)/$(TARGET_GROUPING_BY_VOLUME)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo node_clustring_test: \
	'$$(TARGET_GROUPING_BY_VOLUME) '  >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_GROUPING_BY_VOLUME): $$(MOD_GROUPING_BY_VOLUME) $$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_GROUPING_BY_VOLUME)' \
	'$$(MOD_GROUPING_BY_VOLUME)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)



lib_name:

mod_list:
	@echo  MOD_GROUPING_BY_VOLUME=  \\ >> $(MAKENAME)
	@echo  $(MOD_GROUPING_BY_VOLUME) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(NODE_CLUSTERING_TEST_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

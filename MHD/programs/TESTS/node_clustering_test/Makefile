#
#
#

NODE_CLUSTERING_TEST_DIR = $$(TESTS_DIR)/node_clustering_test

TARGET_GROUPING_BY_VOLUME = grouping_by_volume
TARGET_VOLUME_REPART_TEST = repart_by_vol_test

MOD_GROUPING_BY_VOLUME = \
main_volume_grouping.o \
analyzer_volume_grouping.o

MOD_VOLUME_REPART_TEST = \
main_volume_repart_test.o \
analyzer_volume_repart_test.o


SOURCES = $(shell ls *.f90)

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'NODE_CLUSTERING_TEST_DIR = $(NODE_CLUSTERING_TEST_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_GROUPING_BY_VOLUME = $$(BUILDDIR)/$(TARGET_GROUPING_BY_VOLUME)' >> $(MAKENAME)
	@echo 'TARGET_VOLUME_REPART_TEST = $$(BUILDDIR)/$(TARGET_VOLUME_REPART_TEST)' >> $(MAKENAME)
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
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_VOLUME_REPART_TEST): $$(MOD_VOLUME_REPART_TEST) $$(LIB_FILES_MESH_UTIL)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_VOLUME_REPART_TEST)' \
	'$$(MOD_VOLUME_REPART_TEST)' \
	'-L. $$(LIBS_MESH_UTIL)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)



lib_name:

mod_list:
	@echo  MOD_GROUPING_BY_VOLUME=  \\ >> $(MAKENAME)
	@echo  $(MOD_GROUPING_BY_VOLUME) >> $(MAKENAME)
	@echo  MOD_VOLUME_REPART_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_VOLUME_REPART_TEST) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(NODE_CLUSTERING_TEST_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

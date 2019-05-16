#
#
#

NODE_CLUSTERING_TEST_DIR = $$(TESTS_DIR)/node_clustering_test

TARGET_NODE_CLUSTERING_TEST = node_clustering_test

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

SRC_NODE_CLUSTERING_TEST = $(shell ls *.f90)
MOD_NODE_CLUSTERING_TEST = $(addsuffix .o,$(basename $(SRC_NODE_CLUSTERING_TEST)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'NODE_CLUSTERING_TEST_DIR = $(NODE_CLUSTERING_TEST_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_NODE_CLUSTERING_TEST = $$(BUILDDIR)/$(TARGET_NODE_CLUSTERING_TEST)' \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo node_clustring_test: \
	'$$(TARGET_NODE_CLUSTERING_TEST)  '\
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_NODE_CLUSTERING_TEST): $$(MOD_NODE_CLUSTERING_TEST) $$(LIB_FILES_NODE_CLUSTERING_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_NODE_CLUSTERING_TEST)' \
	'$$(MOD_NODE_CLUSTERING_TEST)' \
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
	@echo  MOD_NODE_CLUSTERING_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_NODE_CLUSTERING_TEST) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(NODE_CLUSTERING_TEST_DIR)' $(SRC_NODE_CLUSTERING_TEST)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

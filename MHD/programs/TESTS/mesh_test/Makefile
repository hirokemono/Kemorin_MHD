#
#
#

MESH_TEST_DIR = $$(TESTS_DIR)/mesh_test

TARGET_MESH_TEST = mesh_test

LIBS_MESH_TEST = \
'$$(LIB_PARA)'   \
'$$(LIB_SOLVER)' \
'$$(LIB_MESH)'

LIB_FILES_MESH_TEST = \
'$$(LIB_PARA_FILE)'   \
'$$(LIB_SOLVER_FILE)' \
'$$(LIB_MESH_FILE)'

ifdef FORTRAN_C_CONNECT
  LIBS_MESH_TEST+=        '$$(LIB_KEMO_C)'
  LIB_FILES_MESH_TEST+=   '$$(LIB_KEMO_C_FILE)'
endif

CURRENTDIR = $(shell pwd)
SOURCES = $(shell ls *.f90)
MOD_MESH_TEST = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'MESH_TEST_DIR = $(MESH_TEST_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_MESH_TEST = $$(TEST_BUILD_DIR)/$(TARGET_MESH_TEST)' \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo fem_mesh_test: \
	'$$(TARGET_MESH_TEST)  '\
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_MESH_TEST): $$(MOD_MESH_TEST) $$(LIB_FILES_MESH_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_MESH_TEST)' \
	'$$(MOD_MESH_TEST)' \
	'-L. $$(LIBS_MESH_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)



lib_name:
	@echo  LIBS_MESH_TEST=  \\          >> $(MAKENAME)
	@echo  $(LIBS_MESH_TEST)            >> $(MAKENAME)
	@echo  LIB_FILES_MESH_TEST=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_MESH_TEST)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

mod_list:
	@echo  MOD_MESH_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_MESH_TEST) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@cat Makefile.depends >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) Makefile.depends '$$(MESH_TEST_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

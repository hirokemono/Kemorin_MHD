#
#
#

BC_FILE_TEST_DIR = $$(TESTS_DIR)/BC_file_test

TARGET_BC_FILE_TEST = fem_boundary_temp

LIBS_BC_FILE_TEST = \
'$$(LIB_PARA)'   \
'$$(LIB_SOLVER)' \
'$$(LIB_MESH)'

LIB_FILES_BCFILE_TEST = \
'$$(LIB_PARA_FILE)'   \
'$$(LIB_SOLVER_FILE)' \
'$$(LIB_MESH_FILE)'

ifdef FORTRAN_C_CONNECT
  LIBS_BC_FILE_TEST+=        '$$(LIB_KEMO_C)'
  LIB_FILES_BCFILE_TEST+=   '$$(LIB_KEMO_C_FILE)'
endif


SRC_BC_FILE_TEST = $(shell ls *.f90)
MOD_BC_FILE_TEST = \
m_ctl_data_test_bc_temp.o \
m_ctl_params_test_bc_temp.o \
\
const_sph_boundary_temp.o \
\
analyzer_sph_bc_temp.o \
main_sph_bc_temp.o


#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'BC_FILE_TEST_DIR = $(BC_FILE_TEST_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_BC_FILE_TEST = $$(BUILDDIR)/$(TARGET_BC_FILE_TEST)' \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo fem_mesh_test: \
	'$$(TARGET_BC_FILE_TEST)  '\
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_BC_FILE_TEST): $$(MOD_BC_FILE_TEST) $$(LIB_FILES_BCFILE_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_BC_FILE_TEST)' \
	'$$(MOD_BC_FILE_TEST)' \
	'-L. $$(LIBS_BC_FILE_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)



lib_name:
	@echo  LIBS_BC_FILE_TEST=  \\          >> $(MAKENAME)
	@echo  $(LIBS_BC_FILE_TEST)            >> $(MAKENAME)
	@echo  LIB_FILES_BCFILE_TEST=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_BCFILE_TEST)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

mod_list:
	@echo  MOD_BC_FILE_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_BC_FILE_TEST) >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(BC_FILE_TEST_DIR)' $(SRC_BC_FILE_TEST)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

#
#
#

SOLVERTEST_DIR = $$(TESTS_DIR)/test_solver

TARGET_SOLVERTEST = blockNNtest
TARGET_DJDSTEST =   djds_solvertest
TARGET_LUTEST =     single_lutest
TARGET_MUL_LUTEST = multi_lutest
TARGET_5LUTEST =    single_5band_lutest

LIBS_SOLVER_TEST = \
'$$(LIB_PARA)'   \
'$$(LIB_SOLVER)' \
'$$(LIB_MESH)'

LIB_FILES_SOLVER_TEST = \
'$$(LIB_PARA_FILE)'   \
'$$(LIB_SOLVER_FILE)' \
'$$(LIB_MESH_FILE)'

ifdef FORTRAN_C_CONNECT
  LIBS_SOLVER_TEST+=        '$$(LIB_KEMO_C)'
  LIB_FILES_SOLVER_TEST+=   '$$(LIB_KEMO_C_FILE)'
endif


SOURCES = $(shell ls *.f90)


MOD_SOLVERTEST_CORE = \
t_ctl_data_solver_test.o \
crs_matrix_io.o \
set_control_solver_test.o \

MOD_SOLVERTEST = \
solve_by_crs_solver.o \
analyzer_test_crs.o \
main_test_crs.o


MOD_DJDSTEST = \
analyzer_test_djds.o \
main_test_djds.o


MOD_LUTEST =     testmt_single.o
MOD_MUL_LUTEST = testmt_multi.o

MOD_5LUTEST = \
testlu_5band.o \
m_sample_matrices.o

MODULES = \
$(MOD_SOLVERTEST_CORE) \
$(MOD_SOLVERTEST) \
$(MOD_DJDSTEST) \
$(MOD_LUTEST) \
$(MOD_MUL_LUTEST) \
$(MOD_5LUTEST)

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'SOLVERTEST_DIR =  $(SOLVERTEST_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_SOLVERTEST = $$(BUILDDIR)/$(TARGET_SOLVERTEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_DJDSTEST =   $$(BUILDDIR)/$(TARGET_DJDSTEST)'   \
	>> $(MAKENAME)
	@echo 'TARGET_LUTEST =     $$(BUILDDIR)/$(TARGET_LUTEST)'     \
	>> $(MAKENAME)
	@echo 'TARGET_5LUTEST =     $$(BUILDDIR)/$(TARGET_5LUTEST)'   \
	>> $(MAKENAME)
	@echo 'TARGET_MUL_LUTEST = $$(BUILDDIR)/$(TARGET_MUL_LUTEST)' \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo solver_tests:  \
	'$$(TARGET_SOLVERTEST)      $$(TARGET_DJDSTEST)'        \
	'$$(TARGET_LUTEST)          $$(TARGET_MUL_LUTEST)'      \
	'$$(TARGET_5LUTEST) '        \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SOLVERTEST)': \
	'$$(MOD_SOLVERTEST) $$(LIB_FILES_SOLVER_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_SOLVERTEST)' \
	'$$(MOD_SOLVERTEST)' \
	'-L. $$(LIBS_SOLVER_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_DJDSTEST)':  \
	'$$(MOD_DJDSTEST) $$(LIB_FILES_SOLVER_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_DJDSTEST)' \
	'$$(MOD_DJDSTEST)' \
	'-L. $$(LIBS_SOLVER_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_LUTEST): $$(MOD_LUTEST) $$(LIB_FILES_SOLVER_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_LUTEST)' \
	'$$(MOD_LUTEST)' \
	'-L. $$(LIBS_SOLVER_TEST) $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_MUL_LUTEST): $$(MOD_MUL_LUTEST) $$(LIB_FILES_SOLVER_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_MUL_LUTEST)' \
	'$$(MOD_MUL_LUTEST)' \
	'-L. $$(LIBS_SOLVER_TEST) $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_5LUTEST): $$(MOD_5LUTEST) $$(LIB_FILES_SOLVER_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_5LUTEST)' \
	'$$(MOD_5LUTEST)' \
	'-L. $$(LIBS_SOLVER_TEST) $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:
	@echo  LIBS_SOLVER_TEST=  \\          >> $(MAKENAME)
	@echo  $(LIBS_SOLVER_TEST)            >> $(MAKENAME)
	@echo  LIB_FILES_SOLVER_TEST=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_SOLVER_TEST)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

mod_list:
	@echo  MOD_SOLVERTEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_SOLVERTEST_CORE)  \\  >> $(MAKENAME)
	@echo  $(MOD_SOLVERTEST)   >> $(MAKENAME)
	@echo  MOD_DJDSTEST=  \\   >> $(MAKENAME)
	@echo  $(MOD_SOLVERTEST_CORE)  \\  >> $(MAKENAME)
	@echo  $(MOD_DJDSTEST)     >> $(MAKENAME)
	@echo  MOD_LUTEST=  \\     >> $(MAKENAME)
	@echo  $(MOD_LUTEST)       >> $(MAKENAME)
	@echo  MOD_MUL_LUTEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_MUL_LUTEST)   >> $(MAKENAME)
	@echo  MOD_5LUTEST=  \\    >> $(MAKENAME)
	@echo  $(MOD_5LUTEST)      >> $(MAKENAME)
	@echo   >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(SOLVERTEST_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

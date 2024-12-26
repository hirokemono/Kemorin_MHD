#
#
#
#
#

FDM_TEST_DIR =       $$(TESTS_DIR)/FDM_test

TARGET_FDM_TEST =      test_fdm

TARGETS_FFT_TEST = \
'$$(TARGET_FDM_TEST)'

LIBS_FDM_TEST = \
'$$(LIB_MHD)'     \
'$$(LIB_PARA)'    \
'$$(LIB_SOLVER)'  \
'$$(LIB_MESH)'

LIB_FILES_FDM_TEST = \
'$$(LIB_MHD_FILE)'    \
'$$(LIB_PARA_FILE)'   \
'$$(LIB_SOLVER_FILE)' \
'$$(LIB_MESH_FILE)'

ifdef FORTRAN_C_CONNECT
  LIBS_FDM_TEST+=        '$$(LIB_KEMO_C)'
  LIB_FILES_FDM_TEST+=   '$$(LIB_KEMO_C_FILE)'
endif

CURRENTDIR = $(shell pwd)
SOURCES = $(shell ls *.f90)

MOD_FDM_TEST = \
FDM_matrices_check.o


#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'FDM_TEST_DIR =  $(FDM_TEST_DIR)'          >> $(MAKENAME)

target_list:
	@echo 'TARGET_FDM_TEST =      $$(TEST_BUILD_DIR)/$(TARGET_FDM_TEST)'     >> $(MAKENAME)

target_task:
	@echo fft_test: $(TARGETS_FFT_TEST) >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_FDM_TEST)': '$$(MOD_FDM_TEST)' \
	'$$(LIB_FILES_FDM_TEST)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_FDM_TEST)' \
	'$$(MOD_FDM_TEST)  -L. $$(LIBS_FDM_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)

lib_name:
	@echo  LIBS_FDM_TEST=  \\          >> $(MAKENAME)
	@echo  $(LIBS_FDM_TEST)            >> $(MAKENAME)
	@echo  LIB_FILES_FDM_TEST=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_FDM_TEST)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

mod_list:
	@echo  MOD_FDM_TEST=      \\  >> $(MAKENAME)
	@echo  $(MOD_FDM_TEST)        >> $(MAKENAME)


module:

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(FDM_TEST_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

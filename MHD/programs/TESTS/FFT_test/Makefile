#
#
#
#
#

FFT_TEST_DIR =       $$(TESTS_DIR)/FFT_test

TARGET_FFT_TEST =      test_fft

LIBS_FFT_TEST = \
'$$(LIB_MESH)'      \
'$$(LIB_FFTPACK)'   \
'$$(LIB_ISPACK)'

LIB_FILES_FFT_TEST = \
'$$(LIB_MESH_FILE)'      \
'$$(LIB_FFTPACK_FILE)'   \
'$$(LIB_ISPACK_FILE)'

ifdef FORTRAN_C_CONNECT
  LIBS_FFT_TEST+=        '$$(LIB_KEMO_C)'
  LIB_FILES_FFT_TEST+=   '$$(LIB_KEMO_C_FILE)'
endif

SRC_FFT_TEST = $(shell ls *.f90)
MOD_FFT_TEST = $(addsuffix .o,$(basename $(SRC_FFT_TEST)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'FFT_TEST_DIR =  $(FFT_TEST_DIR)'          >> $(MAKENAME)

target_list:
	@echo 'TARGET_FFT_TEST =      $$(BUILDDIR)/$(TARGET_FFT_TEST)' \
	>> $(MAKENAME)

target_task:
	@echo fft_test: '$$(TARGET_FFT_TEST)' \
	>> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_FFT_TEST)': '$$(MOD_FFT_TEST)' \
	'$$(LIB_FILES_FFT_TEST)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90FLAGS)' -o '$$(TARGET_FFT_TEST)' \
	'$$(MOD_FFT_TEST)  -L. $$(LIBS_FFT_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)


lib_name:
	@echo  LIBS_FFT_TEST=  \\          >> $(MAKENAME)
	@echo  $(LIBS_FFT_TEST)            >> $(MAKENAME)
	@echo  LIB_FILES_FFT_TEST=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_FFT_TEST)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

mod_list:
	@echo  MOD_FFT_TEST=      \\  >> $(MAKENAME)
	@echo  $(MOD_FFT_TEST)        >> $(MAKENAME)


module:
	@$(MAKE_MOD_DEP) '$(MAKENAME)' '$$(FFT_TEST_DIR)' $(SRC_FFT_TEST)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

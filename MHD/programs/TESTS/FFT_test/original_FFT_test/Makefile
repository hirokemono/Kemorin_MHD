#
#
#
TEST_FFT_ORIGINAL_DIR = $$(FFT_TEST_DIR)/original_FFT_test
TARGET_ORG_FFT_TEST =   $$(TEST_BUILD_DIR)/test_fft
TARGETS = 

SOURCES = $(shell ls *.f90)
MOD_ORG_FFTW_TEST = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'TEST_FFT_ORIGINAL_DIR = $(TEST_FFT_ORIGINAL_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_ORG_FFT_TEST = $(TARGET_ORG_FFT_TEST)' >> $(MAKENAME)
	@echo '$(TARGETS) += $$(TARGET_ORG_FFT_TEST)'        >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_ORG_FFT_TEST)': \
	'$$(MOD_ORG_FFTW_TEST) $$(LIB_FILES_FFT_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_ORG_FFT_TEST)' \
	'$$(MOD_ORG_FFTW_TEST)' \
	'-L. $$(LIBS_FFT_TEST) $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

lib_tasks:

mod_list:
	@echo  MOD_ORG_FFTW_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_ORG_FFTW_TEST)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(TEST_FFT_ORIGINAL_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

distclean: clean

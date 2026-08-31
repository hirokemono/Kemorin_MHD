#
#
#
TEST_SGL_CPLX_ROCFFT_PRT_DIR =    $$(ROCM_TEST_DIR)/single_complex_rocFFT_prt
TARGET_SGL_CPLX_ROCFFT_PRT_TEST = $$(TEST_BUILD_DIR)/test_sgl_cplx_rocFFT_prt
TARGETS = 

SOURCES = $(shell ls *.f90)
MOD_SGL_CPLX_ROCFFT_PRT_TEST = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'TEST_SGL_CPLX_ROCFFT_PRT_DIR = $(TEST_SGL_CPLX_ROCFFT_PRT_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_SGL_CPLX_ROCFFT_PRT_TEST' \
	'= $(TARGET_SGL_CPLX_ROCFFT_PRT_TEST)' >> $(MAKENAME)
	@echo '$(TARGETS) += $$(TARGET_SGL_CPLX_ROCFFT_PRT_TEST)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_SGL_CPLX_ROCFFT_PRT_TEST)': \
	'$$(MOD_SGL_CPLX_ROCFFT_PRT_TEST) $$(LIB_FILES_ROCM_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SGL_CPLX_ROCFFT_PRT_TEST)' \
	'$$(MOD_SGL_CPLX_ROCFFT_PRT_TEST)' \
	'-L. $$(LIBS_ROCM_TEST) $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

lib_tasks:

mod_list:
	@echo  MOD_SGL_CPLX_ROCFFT_PRT_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_SGL_CPLX_ROCFFT_PRT_TEST)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(TEST_SGL_CPLX_ROCFFT_PRT_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

distclean: clean

#
#
#
TEST_CPLX_ROCFFT_RTP_DIR =    $$(ROCM_TEST_DIR)/complex_rocFFT_rtp
TARGET_CPLX_ROCFFT_RTP_TEST = $$(TEST_BUILD_DIR)/test_cplx_rocFFT_rtp
TARGETS = 

SOURCES = $(shell ls *.f90)
MOD_CPLX_ROCFFT_RTP_TEST = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'TEST_CPLX_ROCFFT_RTP_DIR = $(TEST_CPLX_ROCFFT_RTP_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_CPLX_ROCFFT_RTP_TEST' \
	'= $(TARGET_CPLX_ROCFFT_RTP_TEST)' >> $(MAKENAME)
	@echo '$(TARGETS) += $$(TARGET_CPLX_ROCFFT_RTP_TEST)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_CPLX_ROCFFT_RTP_TEST)': \
	'$$(MOD_CPLX_ROCFFT_RTP_TEST) $$(LIB_FILES_ROCM_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_CPLX_ROCFFT_RTP_TEST)' \
	'$$(MOD_CPLX_ROCFFT_RTP_TEST)' \
	'-L. $$(LIBS_ROCM_TEST) $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

lib_tasks:

mod_list:
	@echo  MOD_CPLX_ROCFFT_RTP_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_CPLX_ROCFFT_RTP_TEST)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(TEST_CPLX_ROCFFT_RTP_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

distclean: clean

#
#
#
TEST_REAL_ROCFFT_FFTPACK_RTP_DIR = $$(ROCM_TEST_DIR)/real_rocFFT_FFTPACK_rtp
TARGET_REAL_ROCFFT_FFTPACK_RTP_TEST = $$(TEST_BUILD_DIR)/test_real_rocFFT_FFTPACK_rtp
TARGETS = 

SOURCES = $(shell ls *.f90)
MOD_REAL_ROCFFT_FFTPACK_RTP_TEST = $(addsuffix .o,$(basename $(SOURCES)) )

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'TEST_REAL_ROCFFT_FFTPACK_RTP_DIR = $(TEST_REAL_ROCFFT_FFTPACK_RTP_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_REAL_ROCFFT_FFTPACK_RTP_TEST' \
	'= $(TARGET_REAL_ROCFFT_FFTPACK_RTP_TEST)' >> $(MAKENAME)
	@echo '$(TARGETS) += $$(TARGET_REAL_ROCFFT_FFTPACK_RTP_TEST)' >> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo '$$(TARGET_REAL_ROCFFT_FFTPACK_RTP_TEST)': \
	'$$(MOD_REAL_ROCFFT_FFTPACK_RTP_TEST) $$(LIB_FILES_ROCM_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_REAL_ROCFFT_FFTPACK_RTP_TEST)' \
	'$$(MOD_REAL_ROCFFT_FFTPACK_RTP_TEST)' \
	'-L. $$(LIBS_ROCM_TEST) $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)


lib_name:

lib_tasks:

mod_list:
	@echo  MOD_REAL_ROCFFT_FFTPACK_RTP_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_REAL_ROCFFT_FFTPACK_RTP_TEST)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(TEST_REAL_ROCFFT_FFTPACK_RTP_DIR)' $(SOURCES)

clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

distclean: clean

#
#
#

ROCFFT_TEST_DIR = $$(ROCM_TEST_DIR)/rocFFT

TARGET_ROCFFT_PRT_TEST =         $$(TEST_BUILD_DIR)/test_rocFFT_prt
TARGET_ROCFFT_RTP_TEST =         $$(TEST_BUILD_DIR)/test_rocFFT_rtp
TARGET_ROCFFT_CPU_FFT_PRT_TEST = $$(TEST_BUILD_DIR)/test_rocFFT_cpu_FFTs_prt
TARGET_ROCFFT_CPU_FFT_RTP_TEST = $$(TEST_BUILD_DIR)/test_rocFFT_cpu_FFTs_rtp

CURRENTDIR = $(shell pwd)
SOURCES = $(shell ls *.f90 *.F90)

MOD_ROCFFT_PRT_TEST =         test_rocFFT_prt.o

MOD_ROCFFT_RTP_TEST =         test_rocFFT_rtp.o

MOD_ROCFFT_CPU_FFT_PRT_TEST = \
test_rocFFT_cpu_FFTs_prt.o \
sel_pin_rocFFT_CPU_FFT.o

MOD_ROCFFT_CPU_FFT_RTP_TEST = \
test_rocFFT_cpu_FFTs_rtp.o \
sel_pout_rocFFT_CPU_FFT.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'ROCFFT_TEST_DIR = $(ROCFFT_TEST_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_ROCFFT_PRT_TEST = $(TARGET_ROCFFT_PRT_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_ROCFFT_RTP_TEST = $(TARGET_ROCFFT_RTP_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_ROCFFT_CPU_FFT_PRT_TEST = $(TARGET_ROCFFT_CPU_FFT_PRT_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_ROCFFT_CPU_FFT_RTP_TEST = $(TARGET_ROCFFT_CPU_FFT_RTP_TEST)' \
	>> $(MAKENAME)
	@echo ' ' >> $(MAKENAME)

target_task:
	@echo rocFFT_tests: \
	'$$(TARGET_ROCFFT_PRT_TEST)              $$(TARGET_ROCFFT_RTP_TEST)       ' \
	'$$(TARGET_ROCFFT_CPU_FFT_PRT_TEST)      $$(TARGET_ROCFFT_CPU_FFT_RTP_TEST) ' \
	 >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_ROCFFT_PRT_TEST):' \
	'$$(MOD_ROCFFT_PRT_TEST)' \
	'$$(LIB_FILES_ROCM_TEST)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_ROCFFT_PRT_TEST)' \
	'$$(MOD_ROCFFT_PRT_TEST)' \
	'-L. $$(LIBS_ROCM_TEST) $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_ROCFFT_RTP_TEST):' \
	'$$(MOD_ROCFFT_RTP_TEST)' \
	'$$(LIB_FILES_ROCM_TEST)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_ROCFFT_RTP_TEST)' \
	'$$(MOD_ROCFFT_RTP_TEST)' \
	'-L. $$(LIBS_ROCM_TEST) $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_ROCFFT_CPU_FFT_PRT_TEST):' \
	'$$(MOD_ROCFFT_CPU_FFT_PRT_TEST)' \
	'$$(LIB_FILES_ROCM_TEST)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_ROCFFT_CPU_FFT_PRT_TEST)' \
	'$$(MOD_ROCFFT_CPU_FFT_PRT_TEST)' \
	'-L. $$(LIBS_ROCM_TEST) $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_ROCFFT_CPU_FFT_RTP_TEST):' \
	'$$(MOD_ROCFFT_CPU_FFT_RTP_TEST)' \
	'$$(LIB_FILES_ROCM_TEST)' >> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_ROCFFT_CPU_FFT_RTP_TEST)' \
	'$$(MOD_ROCFFT_CPU_FFT_RTP_TEST)' \
	'-L. $$(LIBS_ROCM_TEST) $$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)

lib_name:

mod_list:
	@echo  MOD_ROCFFT_PRT_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_ROCFFT_PRT_TEST)   >> $(MAKENAME)
	@echo  MOD_ROCFFT_RTP_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_ROCFFT_RTP_TEST)   >> $(MAKENAME)
	@echo  MOD_ROCFFT_CPU_FFT_PRT_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_ROCFFT_CPU_FFT_PRT_TEST)   >> $(MAKENAME)
	@echo  MOD_ROCFFT_CPU_FFT_RTP_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_ROCFFT_CPU_FFT_RTP_TEST)   >> $(MAKENAME)
	@echo ' ' >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(ROCFFT_TEST_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

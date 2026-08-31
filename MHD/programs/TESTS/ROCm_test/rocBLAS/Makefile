#
#
#

ROCBLAS_TEST_DIR = $$(ROCM_TEST_DIR)/rocBLAS

TARGET_ROCM_DGEMM_TEST = $$(TEST_BUILD_DIR)/test_rocBLAS_DGEMM
TARGET_HIPM_DGEMM_TEST = $$(TEST_BUILD_DIR)/test_hipBLAS_DGEMM

CURRENTDIR = $(shell pwd)
SOURCES = $(shell ls *.f90 *.F90)

MOD_ROCM_DGEMM_TEST = \
test_rocBLAS_DGEMM.o \
\
m_DGEMM_test_size.o \
DGEMM_test_omp_target.o \
DGEMM_test_openmp.o

MOD_HIPM_DGEMM_TEST = \
test_HIPmBLAS_DGEMM.o \
\
m_DGEMM_test_size.o \
DGEMM_test_omp_target.o \
DGEMM_test_openmp.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'ROCBLAS_TEST_DIR = $(ROCBLAS_TEST_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_ROCM_DGEMM_TEST = $(TARGET_ROCM_DGEMM_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_HIPM_DGEMM_TEST = $(TARGET_HIPM_DGEMM_TEST)' \
	>> $(MAKENAME)
	@echo ' ' >> $(MAKENAME)

target_task:
	@echo rocBLAS_tests: \
	'$$(TARGET_ROCM_DGEMM_TEST)  ' \
	'$$(TARGET_ROCFFT_PRT_TEST)              $$(TARGET_ROCFFT_RTP_TEST)       ' \
	'$$(TARGET_ROCFFT_CPU_FFT_PRT_TEST)      $$(TARGET_ROCFFT_CPU_FFT_RTP_TEST) ' \
	 >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_ROCM_DGEMM_TEST): $$(MOD_ROCM_DGEMM_TEST) $$(LIB_FILES_ROCM_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_ROCM_DGEMM_TEST)' \
	'$$(MOD_ROCM_DGEMM_TEST)' \
	'-L. $$(LIBS_ROCM_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)

lib_name:

mod_list:
	@echo  MOD_ROCM_DGEMM_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_ROCM_DGEMM_TEST) >> $(MAKENAME)
	@echo  MOD_HIPM_DGEMM_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_HIPM_DGEMM_TEST) >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(ROCBLAS_TEST_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

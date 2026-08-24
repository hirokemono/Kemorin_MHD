#
#
#

ROCM_TEST_DIR = $$(TESTS_DIR)/ROCm_test

TARGET_ROCM_DGEMM_TEST = test_rocBLAS_DGEMM
TARGET_HIPM_DGEMM_TEST = test_hipBLAS_DGEMM

TARGET_CPLX_ROCFFT_PRT_TEST =     test_cplx_rocFFT_prt
TARGET_CPLX_ROCFFT_RTP_TEST =     test_cplx_rocFFT_rtp
TARGET_REAL_ROCFFT_PRT_TEST =     test_real_rocFFT_prt
TARGET_REAL_ROCFFT_RTP_TEST =     test_real_rocFFT_rtp
TARGET_OMP_ROCFFT_PRT_TEST =      test_OMP_rocFFT_prt
TARGET_OMP_ROCFFT_RTP_TEST =      test_OMP_rocFFT_rtp

TARGET_HALF_OMP_ROCFFT_PRT_TEST =  test_half_OMP_rocFFT_prt
TARGET_HALF_OMP_ROCFFT_RTP_TEST =  test_half_OMP_rocFFT_rtp
TARGET_HALF_REAL_ROCFFT_PRT_TEST = test_half_real_rocFFT_prt
TARGET_HALF_REAL_ROCFFT_RTP_TEST = test_half_real_rocFFT_rtp
TARGET_HALF_CPLX_ROCFFT_PRT_TEST = test_half_cplx_rocFFT_prt
TARGET_HALF_CPLX_ROCFFT_RTP_TEST = test_half_cplx_rocFFT_rtp

TARGET_OMP_ROCFFT_FFTW_RTP_TEST = test_OMP_rocFFT_FFTW_rtp

TARGET_SGL_CP_ROCFFT_PRT_TEST = test_sgl_cplx_rocFFT_prt
TARGET_SGL_CP_ROCFFT_RTP_TEST = test_sgl_cplx_rocFFT_rtp
TARGET_SGL_RL_ROCFFT_PRT_TEST = test_sgl_real_rocFFT_prt
TARGET_SGL_RL_ROCFFT_RTP_TEST = test_sgl_real_rocFFT_rtp

LIBS_ROCM_TEST =    \
'$$(LIB_MESH)'      \
'$$(LIB_FFTPACK)'

LIB_FILES_ROCM_TEST =   \
'$$(LIB_MESH_FILE)'     \
'$$(LIB_FFTPACK_FILE)'

ifdef FORTRAN_C_CONNECT
  LIBS_ROCM_TEST+=        '$$(LIB_KEMO_C)'
  LIB_FILES_ROCM_TEST+=   '$$(LIB_KEMO_C_FILE)'
endif

CURRENTDIR = $(shell pwd)
SOURCES = $(shell ls *.f90 *.F90)

MOD_ROCM_DGEMM_TEST = \
test_rocBLAS_DGEMM.o \
\
m_DGEMM_test_size.o \
DGEMM_test_omp_target.o \
DGEMM_test_openmp.o

MOD_HIPM_DGEMM_TEST = test_HIPmBLAS_DGEMM

MOD_CPLX_ROCFFT_PRT_TEST = \
t_parameters_FFT_tests.o \
t_ctl_data_4_FFT_tests.o \
t_fft_test_data.o \
test_cplx_rocFFT_prt.o

MOD_CPLX_ROCFFT_RTP_TEST = \
t_parameters_FFT_tests.o \
t_ctl_data_4_FFT_tests.o \
t_fft_test_data.o \
test_cplx_rocFFT_rtp.o

MOD_REAL_ROCFFT_PRT_TEST = \
t_parameters_FFT_tests.o \
t_ctl_data_4_FFT_tests.o \
t_fft_test_data.o \
test_real_rocFFT_prt.o

MOD_REAL_ROCFFT_RTP_TEST = \
t_parameters_FFT_tests.o \
t_ctl_data_4_FFT_tests.o \
t_fft_test_data.o \
test_real_rocFFT_rtp.o

MOD_OMP_ROCFFT_PRT_TEST = \
t_parameters_FFT_tests.o \
t_ctl_data_4_FFT_tests.o \
t_fft_test_data.o \
test_OMP_rocFFT_prt.o

MOD_OMP_ROCFFT_RTP_TEST = \
t_parameters_FFT_tests.o \
t_ctl_data_4_FFT_tests.o \
t_fft_test_data.o \
test_OMP_rocFFT_rtp.o

MOD_SGL_CP_ROCFFT_PRT_TEST = \
t_parameters_FFT_tests.o \
t_ctl_data_4_FFT_tests.o \
t_fft_test_data.o \
test_sgl_cplx_rocFFT_prt.o

MOD_SGL_CP_ROCFFT_RTP_TEST = \
t_parameters_FFT_tests.o \
t_ctl_data_4_FFT_tests.o \
t_fft_test_data.o \
test_sgl_cplx_rocFFT_rtp.o

MOD_SGL_RL_ROCFFT_PRT_TEST = \
t_parameters_FFT_tests.o \
t_ctl_data_4_FFT_tests.o \
t_fft_test_data.o \
test_sgl_real_rocFFT_prt.o

MOD_SGL_RL_ROCFFT_RTP_TEST = \
t_parameters_FFT_tests.o \
t_ctl_data_4_FFT_tests.o \
t_fft_test_data.o \
test_sgl_real_rocFFT_rtp.o

MOD_HALF_OMP_ROCFFT_PRT_TEST = \
t_parameters_FFT_tests.o \
t_ctl_data_4_FFT_tests.o \
t_fft_test_data.o \
test_half_OMP_rocFFT_prt.o

MOD_HALF_OMP_ROCFFT_RTP_TEST = \
t_parameters_FFT_tests.o \
t_ctl_data_4_FFT_tests.o \
t_fft_test_data.o \
test_half_OMP_rocFFT_rtp.o

MOD_HALF_REAL_ROCFFT_PRT_TEST = \
t_parameters_FFT_tests.o \
t_ctl_data_4_FFT_tests.o \
t_fft_test_data.o \
test_half_real_rocFFT_prt.o

MOD_HALF_REAL_ROCFFT_RTP_TEST = \
t_parameters_FFT_tests.o \
t_ctl_data_4_FFT_tests.o \
t_fft_test_data.o \
test_half_real_rocFFT_rtp.o

MOD_HALF_CPLX_ROCFFT_PRT_TEST = \
t_parameters_FFT_tests.o \
t_ctl_data_4_FFT_tests.o \
t_fft_test_data.o \
test_half_cplx_rocFFT_prt.o

MOD_HALF_CPLX_ROCFFT_RTP_TEST = \
t_parameters_FFT_tests.o \
t_ctl_data_4_FFT_tests.o \
t_fft_test_data.o \
test_half_cplx_rocFFT_rtp.o

MOD_OMP_ROCFFT_FFTW_RTP_TEST = \
t_parameters_FFT_tests.o \
t_ctl_data_4_FFT_tests.o \
t_fft_test_data.o \
test_OMP_rocFFT_FFTW3_rtp.o

#
#  ------------------------------------------------------------------
#

dir_list:
	@echo 'ROCM_TEST_DIR = $(ROCM_TEST_DIR)' >> $(MAKENAME)

target_list:
	@echo 'TARGET_ROCM_DGEMM_TEST = $$(TEST_BUILD_DIR)/$(TARGET_ROCM_DGEMM_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_HIPM_DGEMM_TEST = $$(TEST_BUILD_DIR)/$(TARGET_HIPM_DGEMM_TEST)' \
	>> $(MAKENAME)
	@echo ' ' >> $(MAKENAME)
	@echo 'TARGET_CPLX_ROCFFT_PRT_TEST = $$(TEST_BUILD_DIR)/$(TARGET_CPLX_ROCFFT_PRT_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_CPLX_ROCFFT_RTP_TEST = $$(TEST_BUILD_DIR)/$(TARGET_CPLX_ROCFFT_RTP_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_REAL_ROCFFT_PRT_TEST = $$(TEST_BUILD_DIR)/$(TARGET_REAL_ROCFFT_PRT_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_REAL_ROCFFT_RTP_TEST = $$(TEST_BUILD_DIR)/$(TARGET_REAL_ROCFFT_RTP_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_OMP_ROCFFT_PRT_TEST = $$(TEST_BUILD_DIR)/$(TARGET_OMP_ROCFFT_PRT_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_OMP_ROCFFT_RTP_TEST = $$(TEST_BUILD_DIR)/$(TARGET_OMP_ROCFFT_RTP_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_HALF_OMP_ROCFFT_PRT_TEST =  $$(TEST_BUILD_DIR)/$(TARGET_HALF_OMP_ROCFFT_PRT_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_HALF_OMP_ROCFFT_RTP_TEST =  $$(TEST_BUILD_DIR)/$(TARGET_HALF_OMP_ROCFFT_RTP_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_HALF_REAL_ROCFFT_PRT_TEST = $$(TEST_BUILD_DIR)/$(TARGET_HALF_REAL_ROCFFT_PRT_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_HALF_REAL_ROCFFT_RTP_TEST = $$(TEST_BUILD_DIR)/$(TARGET_HALF_REAL_ROCFFT_RTP_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_HALF_CPLX_ROCFFT_PRT_TEST = $$(TEST_BUILD_DIR)/$(TARGET_HALF_CPLX_ROCFFT_PRT_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_HALF_CPLX_ROCFFT_RTP_TEST = $$(TEST_BUILD_DIR)/$(TARGET_HALF_CPLX_ROCFFT_RTP_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_OMP_ROCFFT_FFTW_RTP_TEST =  $$(TEST_BUILD_DIR)/$(TARGET_OMP_ROCFFT_FFTW_RTP_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_SGL_CP_ROCFFT_PRT_TEST= $$(TEST_BUILD_DIR)/$(TARGET_SGL_CP_ROCFFT_PRT_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_SGL_CP_ROCFFT_RTP_TEST= $$(TEST_BUILD_DIR)/$(TARGET_SGL_CP_ROCFFT_RTP_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_SGL_RL_ROCFFT_PRT_TEST= $$(TEST_BUILD_DIR)/$(TARGET_SGL_RL_ROCFFT_PRT_TEST)' \
	>> $(MAKENAME)
	@echo 'TARGET_SGL_RL_ROCFFT_RTP_TEST= $$(TEST_BUILD_DIR)/$(TARGET_SGL_RL_ROCFFT_RTP_TEST)' \
	>> $(MAKENAME)
	@echo >> $(MAKENAME)

target_task:
	@echo ROCm_tests: \
	'$$(TARGET_ROCM_DGEMM_TEST)  ' \
	'$$(TARGET_CPLX_ROCFFT_PRT_TEST)       $$(TARGET_CPLX_ROCFFT_RTP_TEST)' \
	'$$(TARGET_REAL_ROCFFT_PRT_TEST)       $$(TARGET_REAL_ROCFFT_RTP_TEST)' \
	'$$(TARGET_OMP_ROCFFT_PRT_TEST)        $$(TARGET_OMP_ROCFFT_RTP_TEST)' \
	'$$(TARGET_HALF_OMP_ROCFFT_PRT_TEST)   $$(TARGET_HALF_OMP_ROCFFT_RTP_TEST)' \
	'$$(TARGET_HALF_REAL_ROCFFT_PRT_TEST)  $$(TARGET_HALF_REAL_ROCFFT_RTP_TEST)' \
	'$$(TARGET_HALF_CPLX_ROCFFT_PRT_TEST)  $$(TARGET_HALF_CPLX_ROCFFT_RTP_TEST)' \
	'$$(TARGET_OMP_ROCFFT_FFTW_RTP_TEST)' \
	'$$(TARGET_SGL_CP_ROCFFT_PRT_TEST)     $$(TARGET_SGL_CP_ROCFFT_RTP_TEST)' \
	'$$(TARGET_SGL_RL_ROCFFT_PRT_TEST)     $$(TARGET_SGL_RL_ROCFFT_RTP_TEST)' \
	 >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_ROCM_DGEMM_TEST): $$(MOD_ROCM_DGEMM_TEST) $$(LIB_FILES_ROCM_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_ROCM_DGEMM_TEST)' \
	'$$(MOD_ROCM_DGEMM_TEST)' \
	'-L. $$(LIBS_ROCM_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_CPLX_ROCFFT_PRT_TEST): $$(MOD_CPLX_ROCFFT_PRT_TEST) $$(LIB_FILES_ROCM_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_CPLX_ROCFFT_PRT_TEST)' \
	'$$(MOD_CPLX_ROCFFT_PRT_TEST)' \
	'-L. $$(LIBS_ROCM_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_CPLX_ROCFFT_RTP_TEST): $$(MOD_CPLX_ROCFFT_RTP_TEST) $$(LIB_FILES_ROCM_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_CPLX_ROCFFT_RTP_TEST)' \
	'$$(MOD_CPLX_ROCFFT_RTP_TEST)' \
	'-L. $$(LIBS_ROCM_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_REAL_ROCFFT_PRT_TEST): $$(MOD_REAL_ROCFFT_PRT_TEST) $$(LIB_FILES_ROCM_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_REAL_ROCFFT_PRT_TEST)' \
	'$$(MOD_REAL_ROCFFT_PRT_TEST)' \
	'-L. $$(LIBS_ROCM_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_REAL_ROCFFT_RTP_TEST): $$(MOD_REAL_ROCFFT_RTP_TEST) $$(LIB_FILES_ROCM_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_REAL_ROCFFT_RTP_TEST)' \
	'$$(MOD_REAL_ROCFFT_RTP_TEST)' \
	'-L. $$(LIBS_ROCM_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_OMP_ROCFFT_PRT_TEST): $$(MOD_OMP_ROCFFT_PRT_TEST) $$(LIB_FILES_ROCM_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_OMP_ROCFFT_PRT_TEST)' \
	'$$(MOD_OMP_ROCFFT_PRT_TEST)' \
	'-L. $$(LIBS_ROCM_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_OMP_ROCFFT_RTP_TEST): $$(MOD_OMP_ROCFFT_RTP_TEST) $$(LIB_FILES_ROCM_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_OMP_ROCFFT_RTP_TEST)' \
	'$$(MOD_OMP_ROCFFT_RTP_TEST)' \
	'-L. $$(LIBS_ROCM_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_HALF_OMP_ROCFFT_PRT_TEST): $$(MOD_HALF_OMP_ROCFFT_PRT_TEST) $$(LIB_FILES_ROCM_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_HALF_OMP_ROCFFT_PRT_TEST)' \
	'$$(MOD_HALF_OMP_ROCFFT_PRT_TEST)' \
	'-L. $$(LIBS_ROCM_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_HALF_OMP_ROCFFT_RTP_TEST): $$(MOD_HALF_OMP_ROCFFT_RTP_TEST) $$(LIB_FILES_ROCM_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_HALF_OMP_ROCFFT_RTP_TEST)' \
	'$$(MOD_HALF_OMP_ROCFFT_RTP_TEST)' \
	'-L. $$(LIBS_ROCM_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_HALF_REAL_ROCFFT_PRT_TEST): $$(MOD_HALF_REAL_ROCFFT_PRT_TEST) $$(LIB_FILES_ROCM_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_HALF_REAL_ROCFFT_PRT_TEST)' \
	'$$(MOD_HALF_REAL_ROCFFT_PRT_TEST)' \
	'-L. $$(LIBS_ROCM_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_HALF_REAL_ROCFFT_RTP_TEST): $$(MOD_HALF_REAL_ROCFFT_RTP_TEST) $$(LIB_FILES_ROCM_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_HALF_REAL_ROCFFT_RTP_TEST)' \
	'$$(MOD_HALF_REAL_ROCFFT_RTP_TEST)' \
	'-L. $$(LIBS_ROCM_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_HALF_CPLX_ROCFFT_PRT_TEST): $$(MOD_HALF_CPLX_ROCFFT_PRT_TEST) $$(LIB_FILES_ROCM_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_HALF_CPLX_ROCFFT_PRT_TEST)' \
	'$$(MOD_HALF_CPLX_ROCFFT_PRT_TEST)' \
	'-L. $$(LIBS_ROCM_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_HALF_CPLX_ROCFFT_RTP_TEST): $$(MOD_HALF_CPLX_ROCFFT_RTP_TEST) $$(LIB_FILES_ROCM_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_HALF_CPLX_ROCFFT_RTP_TEST)' \
	'$$(MOD_HALF_CPLX_ROCFFT_RTP_TEST)' \
	'-L. $$(LIBS_ROCM_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_OMP_ROCFFT_FFTW_RTP_TEST): $$(MOD_OMP_ROCFFT_FFTW_RTP_TEST) $$(LIB_FILES_ROCM_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_OMP_ROCFFT_FFTW_RTP_TEST)' \
	'$$(MOD_OMP_ROCFFT_FFTW_RTP_TEST)' \
	'-L. $$(LIBS_ROCM_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SGL_CP_ROCFFT_PRT_TEST): $$(MOD_SGL_CP_ROCFFT_PRT_TEST) $$(LIB_FILES_ROCM_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SGL_CP_ROCFFT_PRT_TEST)' \
	'$$(MOD_SGL_CP_ROCFFT_PRT_TEST)' \
	'-L. $$(LIBS_ROCM_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SGL_CP_ROCFFT_RTP_TEST): $$(MOD_SGL_CP_ROCFFT_RTP_TEST) $$(LIB_FILES_ROCM_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SGL_CP_ROCFFT_RTP_TEST)' \
	'$$(MOD_SGL_CP_ROCFFT_RTP_TEST)' \
	'-L. $$(LIBS_ROCM_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SGL_RL_ROCFFT_PRT_TEST): $$(MOD_SGL_RL_ROCFFT_PRT_TEST) $$(LIB_FILES_ROCM_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SGL_RL_ROCFFT_PRT_TEST)' \
	'$$(MOD_SGL_RL_ROCFFT_PRT_TEST)' \
	'-L. $$(LIBS_ROCM_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)
	@echo '$$(TARGET_SGL_RL_ROCFFT_RTP_TEST): $$(MOD_SGL_RL_ROCFFT_RTP_TEST) $$(LIB_FILES_ROCM_TEST)' \
	>> $(MAKENAME)
	@echo '	''$$(F90)' '$$(F90OPTFLAGS)' '$$(F90CPPFLAGS)' \
	-o '$$(TARGET_SGL_RL_ROCFFT_RTP_TEST)' \
	'$$(MOD_SGL_RL_ROCFFT_RTP_TEST)' \
	'-L. $$(LIBS_ROCM_TEST)' \
	'$$(F90LIBS)' >> $(MAKENAME)
	@echo '' >> $(MAKENAME)



lib_name:
	@echo  LIBS_ROCM_TEST=  \\          >> $(MAKENAME)
	@echo  $(LIBS_ROCM_TEST)            >> $(MAKENAME)
	@echo  LIB_FILES_ROCM_TEST=  \\     >> $(MAKENAME)
	@echo  $(LIB_FILES_ROCM_TEST)       >> $(MAKENAME)
	@echo  '#' >> $(MAKENAME)

mod_list:
	@echo  MOD_ROCM_DGEMM_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_ROCM_DGEMM_TEST) >> $(MAKENAME)
	@echo  MOD_HIPM_DGEMM_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_HIPM_DGEMM_TEST) >> $(MAKENAME)
	@echo ' ' >> $(MAKENAME)
	@echo  MOD_CPLX_ROCFFT_PRT_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_CPLX_ROCFFT_PRT_TEST) >> $(MAKENAME)
	@echo  MOD_CPLX_ROCFFT_RTP_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_CPLX_ROCFFT_RTP_TEST) >> $(MAKENAME)
	@echo  MOD_REAL_ROCFFT_PRT_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_REAL_ROCFFT_PRT_TEST) >> $(MAKENAME)
	@echo  MOD_REAL_ROCFFT_RTP_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_REAL_ROCFFT_RTP_TEST) >> $(MAKENAME)
	@echo  MOD_OMP_ROCFFT_PRT_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_OMP_ROCFFT_PRT_TEST) >> $(MAKENAME)
	@echo  MOD_OMP_ROCFFT_RTP_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_OMP_ROCFFT_RTP_TEST) >> $(MAKENAME)
	@echo  MOD_SGL_CP_ROCFFT_PRT_TEST=  \\   >> $(MAKENAME)
	@echo  $(MOD_SGL_CP_ROCFFT_PRT_TEST)     >> $(MAKENAME)
	@echo  MOD_SGL_CP_ROCFFT_RTP_TEST=  \\   >> $(MAKENAME)
	@echo  $(MOD_SGL_CP_ROCFFT_RTP_TEST)     >> $(MAKENAME)
	@echo  MOD_SGL_RL_ROCFFT_PRT_TEST=  \\   >> $(MAKENAME)
	@echo  $(MOD_SGL_RL_ROCFFT_PRT_TEST)     >> $(MAKENAME)
	@echo  MOD_SGL_RL_ROCFFT_RTP_TEST=  \\   >> $(MAKENAME)
	@echo  $(MOD_SGL_RL_ROCFFT_RTP_TEST)     >> $(MAKENAME)
	@echo  MOD_HALF_OMP_ROCFFT_PRT_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_HALF_OMP_ROCFFT_PRT_TEST)   >> $(MAKENAME)
	@echo  MOD_HALF_OMP_ROCFFT_RTP_TEST=  \\ >> $(MAKENAME)
	@echo  $(MOD_HALF_OMP_ROCFFT_RTP_TEST)   >> $(MAKENAME)
	@echo  MOD_HALF_REAL_ROCFFT_PRT_TEST= \\ >> $(MAKENAME)
	@echo  $(MOD_HALF_REAL_ROCFFT_PRT_TEST)  >> $(MAKENAME)
	@echo  MOD_HALF_REAL_ROCFFT_RTP_TEST= \\ >> $(MAKENAME)
	@echo  $(MOD_HALF_REAL_ROCFFT_RTP_TEST)  >> $(MAKENAME)
	@echo  MOD_HALF_CPLX_ROCFFT_PRT_TEST= \\ >> $(MAKENAME)
	@echo  $(MOD_HALF_CPLX_ROCFFT_PRT_TEST)  >> $(MAKENAME)
	@echo  MOD_HALF_CPLX_ROCFFT_RTP_TEST= \\ >> $(MAKENAME)
	@echo  $(MOD_HALF_CPLX_ROCFFT_RTP_TEST)  >> $(MAKENAME)
	@echo  MOD_OMP_ROCFFT_FFTW_RTP_TEST= \\  >> $(MAKENAME)
	@echo  $(MOD_OMP_ROCFFT_FFTW_RTP_TEST)   >> $(MAKENAME)
	@echo   >> $(MAKENAME)

depends:
	@$(MAKE_MOD_DEP) $(MAKENAME).depends '$$(ROCM_TEST_DIR)' $(SOURCES)


clean:
	rm -f *.o *.mod *~ *.par *.diag *.a

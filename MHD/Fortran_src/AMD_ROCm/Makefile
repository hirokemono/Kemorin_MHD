CFLAGS ?= -lrocfft

TEST_SOURCES  := $(shell find . -name "*.f03" | sed "s,^\./,,g")
TEST_APPS     := compare_fft_test test_FFTPACK5 test_FFTW3 test_ROCmfft_sgl_rtp test_ROCmfft_ptr test_ROCmfft_rtp
RUN_TEST_APPS := $(addprefix run., $(TEST_APPS))

ROCFFT_DIR =   ROCfft_test
FFT_WRAP_DIR = FFT_wrapper
TEST_SRC_DIR = FFT_test

# FC= hipfc -v
# FC= amdflang -fopenmp

# FC= flang
FC= mpif90
F90= $(FC)


CFLAGS= -O3 -g  -fopenmp -fopenmp-target-fast \
  --offload-arch=gfx942:xnack+ -mcmodel=medium \
  -I/opt/rocm-7.2.0/include/hipfort/amdgcn
# -I/system/apps/rhel9/dev/rocm-afar/7702-drop-6.1.0/include/hipfort/amdgcn

 LDFLAGS= \
 -L/opt/rocm-7.2.0/lib -lrocfft -lhipfort-amdgcn  -lamdhip64
#  -L/system/apps/rhel9/dev/rocm-afar/7702-drop-6.1.0/lib -lhipfort-amdgcn \
#  -L/opt/rocm-6.3.3/lib -lrocfft -lrocblas -lamdhip64 \
#  -L/home/hrmatsui/local/amd/lib -lopenblas

F90OPTFLAGS= $(CFLAGS)


MOD_FFTPACK_TEST= \
m_precision.o m_constants.o m_machine_parameter.o normalize_for_FFTPACK.o multi_pin_FFTPACK_smp.o multi_pout_FFTPACK_smp.o calypso_multi_fftpack.o t_FFTPACK5_wrapper.o m_FFT_size.o t_fft_test_data.o test_FFTPACK5.o

MOD_FFW3_F_TEST= \
m_precision.o m_constants.o m_machine_parameter.o m_fftw_parameters.o normalize_for_FFTW.o multi_pin_FFTW3_smp.o multi_pout_FFTW3_smp.o calypso_multi_FFTW3.o t_multi_FFTW_wrapper.o m_FFT_size.o t_fft_test_data.o test_FFTW3.o

MOD_CMPR_FFT_TEST= \
m_precision.o m_constants.o m_machine_parameter.o compare_FFT_test.o m_FFT_size.o t_fft_test_data.o

#   .PHONY: build run clean $(TEST_APPS) $(RUN_TEST_APPS)

all: compare_fft_test test_FFTPACK5 test_FFTW3 test_ROCmfft_sgl_rtp test_ROCmfft_sgl_prt test_ROCmfft_ptr test_ROCmfft_rtp

rocfft: rocfft.o m_precision.o
	$(FC)  $(CFLAGS) $(LDFLAGS) $< -o $@


compare_fft_test: $(MOD_CMPR_FFT_TEST)
	$(F90) $(F90OPTFLAGS) $(F90CPPFLAGS) -o $@ $(MOD_CMPR_FFT_TEST) $(F90LIBS)

test_ROCmfft_rtp: test_ROCmfft_rtp.o t_fft_test_data.o normalize_for_OMP_FFTW.o m_FFT_size.o m_machine_parameter.o m_constants.o m_precision.o fftpack5.11d.o
	$(FC)  $(CFLAGS) $(LDFLAGS) $^ -o $@

test_ROCmfft_ptr: test_ROCmfft_ptr.o t_ROCmFFT_wrapper.o t_fft_test_data.o normalize_for_OMP_FFTW.o m_FFT_size.o m_machine_parameter.o m_constants.o m_precision.o fftpack5.11d.o
	$(FC)  $(CFLAGS) $(LDFLAGS) $^ -o $@

test_ROCmfft_sgl_rtp: test_ROCmfft_sgl_rtp.o normalize_for_FFTW.o t_fft_test_data.o m_FFT_size.o m_machine_parameter.o m_constants.o m_precision.o fftpack5.11d.o
	$(FC)  $(CFLAGS) $(LDFLAGS) $^ -o $@

test_ROCmfft_sgl_prt: test_ROCmfft_sgl_prt.o t_fft_test_data.o m_FFT_size.o m_machine_parameter.o m_constants.o m_precision.o fftpack5.11d.o
	$(FC)  $(CFLAGS) $(LDFLAGS) $^ -o $@

test_FFTPACK5: $(MOD_FFTPACK_TEST) fftpack5.11d.o
	$(FC)  $(CFLAGS) $(LDFLAGS) $^ -o $@

test_FFTW3: $(MOD_FFW3_F_TEST)
	$(FC)  $(CFLAGS) $(LDFLAGS) -L/home/hrmatsui/local/amd/lib -lfftw3 $^ -o $@



rocfft.o: rocfft.f03 m_precision.o
	$(FC)  $(CFLAGS) -c $<

test_ROCmfft_rtp.o: $(ROCFFT_DIR)/test_ROCmfft_rtp.f90 t_fft_test_data.o normalize_for_OMP_FFTW.o m_FFT_size.o m_machine_parameter.o m_constants.o m_precision.o
	$(FC)  $(CFLAGS) -c $<
test_ROCmfft_ptr.o: $(ROCFFT_DIR)/test_ROCmfft_ptr.f90 t_ROCmFFT_wrapper.o t_fft_test_data.o normalize_for_OMP_FFTW.o m_FFT_size.o m_machine_parameter.o m_constants.o m_precision.o
	$(FC)  $(CFLAGS) -c $<
test_ROCmfft_sgl_rtp.o: $(ROCFFT_DIR)/test_ROCmfft_sgl_rtp.f90 normalize_for_FFTW.o t_fft_test_data.o m_FFT_size.o m_machine_parameter.o m_constants.o m_precision.o
	$(FC)  $(CFLAGS) -c $<
test_ROCmfft_sgl_prt.o: $(ROCFFT_DIR)/test_ROCmfft_sgl_prt.f90 t_fft_test_data.o m_FFT_size.o m_machine_parameter.o m_constants.o m_precision.o
	$(FC)  $(CFLAGS) -c $<

test_FFTW3.o: $(TEST_SRC_DIR)/test_FFTW3.f90 m_precision.o m_constants.o m_machine_parameter.o m_FFT_size.o t_multi_FFTW_wrapper.o t_fft_test_data.o calypso_multi_FFTW3.o
	$(F90) -c $(F90OPTFLAGS) $<
test_FFTPACK5.o: $(TEST_SRC_DIR)/test_FFTPACK5.f90 m_precision.o m_constants.o m_machine_parameter.o m_FFT_size.o t_fft_test_data.o t_FFTPACK5_wrapper.o calypso_multi_fftpack.o
	$(F90) -c $(F90OPTFLAGS) $<
compare_FFT_test.o: $(TEST_SRC_DIR)/compare_FFT_test.f90 m_precision.o m_constants.o m_machine_parameter.o t_fft_test_data.o
	$(F90) -c $(F90OPTFLAGS) $<

t_ROCmFFT_wrapper.o: $(ROCFFT_DIR)/t_ROCmFFT_wrapper.f90 m_constants.o m_precision.o
	$(F90) -c $(F90OPTFLAGS) $<

m_fftw_parameters.o: $(FFT_WRAP_DIR)/m_fftw_parameters.f90 m_precision.o
	$(F90) -c $(F90OPTFLAGS) $<

t_multi_FFTW_wrapper.o: $(FFT_WRAP_DIR)/t_multi_FFTW_wrapper.f90 m_precision.o m_constants.o m_fftw_parameters.o multi_pout_FFTW3_smp.o
	$(F90) -c $(F90OPTFLAGS) $<
calypso_multi_FFTW3.o: $(FFT_WRAP_DIR)/calypso_multi_FFTW3.f90 m_precision.o m_constants.o t_multi_FFTW_wrapper.o multi_pin_FFTW3_smp.o
	$(F90) -c $(F90OPTFLAGS) $<
multi_pin_FFTW3_smp.o: $(FFT_WRAP_DIR)/multi_pin_FFTW3_smp.f90 m_precision.o m_constants.o m_fftw_parameters.o normalize_for_FFTW.o
	$(F90) -c $(F90OPTFLAGS) $<
multi_pout_FFTW3_smp.o: $(FFT_WRAP_DIR)/multi_pout_FFTW3_smp.f90 m_precision.o m_constants.o m_fftw_parameters.o normalize_for_FFTW.o
	$(F90) -c $(F90OPTFLAGS) $<

t_FFTPACK5_wrapper.o: $(FFT_WRAP_DIR)/t_FFTPACK5_wrapper.f90 m_precision.o m_constants.o multi_pout_FFTPACK_smp.o
	$(F90) -c $(F90OPTFLAGS) $<
calypso_multi_fftpack.o: $(FFT_WRAP_DIR)/calypso_multi_fftpack.f90 m_precision.o m_constants.o t_FFTPACK5_wrapper.o multi_pout_FFTPACK_smp.o multi_pin_FFTPACK_smp.o
	$(F90) -c $(F90OPTFLAGS) $<
multi_pin_FFTPACK_smp.o: $(FFT_WRAP_DIR)/multi_pin_FFTPACK_smp.f90 m_precision.o m_constants.o normalize_for_FFTPACK.o
	$(F90) -c $(F90OPTFLAGS) $<
multi_pout_FFTPACK_smp.o: $(FFT_WRAP_DIR)/multi_pout_FFTPACK_smp.f90 m_precision.o m_constants.o normalize_for_FFTPACK.o
	$(F90) -c $(F90OPTFLAGS) $<

normalize_for_FFTPACK.o: $(FFT_WRAP_DIR)/normalize_for_FFTPACK.f90 m_precision.o m_constants.o
	$(F90) -c $(F90OPTFLAGS) $<
normalize_for_FFTW.o: $(FFT_WRAP_DIR)/normalize_for_FFTW.f90 m_precision.o m_constants.o
	$(F90) -c $(F90OPTFLAGS) $<
normalize_for_OMP_FFTW.o: $(FFT_WRAP_DIR)/normalize_for_OMP_FFTW.f90 m_precision.o m_constants.o
	$(F90) -c $(F90OPTFLAGS) $<

t_fft_test_data.o: $(TEST_SRC_DIR)/t_fft_test_data.f90 m_machine_parameter.o m_machine_parameter.o m_constants.o m_precision.o
	$(FC)  $(CFLAGS) -c $<
m_FFT_size.o: $(TEST_SRC_DIR)/m_FFT_size.f90 m_precision.o
	$(FC)  $(CFLAGS) -c $<

m_machine_parameter.o: $(TEST_SRC_DIR)/m_machine_parameter.f90 m_precision.o
	$(FC)  $(CFLAGS) -c $<

m_constants.o: $(TEST_SRC_DIR)/m_constants.f90 m_precision.o
	$(FC)  $(CFLAGS) -c $<

m_precision.o: $(TEST_SRC_DIR)/m_precision.f90
	$(FC)  $(CFLAGS) -c $<

fftpack5.11d.o: $(TEST_SRC_DIR)/fftpack5.11d.f90
	$(FC)  $(CFLAGS) -c $<


$(RUN_TEST_APPS): run.%: %
	HIP_TRACE_API=1 ./$^

run: $(RUN_TEST_APPS) 

clean:
	rm -f $(TEST_APPS) test_FFTPACK5 *.o *.mod

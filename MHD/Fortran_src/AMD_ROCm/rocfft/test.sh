#!/bin/bash
#PBS -P NIFS24KISC010
#PBS -q B_dev
#####       Set number of nodes
#PBS -l select=1
#####       Set elapsed time
#PBS -l walltime=00:05:00

# module load rocm/7.2.0
# module load openmpi/5.0.7/rocm7.2.0_amdflang

source ~/AMD_modules.sh
#   export DYLD_LIBRARY_PATH=/opt/rocm-7.2.0/lib:$DYLD_LIBRARY_PATH
cd $PBS_O_WORKDIR

# make clean
# make compare_fft_test test_FFTPACK5

./test_FFTPACK5
./test_FFTW3
./test_ROCmfft_sgl_rtp
./test_ROCmfft_sgl_prt
./test_ROCmfft_rtp
./test_ROCmfft_ptr

./compare_fft_test 'ref/fftpack_test.dat' fftpack_test.dat
./compare_fft_test 'ref/fftpack_test.dat' mul_fftw_test.dat
./compare_fft_test 'ref/fftpack_test.dat' sgl_rtp_ROCmfft_test.dat
./compare_fft_test 'ref/fftpack_test.dat' sgl_prt_ROCmfft_test.dat
./compare_fft_test 'ref/fftpack_test.dat' rtp_ROCmfft_test.dat
./compare_fft_test 'ref/fftpack_test.dat' prt_ROCmfft_test.dat


# rocprof-compute profile --name rocprof_result -- ./test_ROCmfft_ptr
# rocprof-compute analyze -p workloads/sample/MI300/


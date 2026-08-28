!>@file   test_half_OMP_rocFFT_prt.f90
!!@brief  program test_half_OMP_rocFFT_prt
!!
!!@author H. Matsui
!!@date Programmed in March., 2026
!
!>@brief Test program of OpenMP rocFFT and FFTPACK
!!      with inner series array
!!
!!@verbatim
!! ----------------------------------------------------------------------
!!     Control file example
!! ----------------------------------------------------------------------
!!  begin FFT_test_ctl
!!    output_file_name    'prt_OMP_rocFFT_FFTPACK_test.dat'
!!
!!    FFT_length_ctl         128
!!    num_series_ctl          24
!!    num_test_loop_ctl       10
!!  end FFT_test_ctl
!! ----------------------------------------------------------------------
!!@endverbatim
      program test_half_OMP_rocFFT_prt
!
      use iso_c_binding
      use omp_lib
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_fft_test_data
      use t_parameters_FFT_tests
      use t_ctl_data_4_FFT_tests
      use t_multi_rocFFT_wrapper
      use t_FFTPACK5_wrapper
      use pin_OMP_rocFFT_FFTPACK
!
      implicit none
!
      character(len = kchara), parameter                                &
     &             :: test_name = 'prt_OpenMP_rocFFT_FFTPACK'
      character(len = kchara), parameter                                &
     &             :: def_fname = 'prt_OMP_rocFFT_FFTPACK_test.dat'
!
      character(len = kchara) :: ctl_file_name
      type(FFT_tests_ctl), save :: fft_c1
      type(FFT_test_parameters), save :: fft_test_p1
!
      type(fft_test_data) :: ft1
      real(kind = kreal) :: start, elapsed(9)
!
      type(calypso_rocFFT_params), target :: fwd
      type(calypso_rocFFT_params), target :: bwd
      type(calypso_rocFFT_work), target :: WK_rocFFT
      type(working_FFTPACK) :: WK_FFTPACK_T
!
      integer(kind = kint) :: ncomp_GPU
      integer(kind = kint) :: ncomp_CPU
      integer(kind = kint) :: i, nd, icou
!
!
      write(*,'(a)') '-----  Test prt shared OpenMP rocFFT  -----'
!
      call default_FFT_test_parameters(test_name, def_fname,            &
     &                                 fft_test_p1)
      if(command_argument_count() .ge. 1) then
        call get_command_argument(1, ctl_file_name)
        call read_control_file_FFT_tests(ctl_file_name, fft_c1)
        call set_FFT_test_parameters(fft_c1, fft_test_p1)
      else
        write(*,*) 'No control file name in command: Use default'
      end if
!
      ncomp_GPU = fft_test_p1%ratio_rocFFT * fft_test_p1%Ncomp_test
      ncomp_CPU = fft_test_p1%Ncomp_test - ncomp_GPU
      call init_fft_test_data                                           &
     &   (fft_test_p1%Ncomp_test, fft_test_p1%Nfft_test, ft1)
      call swap_fft_test_input_to_pin(ft1)
!
!   Initialize Fourier transform
      start = OMP_GET_WTIME()
      call init_pin_OMP_rocFFT_FFTPACK(ft1%nfld, Ncomp_GPU, Ncomp_CPU,  &
     &    ft1%ngrd, np_smp, fwd, bwd, WK_rocFFT, WK_FFTPACK_T)
      elapsed(1) = OMP_GET_WTIME() - start
!
      elapsed(2:4) = zero
      do icou = 1, fft_test_p1%Nloop_test + 1
        if(mod(icou, 20) .eq. 0) write(*,*) 'loop count: ', icou
!
        start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%s_k(1:ft1%ngrd,1:ft1%nfld) = ft1%org(1:ft1%ngrd,1:ft1%nfld)
!$omp end parallel workshare
        elapsed(3) = elapsed(3) + OMP_GET_WTIME() - start
!
!   Forward transform
        call pin_fwd_OMP_rocFFT_FFTPACK(ft1%nfld, Ncomp_CPU,            &
     &      fwd, WK_rocFFT, WK_FFTPACK_T, ft1%s_k(1,1), elapsed(2:5))
!
        start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%f_x(1:ft1%ngrd,1:ft1%nfld) = ft1%s_k(1:ft1%ngrd,1:ft1%nfld)
!$omp end parallel workshare
        elapsed(3) = elapsed(3) + OMP_GET_WTIME() - start
!
!   Backword transform
        call pin_bwd_OMP_rocFFT_FFTPACK(ft1%nfld, Ncomp_CPU,            &
     &      bwd, WK_rocFFT, WK_FFTPACK_T, ft1%f_x(1,1), elapsed(2:5))
        if(icou .eq. 1) elapsed(6:9) = elapsed(2:5)
      end do
      elapsed(6) = elapsed(2) - elapsed(6)
      elapsed(8) = elapsed(4) - elapsed(8)
      elapsed(9) = elapsed(5) - elapsed(9)
!
!   Finalize
      start = OMP_GET_WTIME()
      call calypso_rocFFT_fin(fwd, bwd, WK_rocFFT)
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
  10  continue
      if(fft_test_p1%nloop_test .eq. 1) then
        call swap_fft_test_data_to_pout(ft1)
        call write_fft_test_data(fft_test_p1%file_name, ft1)
      end if
      call dealloc_fft_test_data(ft1)
!
      call write_sharing_FFT_test_elapsed(fft_test_p1, elapsed)
!
      stop 'finish'
!
      end program test_half_OMP_rocFFT_prt
!
! mpif90 --offload-arch=gfx942 -mcmodel=medium -mcmodel=medium -O3 -g -fopenmp -fopenmp-target-fast  -I. -I/home/hrmatsui/src_kemo/work -I/opt/rocm-7.2.0/include/hipfort/amdgcn -DPNG_OUTPUT -DZLIB_IO -DFFTW3 -D_AMD_ROCM_ -o ./test_half_OMP_rocFFT_prt test_half_OMP_rocFFT_prt.f90 /home/hrmatsui/src_kemo/work/m_FFT_size.o /home/hrmatsui/src_kemo/work/t_fft_test_data.o -L/home/hrmatsui/src_kemo/work -lkemo_core -lkemo_c -L/home/hrmatsui/local/amd/lib -lpng -L/home/hrmatsui/local/amd/lib -lz -L/home/hrmatsui/local/amd/lib -lfftw3 -L/opt/rocm-7.2.0/lib -lrocfft -lrocblas -lhipfort-amdgcn -lamdhip64

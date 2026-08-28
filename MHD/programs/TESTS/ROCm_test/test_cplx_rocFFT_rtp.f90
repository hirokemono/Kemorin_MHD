!>@file   test_cplx_rocFFT_rtp.f90
!!@brief  program test_cplx_rocFFT_rtp
!!
!!@author H. Matsui
!!@date Programmed in MArch., 2026
!
!>@brief Test program of complex rocFFT
!!      with outer series array
!!
!!@verbatim
!! ----------------------------------------------------------------------
!!     Control file example
!! ----------------------------------------------------------------------
!!  begin FFT_test_ctl
!!    output_file_name    'rtp_rocFFT_test.dat'
!!
!!    FFT_length_ctl         128
!!    num_series_ctl          24
!!    num_test_loop_ctl       10
!!  end FFT_test_ctl
!!
!! ----------------------------------------------------------------------
!!@endverbatim
      program test_cplx_rocFFT_rtp
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
      use multi_pout_complex_rocFFT
!
      implicit none
!
      character(len = kchara), parameter                                &
     &             :: test_name = 'rtp_complex_rocFFT'
      character(len = kchara), parameter                                &
     &             :: def_fname = 'rtp_rocFFT_test.dat'
!
      character(len = kchara) :: ctl_file_name
      type(FFT_tests_ctl), save :: fft_c1
      type(FFT_test_parameters), save :: fft_test_p1
!
      type(fft_test_data) :: ft1
!
      type(calypso_rocFFT_params), target :: fwd
      type(calypso_rocFFT_params), target :: bwd
      type(calypso_rocFFT_work), target :: WK_fft
!
      real(kind = kreal) :: start, finish, elapsed(5)
      integer(kind = kint) :: i, nd
      integer(kind = kint) :: icou
!
!
      write(*,'(a)') '-----  Test rtp complex rocFFT  -----'
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
      call init_fft_test_data                                           &
     &   (fft_test_p1%Ncomp_test, fft_test_p1%Nfft_test, ft1)
!
!   Initialize Fourier transform
      start = OMP_GET_WTIME()
      call calypso_pout_rocFFT_init                                     &
     &   (fft_test_p1%Ncomp_test, fft_test_p1%Ncomp_test,               &
     &    fft_test_p1%Nfft_test, fwd, bwd, WK_fft)
      elapsed(1) = OMP_GET_WTIME() - start
!
      elapsed(2:3) = zero
      do icou = 1, fft_test_p1%Nloop_test + 1
        if(mod(icou, 20) .eq. 0) write(*,*) 'loop count: ', icou
!
        start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%s_k(1:ft1%nfld,1:ft1%ngrd) = ft1%org(1:ft1%nfld,1:ft1%ngrd)
!$omp end parallel workshare
        elapsed(3) = elapsed(3) + OMP_GET_WTIME() - start
!
!   Forward transform
        call multi_pout_fwd_rocFFT_r2c(ft1%nfld, fwd, WK_fft,           &
     &      ft1%s_k(1,1), elapsed(2), elapsed(3))
!
        start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%f_x(1:ft1%nfld,1:ft1%ngrd) = ft1%s_k(1:ft1%nfld,1:ft1%ngrd)
!$omp end parallel workshare
        elapsed(3) = elapsed(3) + OMP_GET_WTIME() - start
!
!   Backword transform
        call multi_pout_bwd_rocFFT_c2r(ft1%nfld, bwd, WK_fft,           &
     &      ft1%f_x(1,1), elapsed(2), elapsed(3))
        if(icou .eq. 1) elapsed(4) = elapsed(2)
      end do
      elapsed(4) = elapsed(2) - elapsed(4)
!
!   Finalize
      start = OMP_GET_WTIME()
      call calypso_rocFFT_fin(fwd, bwd, WK_fft)
      elapsed(1) = elapsed(1) + OMP_GET_WTIME() - start
!
  10  continue
      if(fft_test_p1%Nloop_test .eq. 1)  then
        call write_fft_test_data(fft_test_p1%file_name, ft1)
      end if
      call dealloc_fft_test_data(ft1)
!
      call write_rocFFT_test_elapsed(fft_test_p1, elapsed(1))
!
      stop 'finish'
      end program test_cplx_rocFFT_rtp

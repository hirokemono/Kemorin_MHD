!>@file   test_prt_select_FFT.f90
!!@brief  module test_prt_select_FFT
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!!      Modified in Aug., 2026
!
!> @brief Test program of FFT with inner series array
!!
!!@verbatim
!! ----------------------------------------------------------------------
!!     Control file example
!! ----------------------------------------------------------------------
!!  begin FFT_test_ctl
!!    output_file_name    'prt_FFT_test.dat'
!!
!!    FFT_length_ctl         128
!!    num_series_ctl          24
!!    num_test_loop_ctl       10
!!  end FFT_test_ctl
!! ----------------------------------------------------------------------
!!@endverbatim
      program test_prt_select_FFT
!
      use omp_lib
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_fft_test_data
      use t_FFT_selector
      use t_parameters_FFT_tests
      use t_ctl_data_4_FFT_tests
!
      use select_multi_FFT_init
      use multi_FFT_select
!
      implicit none
!
      character(len = kchara), parameter                                &
     &                        :: test_name = 'prt_FFT'
      character(len = kchara), parameter                                &
     &                        :: def_fname = 'prt_FFT_test.dat'
!
      character(len = kchara) :: ctl_file_name
      type(FFT_tests_ctl), save :: fft_c1
      type(FFT_test_parameters), save :: fft_test_p1
!
      type(working_FFTs), target :: WK_FFTs
      type(fft_test_data) :: ft1
      integer(kind = kint) :: iloop = 0
!
!
      write(*,'(a)') '-----  Test FFT with inner series loop -----'
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
      call swap_fft_test_input_to_pin(ft1)
!
      ft1%start = OMP_GET_WTIME()
      call sel_multi_FFT_init(fft_test_p1%iflag_FFT,                    &
     &    np_smp, ft1%nstack, ft1%nfld, ft1%ngrd, WK_FFTs)
      ft1%elapsed(1) = ft1%elapsed(1) + OMP_GET_WTIME() - ft1%start
!
      do iloop = 1, fft_test_p1%nloop_test
        if(mod(iloop, 20) .eq. 0) write(*,*) 'loop count: ', iloop
!
        ft1%start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%s_k(1:ft1%ngrd,1:ft1%nfld) = ft1%org(1:ft1%ngrd,1:ft1%nfld)
!$omp end parallel workshare
        ft1%elapsed(3) = ft1%elapsed(3) + OMP_GET_WTIME() - ft1%start
!
        call select_pin_fwd_FFTs                                        &
     &     (fft_test_p1%iflag_FFT, ft1%nfld, ft1%ngrd, ft1%s_k,         &
     &      WK_FFTs, ft1%elapsed(2), ft1%elapsed(3))
!
        ft1%start = OMP_GET_WTIME()
!$omp parallel workshare
        ft1%f_x(1:ft1%ngrd,1:ft1%nfld) = ft1%s_k(1:ft1%ngrd,1:ft1%nfld)
!$omp end parallel workshare
        ft1%elapsed(3) = ft1%elapsed(3) + OMP_GET_WTIME() - ft1%start
!
        call select_pin_bwd_FFTs                                        &
     &     (fft_test_p1%iflag_FFT, ft1%nfld, ft1%ngrd, ft1%f_x,         &
     &      WK_FFTs, ft1%elapsed(2), ft1%elapsed(3))
      end do
!
      call sel_multi_FFT_fin(fft_test_p1%iflag_FFT, np_smp, WK_FFTs)
!
      if(fft_test_p1%nloop_test .eq. 1) then
        call swap_fft_test_data_to_pout(ft1)
        call write_fft_test_data(fft_test_p1%file_name, ft1)
      end if
      call dealloc_fft_test_data(ft1)
!
      call write_fft_test_elapsed(fft_test_p1, ft1%elapsed(1))
!
      stop
      end program test_prt_select_FFT


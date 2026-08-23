!>@file   test_prt_ISPACK3_FFT.f90
!!@brief  module test_prt_ISPACK3_FFT
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!!      Modified in Aug., 2026
!
!> @brief Test program of ISPACK V.0.97 with outer series array
!!
!!@verbatim
!! ----------------------------------------------------------------------
!!     Control file example
!! ----------------------------------------------------------------------
!!  begin FFT_test_ctl
!!    output_file_name    'prt_ISPACK3_test.dat'
!!
!!    FFT_length_ctl         128
!!    num_series_ctl          24
!!    num_test_loop_ctl       10
!!  end FFT_test_ctl
!!
!! ----------------------------------------------------------------------
!!@endverbatim
      program test_prt_ISPACK3_FFT
!
      use omp_lib
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_fft_test_data
      use t_ispack3_FFT_wrapper
      use t_parameters_FFT_tests
      use t_ctl_data_4_FFT_tests
      use calypso_multi_ispack3
      use transfer_to_long_integers
!
      implicit none
!
      character(len = kchara), parameter                                &
     &                        :: test_name = 'prt_ISPACK_v3'
      character(len = kchara), parameter                                &
     &                        :: def_fname = 'prt_ISPACK3_test.dat'
!
      character(len = kchara) :: ctl_file_name
      type(FFT_tests_ctl), save :: fft_c1
      type(FFT_test_parameters), save :: fft_test_p1
!
      type(working_ISPACK3) ::  WK_ISPACK3_t
      type(fft_test_data) :: ft0
      integer(kind = kint) :: iloop = 0
!
!
      write(*,'(a)') '-----  Test ISPACK3 with inner series loop -----'
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
      iflag_debug = 1
      call init_fft_test_data                                           &
     &   (fft_test_p1%Ncomp_test, fft_test_p1%Nfft_test, ft0)
      call swap_fft_test_input_to_pin(ft0)
!
      ft0%start = OMP_GET_WTIME()
      call init_wk_ispack3_t                                            &
     &   (np_smp, ft0%nstack, cast_long(ft0%ngrd), WK_ISPACK3_t)
      ft0%elapsed(1) = ft0%elapsed(1) + OMP_GET_WTIME() - ft0%start
!
      do iloop = 1, fft_test_p1%nloop_test
        if(mod(iloop, 20) .eq. 0) write(*,*) 'loop count: ', iloop
!
        ft0%start = OMP_GET_WTIME()
!$omp parallel workshare
        ft0%s_k(1:ft0%ngrd,1:ft0%nfld) = ft0%org(1:ft0%ngrd,1:ft0%nfld)
!$omp end parallel workshare
        ft0%elapsed(3) = ft0%elapsed(3) + OMP_GET_WTIME() - ft0%start
!
        call calypso_multi_pin_FXRTFA                                   &
     &     (cast_long(ft0%nfld), cast_long(ft0%ngrd), ft0%s_k,          &
     &      WK_ISPACK3_t, ft0%elapsed(2), ft0%elapsed(3))
!
        ft0%start = OMP_GET_WTIME()
!$omp parallel workshare
        ft0%f_x(1:ft0%ngrd,1:ft0%nfld) = ft0%s_k(1:ft0%ngrd,1:ft0%nfld)
!$omp end parallel workshare
        ft0%elapsed(3) = ft0%elapsed(3) + OMP_GET_WTIME() - ft0%start
!
        call calypso_multi_pin_FXRTBA                                   &
     &     (cast_long(ft0%nfld), cast_long(ft0%ngrd), ft0%f_x,          &
     &      WK_ISPACK3_t, ft0%elapsed(2), ft0%elapsed(3))
      end do
!
      if(fft_test_p1%nloop_test .eq. 1) then
        call swap_fft_test_data_to_pout(ft0)
        call write_fft_test_data(fft_test_p1%file_name, ft0)
      end if
      call dealloc_fft_test_data(ft0)
!
      call write_fft_test_elapsed(fft_test_p1, ft0%elapsed(1))
!
      stop 'finish'
      end program test_prt_ISPACK3_FFT


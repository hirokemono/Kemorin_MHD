!>@file   test_prt_FFTW3.f90
!!@brief  module test_prt_FFTW3
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!!      Modified in Aug., 2026
!
!> @brief Test program of FFTW3 with inner series array
!!
!!@verbatim
!! ----------------------------------------------------------------------
!!     Control file example
!! ----------------------------------------------------------------------
!!  begin FFT_test_ctl
!!    output_file_name    'prt_fftw_test.dat'
!!
!!    FFT_length_ctl         128
!!    num_series_ctl          24
!!    num_test_loop_ctl       10
!!  end FFT_test_ctl
!!
!! ----------------------------------------------------------------------
!!@endverbatim
      program test_prt_FFTW3
!
      use omp_lib
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_FFT_size
!
      use t_multi_FFTW_wrapper
      use t_fft_test_data
      use t_ctl_data_4_FFT_tests
      use calypso_multi_FFTW3
!
      implicit none
!
      character(len = kchara) :: ctl_file_name
      type(FFT_tests_ctl), save :: fft_c1
!
      character(len = kchara), parameter                                &
     &                        :: def_file_name = 'prt_fftw_test.dat'
!
      character(len = kchara) :: file_name = def_file_name
      integer(kind = kint) :: nfft_test =  ngrid
      integer(kind = kint) :: ncomp_test = n_field
      integer(kind = kint) :: nloop_test = n_loop
!
      type(working_mul_FFTW) :: WK_MUL_FFTW_t
      type(fft_test_data) :: ft3
      integer(kind = kint) :: iloop = 0
!
!
!
      if(command_argument_count() .ge. 1) then
        call get_command_argument(1, ctl_file_name)
        call read_control_file_FFT_tests(ctl_file_name, fft_c1)
!
        if(fft_c1%FFT_test_output_ctl%iflag .gt. 0) then
          file_name = fft_c1%FFT_test_output_ctl%charavalue
        end if
        if(fft_c1%FFT_length_ctl%iflag .gt. 0) then
          nfft_test = fft_c1%FFT_length_ctl%intvalue
        end if
        if(fft_c1%num_series_ctl%iflag .gt. 0) then
          nloop_test = fft_c1%num_series_ctl%intvalue
        end if
        if(fft_c1%loop_counts_ctl%iflag .gt. 0) then
          nloop_test = fft_c1%loop_counts_ctl%intvalue
        end if
!
      else
        write(*,*) 'No control file name in command: Use default'
      end if
!
      write(*,'(a)') '-----  Test FFTW  -----'
      iflag_debug = 1
      call init_fft_test_data(ncomp_test, nfft_test, ft3)
      call swap_fft_test_input_to_pin(ft3)
!
      ft3%start = OMP_GET_WTIME()
      call init_FFTW_mul_type                                           &
     &   (np_smp, ft3%nstack, ft3%ngrd, WK_MUL_FFTW_t)
      ft3%elapsed(1) = ft3%elapsed(1) + OMP_GET_WTIME() - ft3%start
!
      do iloop = 1, n_loop
        if(mod(iloop, 20) .eq. 0) write(*,*) 'loop count: ', iloop
!
        ft3%start = OMP_GET_WTIME()
!$omp parallel workshare
        ft3%s_k(1:ft3%ngrd,1:ft3%nfld) = ft3%org(1:ft3%ngrd,1:ft3%nfld)
!$omp end parallel workshare
        ft3%elapsed(3) = ft3%elapsed(3) + OMP_GET_WTIME() - ft3%start
!
        call calypso_multi_pin_fwd_FFTW3(np_smp, ft3%nstack,            &
     &      ft3%nfld, ft3%ngrd, ft3%s_k, WK_MUL_FFTW_t,                 &
     &      ft3%elapsed(2), ft3%elapsed(3))
!
        ft3%start = OMP_GET_WTIME()
!$omp parallel workshare
        ft3%f_x(1:ft3%ngrd,1:ft3%nfld) = ft3%s_k(1:ft3%ngrd,1:ft3%nfld)
!$omp end parallel workshare
        ft3%elapsed(3) = ft3%elapsed(3) + OMP_GET_WTIME() - ft3%start
!
        call calypso_multi_pin_bwd_FFTW3(np_smp, ft3%nstack,            &
     &      ft3%nfld, ft3%ngrd, ft3%f_x, WK_MUL_FFTW_t,                 &
     &      ft3%elapsed(2), ft3%elapsed(3))
      end do
!
      ft3%start = OMP_GET_WTIME()
      if(n_loop .eq. 1) then
        call swap_fft_test_data_to_pout(ft3)
        call write_fft_test_data(file_name, ft3)
      end if
      call dealloc_fft_test_data(ft3)
      ft3%elapsed(1) = ft3%elapsed(1) + OMP_GET_WTIME() - ft3%start
!
      write(*,'(a,i4)') 'Number of threads:  ', np_smp
      write(*, '(a,3i6)')  "Num (point, field, loop): ",                &
     &       nfft_test, ncomp_test, nloop_test
      write(*, '("Initialize:      ",1pE16.6e3)') ft3%elapsed(1)
      write(*, '("Wrapped FFTW3:   ",1pE16.6e3)') ft3%elapsed(2)
      write(*, '("Data copy:       ",1pE16.6e3)') ft3%elapsed(3)
      write(*,'(a)') '-----------------------------'
      write(*,'(a)') ' '
!
      stop 'finish'
      end program test_prt_FFTW3


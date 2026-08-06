!>@file   test_ISPACK1_FFT.f90
!!@brief  module test_ISPACK1_FFT
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
!!    output_file_name    'rtp_ISPACK1_test.dat'
!!
!!    FFT_length_ctl         128
!!    num_series_ctl          24
!!    num_test_loop_ctl       10
!!  end FFT_test_ctl
!!
!! ----------------------------------------------------------------------
!!@endverbatim
      program test_ISPACK1_FFT
!
      use omp_lib
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_FFT_size
!
      use t_fft_test_data
      use t_ispack_FFT_wrapper
      use t_ctl_data_4_FFT_tests
      use calypso_multi_ispack
!
      implicit none
!
      character(len = kchara) :: ctl_file_name
      type(FFT_tests_ctl), save :: fft_c1
!
      character(len = kchara), parameter                                &
     &                       :: def_file_name = 'rtp_ISPACK1_test.dat'
!
      character(len = kchara) :: file_name = def_file_name
      integer(kind = kint) :: nfft_test =  ngrid
      integer(kind = kint) :: ncomp_test = n_field
      integer(kind = kint) :: nloop_test = n_loop
!
      type(working_ISPACK) ::  WK_FFTPACK_t
      type(fft_test_data) :: ft0
      integer(kind = kint) :: iloop = 0
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
      iflag_debug = 1
      call init_fft_test_data(ncomp_test, nfft_test, ft0)
!
      ft0%start = OMP_GET_WTIME()
      call init_wk_ispack_t(np_smp, ft0%nstack, ft0%ngrd, WK_FFTPACK_t)
      ft0%elapsed(1) = ft0%elapsed(1) + OMP_GET_WTIME() - ft0%start
!
      do iloop = 1, nloop_test
        if(mod(iloop, 20) .eq. 0) write(*,*) 'loop count: ', iloop
!
        ft0%start = OMP_GET_WTIME()
!$omp parallel workshare
        ft0%s_k(1:ft0%nfld,1:ft0%ngrd) = ft0%org(1:ft0%nfld,1:ft0%ngrd)
!$omp end parallel workshare
        ft0%elapsed(3) = ft0%elapsed(3) + OMP_GET_WTIME() - ft0%start
!
        call FTTRUF_kemo_t(np_smp, ft0%nstack, ft0%nfld, ft0%ngrd,      &
     &      ft0%s_k, WK_FFTPACK_t, ft0%elapsed(2), ft0%elapsed(3))
!
        ft0%start = OMP_GET_WTIME()
!$omp parallel workshare
        ft0%f_x(1:ft0%nfld,1:ft0%ngrd) = ft0%s_k(1:ft0%nfld,1:ft0%ngrd)
!$omp end parallel workshare
        ft0%elapsed(3) = ft0%elapsed(3) + OMP_GET_WTIME() - ft0%start
!
        call FTTRUB_kemo_t(np_smp, ft0%nstack, ft0%nfld, ft0%ngrd,      &
     &      ft0%f_x, WK_FFTPACK_t, ft0%elapsed(2), ft0%elapsed(3))
      end do
!
      if(nloop_test .eq. 1) call write_fft_test_data(file_name, ft0)
      call dealloc_fft_test_data(ft0)
!
      write(*,'(a)') '-----  Test ISPACK1 with outer series loop -----'
      write(*,'(a,i4)') 'Number of threads:  ', np_smp
      write(*, '(a,3i6)')  "Num (point, field, loop): ",                &
     &       nfft_test, ncomp_test, nloop_test
      write(*, '("Initialize:      ",1pE16.6e3)') ft0%elapsed(1)
      write(*, '("ISPACK3:         ",1pE16.6e3)') ft0%elapsed(2)
      write(*, '("Data copy:       ",1pE16.6e3)') ft0%elapsed(3)
      write(*,'(a)') '-----------------------------'
      write(*,'(a)') ' '
!
      stop 'finish'
      end program test_ISPACK1_FFT


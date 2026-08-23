!>@file   t_parameters_FFT_tests.f90
!!@brief  module t_parameters_FFT_tests
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2011
!
!> @brief Control data structure for FFT tests
!!
!!@verbatim
!!@endverbatim
!
      module t_parameters_FFT_tests
!
      use m_precision
      use m_machine_parameter
!
      implicit none
!
!
      integer(kind = kint), parameter, private ::  ngrid =    128
      integer(kind = kint), parameter, private ::  n_field =    8
      integer(kind = kint), parameter, private ::  n_loop =     1
!
      type FFT_test_parameters
!>        FFT library name
        character(len = kchara) :: test_name
!>        output file name
        character(len = kchara) :: file_name
!>        Length of FFT
        integer(kind = kint) :: Nfft_test =  ngrid
!>        number of date series for FFT
        integer(kind = kint) :: Ncomp_test = n_field
!>        Number of iteration of test
        integer(kind = kint) :: Nloop_test = n_loop
      end type FFT_test_parameters
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine default_FFT_test_parameters(test_name, def_fname,      &
     &                                       fft_test_p)
!
      character(len = kchara), intent(in) :: test_name, def_fname
      type(FFT_test_parameters), intent(inout) :: fft_test_p
!
!
      fft_test_p%test_name = test_name
      fft_test_p%file_name = def_fname
!
      end subroutine default_FFT_test_parameters
!
!  ---------------------------------------------------------------------
!
      subroutine set_FFT_test_parameters(fft_c, fft_test_p)
!
      use t_ctl_data_4_FFT_tests
!
      type(FFT_tests_ctl), intent(in) :: fft_c
      type(FFT_test_parameters), intent(inout) :: fft_test_p
!
!
      if(fft_c%FFT_test_output_ctl%iflag .gt. 0) then
        fft_test_p%file_name = fft_c%FFT_test_output_ctl%charavalue
      end if
      if(fft_c%FFT_length_ctl%iflag .gt. 0) then
        fft_test_p%Nfft_test = fft_c%FFT_length_ctl%intvalue
      end if
      if(fft_c%num_series_ctl%iflag .gt. 0) then
        fft_test_p%Ncomp_test = fft_c%num_series_ctl%intvalue
      end if
      if(fft_c%loop_counts_ctl%iflag .gt. 0) then
        fft_test_p%nloop_test = fft_c%loop_counts_ctl%intvalue
      end if
!
      end subroutine set_FFT_test_parameters
!
!  ---------------------------------------------------------------------
!
      subroutine write_fft_test_elapsed(fft_test_p, elapsed)
!
      use m_machine_parameter
!
      type(FFT_test_parameters), intent(in) :: fft_test_p
      real(kind = kreal), intent(in) :: elapsed(3)
!
!
      write(*,'(a,i4)') 'Number of threads:  ', np_smp
      write(*, '(a,3i6)')  'Num (point, field, loop): ',                &
     &                   fft_test_p%nfft_test, fft_test_p%Ncomp_test,   &
     &                   fft_test_p%nloop_test
      write(*, '(a,1pE16.6e3)') 'Initialize:      ', elapsed(1)
      write(*, '(2a,1pE16.6e3)') trim(fft_test_p%test_name),            &
     &                                         ': ', elapsed(2)
      write(*, '(a,1pE16.6e3)') 'Data copy:       ', elapsed(3)
      write(*,'(a)') '-----------------------------'
      write(*,'(a)') ' '
!
      end subroutine write_fft_test_elapsed
!
!  ---------------------------------------------------------------------
!
      end module t_parameters_FFT_tests

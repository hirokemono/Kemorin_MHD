!>@file   m_FFT_selector.f90
!!@brief  module m_FFT_selector
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Selector of Fourier transform
!!
!!@verbatim
!!      subroutine set_fft_library_ctl(FFT_library_ctl)
!!      subroutine write_elapsed_4_FFT(i_mode, etime_fft)
!!      character(len = kchara) function chosen_fft_name(i_mode)
!!@endverbatim
!!
      module m_FFT_selector
!
      use m_precision
!
      implicit none
!
!>      Character flag to use FFTPACK5
      character(len = kchara), parameter :: hd_FFTPACK = 'FFTPACK'
!>      Character flag to use FFTW3
      character(len = kchara), parameter :: hd_FFTW =    'FFTW'
!>      Character flag to use FFTW3
      character(len = kchara), parameter :: hd_FFTW3 =    'fftw3'
!>      Character flag to use FFTW3 for each component
      character(len = kchara), parameter :: hd_FFTW_F =  'FFTW_FIELD'
!>      Character flag to use FFTW3 for each component
      character(len = kchara), parameter :: hd_FFTW3_F = 'fftw3_field'
!>      Character flag to use single transforms in FFTW3
      character(len = kchara), parameter :: hd_FFTW_S =  'FFTW_SINGLE'
!>      Character flag to use single transforms in FFTW3
      character(len = kchara), parameter :: hd_FFTW3_S = 'fftw3_single'
!>      Character flag to use FFTW3 for all components
      character(len = kchara), parameter                                &
     &          :: hd_FFTW_C =   'FFTW_COMPONENT'
!>      Character flag to use FFTW3 for all components
      character(len = kchara), parameter                                &
     &          :: hd_FFTW3_C =  'fftw3_component'
!>      Character flag to use ISPACK
      character(len = kchara), parameter :: hd_ISPACK =   'ISPACK'
!>      Character flag to use ISPACK
      character(len = kchara), parameter :: hd_ISPACK3 =  'ISPACK3'
!
!>      Character flag to use test FFT
      character(len = kchara), parameter :: hd_TEST_FFT =  'TEST'
!
!>      integer flag for undefined
      integer(kind = kint), parameter :: iflag_UNDEFINED_FFT =   0
!>      integer flag to use FFTPACK5
      integer(kind = kint), parameter :: iflag_FFTPACK =     1
!>      integer flag to use FFTPACK5
      integer(kind = kint), parameter :: iflag_FFTPACK =     1
!>      integer flag to use FFTPACK5
      integer(kind = kint), parameter :: iflag_FFTPACK =     1
!
!>      integer flag to use FFTW3
      integer(kind = kint), parameter :: iflag_FFTW =        11
!>      integer flag to use single transforms in FFTW3
      integer(kind = kint), parameter :: iflag_FFTW_SINGLE = 12
!>      integer flag to use single transforms in FFTW3
      integer(kind = kint), parameter :: iflag_FFTW_COMPONENT = 13
!>      integer flag to use FFTW3 for each component
!      integer(kind = kint), parameter :: iflag_FFTW_FIELD = 14
!>      integer flag to use ISPACK Ver.0.93
      integer(kind = kint), parameter :: iflag_ISPACK1 =     21
!>      integer flag to use ISPACK Ver. 3.01
      integer(kind = kint), parameter :: iflag_ISPACK3 =     31
!
!>      integer flag to use test FFT
      integer(kind = kint), parameter :: iflag_test_fft =    99
!
      private :: hd_FFTPACK, hd_FFTW, hd_FFTW3, hd_FFTW_S, hd_FFTW3_S
      private :: hd_ISPACK, hd_ISPACK3, hd_FFTW_F, hd_FFTW3_F
      private :: hd_FFTW_C, hd_FFTW3_C, hd_TEST_FFT
!
! ------------------------------------------------------------------
!
      contains
!
! ------------------------------------------------------------------
!
      subroutine set_fft_library_ctl(FFT_library_ctl, iflag_FFT)
!
      use skip_comment_f
!
      character(len = kchara), intent(in) :: FFT_library_ctl
      integer(kind = kint), intent(inout) :: iflag_FFT
!
!
      if     (cmp_no_case(FFT_library_ctl, hd_FFTPACK)) then
        iflag_FFT = iflag_FFTPACK
      else if(cmp_no_case(FFT_library_ctl, hd_ISPACK)) then
        iflag_FFT = iflag_ISPACK1
      else if(cmp_no_case(FFT_library_ctl, hd_ISPACK3)) then
        iflag_FFT = iflag_ISPACK3
      else if(cmp_no_case(FFT_library_ctl, hd_FFTW)                     &
     &     .or. cmp_no_case(FFT_library_ctl, hd_FFTW3)) then
        iflag_FFT = iflag_FFTW
      else if(cmp_no_case(FFT_library_ctl, hd_FFTW_S)                   &
     &     .or. cmp_no_case(FFT_library_ctl, hd_FFTW3_S)) then
        iflag_FFT = iflag_FFTW_SINGLE
      else if(cmp_no_case(FFT_library_ctl, hd_FFTW_C)                   &
     &     .or. cmp_no_case(FFT_library_ctl, hd_FFTW3_C)) then
        iflag_FFT = iflag_FFTW_COMPONENT
!      else if(cmp_no_case(FFT_library_ctl, hd_FFTW_F)                  &
!     &     .or. cmp_no_case(FFT_library_ctl, hd_FFTW3_F)) then
!        iflag_FFT = iflag_FFTW_FIELD
      else if(cmp_no_case(FFT_library_ctl, hd_TEST_FFT)) then
        iflag_FFT = iflag_test_fft
      end if
!
      end subroutine set_fft_library_ctl
!
! ------------------------------------------------------------------
!
      subroutine write_elapsed_4_FFT(i_mode, etime_fft)
!
      integer(kind = kint), intent(in) :: i_mode
      real(kind = kreal), intent(in) :: etime_fft
!
      if     (i_mode .eq. iflag_FFTPACK) then
        write(*,*) 'elapsed by FFTPACK (',                              &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
!
      else if(i_mode .eq. iflag_FFTW) then
        write(*,*) 'elapsed by FFTW3 for each component (',             &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
      else if(i_mode .eq. iflag_FFTW_SINGLE) then
        write(*,*) 'elapsed by single FFTW3 (',                         &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
      else if(i_mode .eq. iflag_FFTW_COMPONENT) then
        write(*,*) 'elapsed by FFTW3 for all component (',              &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
!
      else if(i_mode .eq. iflag_ISPACK1) then
        write(*,*) 'elapsed by ISPACK V0.93 (',                         &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
      else if(i_mode .eq. iflag_ISPACK3) then
        write(*,*) 'elapsed by ISPACK V3.0.1 (',                        &
     &            trim(chosen_fft_name(i_mode)), '): ', etime_fft
      end if
!
      end subroutine write_elapsed_4_FFT
!
! ------------------------------------------------------------------
!
      character(len = kchara) function chosen_fft_name(i_mode)
!
      integer(kind = kint), intent(in) :: i_mode
!
      if     (i_mode .eq. iflag_FFTPACK) then
        chosen_fft_name = hd_FFTPACK
!
      else if(i_mode .eq. iflag_FFTW) then
        chosen_fft_name = hd_FFTW
      else if(i_mode .eq. iflag_FFTW_SINGLE) then
        chosen_fft_name = hd_FFTW_S
      else if(i_mode .eq. iflag_FFTW_COMPONENT) then
        chosen_fft_name = hd_FFTW_C
!
      else if(i_mode .eq. iflag_ISPACK1) then
        chosen_fft_name = hd_ISPACK
      else if(i_mode .eq. iflag_ISPACK3) then
        chosen_fft_name = hd_ISPACK3
      else if(i_mode .eq. iflag_test_fft) then
        chosen_fft_name = hd_TEST_FFT
      end if
!
      end function chosen_fft_name
!
! ------------------------------------------------------------------
!
      end module m_FFT_selector

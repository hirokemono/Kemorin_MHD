!>@file   m_rocFFT_labels.f90
!!@brief  module m_rocFFT_labels
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2026
!
!>@brief  Control labels for rocFFT
!!
!!@verbatim
!!      subroutine init_rocFFT_mode_flags()
!!      integer(kind = kint) function find_set_rocFFT_flag(label)
!!        character(len = kchara), intent(in) :: label
!!      character(len = kchara) function find_rocFFT_label(iflag_fft)
!!        integer(kind = kint), intent(in) :: iflag_fft
!!      subroutine check_rocFFT_mode_flags(id_file)
!!        integer(kind = kint), intent(in) :: id_file
!!   ------------------------------------------------------------------
!!    FFT Package lists
!!      rocFFT, rocFFT_complex:  AMD rocFFT
!!   ------------------------------------------------------------------
!!    FFT size flags
!!
!|      once:         Call FFT once for all data transform
!|      domain:       Call FFT once for each spherical harmonic mode
!|      component:    Call FFT once for each components
!|      single:       Call FFT for each transform
!!   ------------------------------------------------------------------
!!@endverbatim
      module m_rocFFT_labels
!
      use m_precision
      use m_FFT_selector
      use m_complex_rocFFT_labels
      use m_real_rocFFT_labels
      use m_OMP_rocFFT_labels
      use t_multi_flag_labels
!
      implicit none
!
!>      Character flag to use rocFFT
      character(len = kchara), parameter, private                       &
     &                                   :: hd_rocFFT =     'rocFFT'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_rocFFT_mode_flags()
!
      type(multi_flag_labels) :: rocFFT_base_flags
!
!
      call init_multi_flags_by_one_label(hd_rocFFT, rocFFT_base_flags)
      call init_rocFFT_r2c_flags(rocFFT_base_flags)
      call init_rocFFT_r2r_flags(rocFFT_base_flags)
      call init_OMP_rocFFT_flags(rocFFT_base_flags)
      call dealloc_multi_flags(rocFFT_base_flags)
!
      end subroutine init_rocFFT_mode_flags
!
! ----------------------------------------------------------------------
!
      integer(kind = kint) function find_set_rocFFT_flag(label)
!
      character(len = kchara), intent(in) :: label
!
      integer(kind = kint) :: iflag_fft
!
      iflag_fft = find_set_rocFFT_r2c_flag(label)
      if(iflag_fft .lt. 0) iflag_fft = find_set_rocFFT_r2r_flag(label)
      if(iflag_fft .lt. 0) iflag_fft = find_set_OMP_rocFFT_flag(label)
      find_set_rocFFT_flag = iflag_fft
!
      end function find_set_rocFFT_flag
!
! ----------------------------------------------------------------------
!
      character(len = kchara) function find_rocFFT_label(iflag_fft)
!
      integer(kind = kint), intent(in) :: iflag_fft
!
      character(len = kchara) :: tmpchara
!
      tmpchara = 'NONE'
      if((iflag_fft/10) .eq. (iflag_rocFFT/10)) then
        tmpchara = find_rocFFT_r2c_label(iflag_fft)
      else if((iflag_fft/10) .eq. (iflag_real_rocFFT/10)) then
        tmpchara = find_rocFFT_r2r_label(iflag_fft)
      else if((iflag_fft/10) .eq. (iflag_OMP_rocFFT/10)) then
        tmpchara = find_OMP_rocFFT_label(iflag_fft)
      end if
      find_rocFFT_label = tmpchara
!
      end function find_rocFFT_label
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_rocFFT_mode_flags(id_file)
!
      integer(kind = kint), intent(in) :: id_file
!
      call check_rocFFT_r2c_mode_flags(id_file)
      write(id_file,*) ''
      call check_rocFFT_r2r_mode_flags(id_file)
      write(id_file,*) ''
      call check_OMP_rocFFT_mode_flags(id_file)
      write(id_file,*) ''
!
      end subroutine check_rocFFT_mode_flags
!
! ----------------------------------------------------------------------
!
      end module m_rocFFT_labels

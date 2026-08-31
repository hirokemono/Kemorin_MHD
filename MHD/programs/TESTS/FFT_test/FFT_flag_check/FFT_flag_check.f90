!>@file   FFT_flag_check.F90
!!@brief  module FFT_flag_check
!!
!!@author H. Matsui
!!@date Programmed in Aug., 2026
!
!>@brief  FFT control label check
!!
      program FFT_flag_check
!
      use m_FFT_labels
!
      implicit none
!
!
      call init_FFT_mode_flags()
      call check_FFT_mode_flags(6)
!
      end program FFT_flag_check


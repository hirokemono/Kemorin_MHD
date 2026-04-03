!>@file   m_FFT_size.f90
!!@brief  module m_FFT_size
!!
!!@author H. Matsui
!!@date Programmed in March, 2026
!
!> @brief Set data size for FFT tests
!!
      module m_FFT_size
!
      use m_precision
!
      integer(kind = kint), parameter ::  ngrid =   2048
      integer(kind = kint), parameter ::  n_field = 1024
      integer(kind = kint), parameter ::  n_loop =   200
!
      end module m_FFT_size

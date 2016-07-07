!>@file   m_rms_4_sph_spectr.f90
!!@brief  module m_rms_4_sph_spectr
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2008
!
!>@brief Mean sqare data
!!
      module m_rms_4_sph_spectr
!
      use m_precision
      use t_rms_4_sph_spectr
!
      implicit none
!
!>      Structure of mean square data
      type(sph_mean_squares), save :: pwr1
!
      end module m_rms_4_sph_spectr

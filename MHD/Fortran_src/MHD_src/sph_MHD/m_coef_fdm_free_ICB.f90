!>@file   m_coef_fdm_free_ICB.f90
!!@brief  module m_coef_fdm_free_ICB
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Matrix to evaluate poloidal velocity and toroidal vorticity
!!       at CMB with free slip boundary
!!
      module m_coef_fdm_free_ICB
!
      use m_precision
!
      use t_coef_fdm2_MHD_boundaries
!
      implicit none
!
      type(fdm2_free_slip), save :: fdm2_free_ICB1
!
      end module m_coef_fdm_free_ICB

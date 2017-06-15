!>@file   m_coef_fdm_free_CMB.f90
!!@brief  module m_coef_fdm_free_CMB
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Matrix to evaluate poloidal velocity and toroidal vorticity
!!       at CMB with free slip boundary
!!
!
      module m_coef_fdm_free_CMB
!
      use m_precision
!
      use t_coef_fdm2_MHD_boundaries
      use cal_inverse_small_matrix
!
      implicit none
!
      type(fdm2_free_slip), save :: fdm2_free_CMB1
!
      end module m_coef_fdm_free_CMB

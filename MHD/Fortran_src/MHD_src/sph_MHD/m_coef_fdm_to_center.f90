!>@file   m_coef_fdm_to_center.f90
!!@brief  module m_coef_fdm_to_center
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Matrix to evaluate radial derivative
!!       toward center
!!
!!
      module m_coef_fdm_to_center
!
      use m_precision
!
      use t_coef_fdm2_MHD_boundaries
!
      implicit none
!
!
      type(fdm2_center_mat), save :: fdm2_center1
!fdm2_center1%dmat_fix_fld
!
      end module m_coef_fdm_to_center

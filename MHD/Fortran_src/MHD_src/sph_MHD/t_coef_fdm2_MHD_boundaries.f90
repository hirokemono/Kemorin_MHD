!>@file   t_coef_fdm2_MHD_boundaries.f90
!!@brief  module t_coef_fdm2_MHD_boundaries
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Matrix to evaluate poloidal velocity and toroidal vorticity
!!       at CMB with free slip boundary
!!
!!@verbatim
!!      subroutine check_coef_fdm_fix_dr_2ctr(fdm2_center)
!!        type(fdm2_ICB_vpol), intent(in) :: fdm2_center
!!
!!     Matrix for derivatives at the next of center with fixed field
!!      dfdr(1) =      fdm2_center%dmat_fix_fld(-1,2) * d_center(0)
!!                   + fdm2_center%dmat_fix_fld( 0,2) * d_rj(1)
!!                   + fdm2_center%dmat_fix_fld( 1,2) * d_rj(2)
!!      d2fdr2(1) =    fdm2_center%dmat_fix_fld(-1,3) * d_center(0)
!!                   + fdm2_center%dmat_fix_fld( 0,3) * d_rj(1)
!!                   + fdm2_center%dmat_fix_fld( 1,3) * d_rj(2)
!!
!!     Matrix to evaluate field at center fixed radial derivative
!!      (Only used for l = m = 0 component of scalar)
!!      d_center(0) =fdm2_center%dmat_fix_dr(-1,1) * dfdr(0)
!!                 + fdm2_center%dmat_fix_dr( 0,1) * d_center(0)
!!                 + fdm2_center%dmat_fix_dr( 1,1) * d_rj(1)
!!      d2fdr2(0) =  fdm2_center%dmat_fix_dr(-1,3) * dfdr(0)
!!                 + fdm2_center%dmat_fix_dr( 0,3) * d_center(0)
!!                 + fdm2_center%dmat_fix_dr( 1,3) * d_rj(1)
!!
!!     Matrix to evaluate field at center fixed field
!!      (Only used for l = m = 0 component of scalar)
!!      dfdr(0) =    fdm2_center%dmat_fixed( 0,2) * d_center(0)
!!                 + fdm2_center%dmat_fixed( 1,2) * d_rj(1)
!!                 + fdm2_center%dmat_fixed( 2,2) * d_rj(2)
!!      d2fdr2(0) =  fdm2_center%dmat_fixed( 0,3) * d_center(0)
!!                 + fdm2_center%dmat_fixed( 1,3) * d_rj(1)
!!                 + fdm2_center%dmat_fixed( 2,3) * d_rj(2)
!!@endverbatim
!!
!!@n @param r_from_CMB(-3:0) radius from next points of CMB
!!@n @param radius(1:2) radius at two innermost grids
!
      module t_coef_fdm2_MHD_boundaries
!
      use m_precision
      use m_constants
!
      implicit none
!
!>      Structure for FDM matrix of center
      type fdm2_center_mat
!>        Matrix to evaluate radial derivative at center
!!        with fixed field
        real(kind = kreal) :: dmat_fix_fld(-1:1,3)
!
!>        Matrix to evaluate field at center
!!        with fixed radial derivative
        real(kind = kreal) :: dmat_fix_dr(-1:1,3)
!>        Matrix to evaluate field at center with fixed scalar
        real(kind = kreal) :: dmat_fixed( 0:2,3)
      end type fdm2_center_mat
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_coef_fdm_fix_dr_2ctr(fdm2_center)
!
      type(fdm2_center_mat), intent(in) :: fdm2_center
!
!
      write(50,*) ' fdm2_center%dmat_fix_fld'
      write(50,*) ' mat_fdm21,  mat_fdm22,  mat_fdm23'
      write(50,'(1p9E25.15e3)') fdm2_center%dmat_fix_fld(-1:1,2)
      write(50,*) ' mat_fdm31,  mat_fdm32,  mat_fdm33'
      write(50,'(1p9E25.15e3)') fdm2_center%dmat_fix_fld(-1:1,3)
!
      write(50,*) ' fdm2_center%dmat_fix_dr'
      write(50,*) ' mat_fdm21,  mat_fdm22,  mat_fdm23'
      write(50,'(1p9E25.15e3)') fdm2_center%dmat_fix_dr(-1:1,2)
      write(50,*) ' mat_fdm31,  mat_fdm32,  mat_fdm33'
      write(50,'(1p9E25.15e3)') fdm2_center%dmat_fix_dr(-1:1,3)
!
      write(50,*) ' fdm2_center%dmat_fixed'
      write(50,*) ' mat_fdm21,  mat_fdm22,  mat_fdm23'
      write(50,'(1p9E25.15e3)') fdm2_center%dmat_fixed(0:2,2)
      write(50,*) ' mat_fdm31,  mat_fdm32,  mat_fdm33'
      write(50,'(1p9E25.15e3)') fdm2_center%dmat_fixed(0:2,3)
!
      end subroutine check_coef_fdm_fix_dr_2ctr
!
! -----------------------------------------------------------------------
!
      end module t_coef_fdm2_MHD_boundaries

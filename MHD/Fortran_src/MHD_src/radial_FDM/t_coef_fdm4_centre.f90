!>@file   t_coef_fdm4_centre.f90
!!@brief  module t_coef_fdm4_centre
!!
!!@author H. Matsui
!!@date Programmed in May., 2013
!
!>@brief Matrix to evaluate radial derivative around center
!!
!!@verbatim
!!      subroutine check_4th_CTR_vpol_fdm(fdm4_vpol_CTR)
!!        type(fdm4_center_vpol), intent(in) :: fdm4_vpol_CTR
!!
!!      subroutine cal_fdm4_4th_vpol_center1(radius, fdm4_vpol_CTR)
!!      subroutine cal_fdm4_4th_vpol_center2(radius, fdm4_vpol_CTR)
!!        type(fdm4_center_vpol), intent(inout) :: fdm4_vpol_CTR
!!
!!   Matrix for poloidal velocity at next of center
!!      dfdr =      fdm4_vpol_CTR%dmat_vp1( 2,2) * d_rj(3)
!!                + fdm4_vpol_CTR%dmat_vp1( 1,2) * d_rj(2)
!!                + fdm4_vpol_CTR%dmat_vp1( 0,2) * d_rj(1)
!!      d2fdr2 =    fdm4_vpol_CTR%dmat_vp1( 2,3) * d_rj(3)
!!                + fdm4_vpol_CTR%dmat_vp1( 1,3) * d_rj(2)
!!                + fdm4_vpol_CTR%dmat_vp1( 0,3) * d_rj(1)
!!      d3fdr3 =    fdm4_vpol_CTR%dmat_vp1( 2,4) * d_rj(3)
!!                + fdm4_vpol_CTR%dmat_vp1( 1,4) * d_rj(2)
!!                + fdm4_vpol_CTR%dmat_vp1( 0,4) * d_rj(1)
!!      d4fdr4 =    fdm4_vpol_CTR%dmat_vp1( 2,5) * d_rj(3)
!!                + fdm4_vpol_CTR%dmat_vp1( 1,5) * d_rj(2)
!!                + fdm4_vpol_CTR%dmat_vp1( 0,5) * d_rj(1)
!!
!!   Matrix for poloidal velocity at 2nd next of center
!!      dfdr =      fdm4_vpol_CTR%dmat_vp2( 2,2) * d_rj(4)
!!                + fdm4_vpol_CTR%dmat_vp2( 1,2) * d_rj(3)
!!                + fdm4_vpol_CTR%dmat_vp2( 0,2) * d_rj(2)
!!                + fdm4_vpol_CTR%dmat_vp2(-1,2) * d_rj(1)
!!      d2fdr2 =    fdm4_vpol_CTR%dmat_vp2( 2,3) * d_rj(4)
!!                + fdm4_vpol_CTR%dmat_vp2( 1,3) * d_rj(3)
!!                + fdm4_vpol_CTR%dmat_vp2( 0,3) * d_rj(2)
!!                + fdm4_vpol_CTR%dmat_vp2(-1,3) * d_rj(1)
!!      d3fdr3 =    fdm4_vpol_CTR%dmat_vp2( 2,4) * d_rj(4)
!!                + fdm4_vpol_CTR%dmat_vp2( 1,4) * d_rj(3)
!!                + fdm4_vpol_CTR%dmat_vp2( 0,4) * d_rj(2)
!!                + fdm4_vpol_CTR%dmat_vp2(-1,4) * d_rj(1)
!!      d4fdr4 =    fdm4_vpol_CTR%dmat_vp2( 2,5) * d_rj(4)
!!                + fdm4_vpol_CTR%dmat_vp2( 1,5) * d_rj(3)
!!                + fdm4_vpol_CTR%dmat_vp2( 0,5) * d_rj(2)
!!                + fdm4_vpol_CTR%dmat_vp2(-1,5) * d_rj(1)
!!@endverbatim
!!
      module t_coef_fdm4_centre
!
      use m_precision
      use m_constants
!
      implicit none
!
!>      Structure for FDM matrix of center
      type fdm4_center_vpol
!>        Matrix to evaluate radial derivative at center
!!       for poloidal velocity
        real(kind = kreal) :: dmat_vp1(-2:2,1:5)
!>        Matrix to evaluate radial derivative at next of center
!!       for poloidal velocity
        real(kind = kreal) :: dmat_vp2(-2:2,1:5)
      end type fdm4_center_vpol
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine check_4th_CTR_vpol_fdm(fdm4_vpol_CTR)
!
      type(fdm4_center_vpol), intent(in) :: fdm4_vpol_CTR
!
!
      write(50,*) ' fdm4_vpol_CTR%dmat_vp0'
      write(50,*) 'matrix for dfdr'
      write(50,'(1p9E25.15e3)') fdm4_vpol_CTR%dmat_vp1(0:2,2)
      write(50,*) 'matrix for d2fdr2'
      write(50,'(1p9E25.15e3)') fdm4_vpol_CTR%dmat_vp1(0:2,3)
      write(50,*) 'matrix for d3fdr3'
      write(50,'(1p9E25.15e3)') fdm4_vpol_CTR%dmat_vp1(0:2,4)
      write(50,*) 'matrix for d4fdr4'
      write(50,'(1p9E25.15e3)') fdm4_vpol_CTR%dmat_vp1(0:2,5)
!
      write(50,*) ' fdm4_vpol_CTR%dmat_vp1'
      write(50,*) 'matrix for dfdr'
      write(50,'(1p9E25.15e3)') fdm4_vpol_CTR%dmat_vp2(-1:2,2)
      write(50,*) 'matrix for d2fdr2'
      write(50,'(1p9E25.15e3)') fdm4_vpol_CTR%dmat_vp2(-1:2,3)
      write(50,*) 'matrix for d3fdr3'
      write(50,'(1p9E25.15e3)') fdm4_vpol_CTR%dmat_vp2(-1:2,4)
      write(50,*) 'matrix for d4fdr4'
      write(50,'(1p9E25.15e3)') fdm4_vpol_CTR%dmat_vp2(-1:2,5)
!
      end subroutine check_4th_CTR_vpol_fdm
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_fdm4_4th_vpol_center1(radius, fdm4_vpol_CTR)
!
      use cal_inverse_small_matrix
!
      real(kind = kreal), intent(in) :: radius(3)
      type(fdm4_center_vpol), intent(inout) :: fdm4_vpol_CTR
!
!>      Work matrix to evaluate fdm4_vpol_CTR%dmat_fix_fld(-1:1,3)
!!@verbatim
!!      dfdr =      mat_fdm_ctr1_fix_5(2,1) * d_rj(1)
!!                + mat_fdm_ctr1_fix_5(2,2) * d_center(0)
!!                + mat_fdm_ctr1_fix_5(2,3) * d_rj(2)
!!                + mat_fdm_ctr1_fix_5(2,4) * dfdr(0)
!!                + mat_fdm_ctr1_fix_5(2,5) * d_rj(3)
!!      d2fdr2 =    mat_fdm_ctr1_fix_5(3,1) * d_rj(1)
!!                + mat_fdm_ctr1_fix_5(3,2) * d_center(0)
!!                + mat_fdm_ctr1_fix_5(3,3) * d_rj(2)
!!                + mat_fdm_ctr1_fix_5(3,4) * dfdr(0)
!!                + mat_fdm_ctr1_fix_5(3,5) * d_rj(3)
!!      d3fdr3 =    mat_fdm_ctr1_fix_5(4,1) * d_rj(1)
!!                + mat_fdm_ctr1_fix_5(4,2) * d_center(0)
!!                + mat_fdm_ctr1_fix_5(4,3) * d_rj(2)
!!                + mat_fdm_ctr1_fix_5(4,4) * dfdr(0)
!!                + mat_fdm_ctr1_fix_5(4,5) * d_rj(3)
!!      d4fdr4 =    mat_fdm_ctr1_fix_5(5,1) * d_rj(1)
!!                + mat_fdm_ctr1_fix_5(5,2) * d_center(0)
!!                + mat_fdm_ctr1_fix_5(5,3) * d_rj(2)
!!                + mat_fdm_ctr1_fix_5(5,4) * dfdr(0)
!!                + mat_fdm_ctr1_fix_5(5,5) * d_rj(3)
!!@endverbatim
      real(kind = kreal) :: mat_fdm_ctr1_fix_5(5,5)
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: mat_taylor_5(5,5)
      real(kind = kreal) :: dr_p1, dr_p2
!
!
      dr_p1 = radius(2) - radius(1)
      dr_p2 = radius(3) - radius(1)
!
      mat_taylor_5(1,1) =  one
      mat_taylor_5(1,2) =  zero
      mat_taylor_5(1,3) =  zero
      mat_taylor_5(1,4) =  zero
      mat_taylor_5(1,5) =  zero
!
      mat_taylor_5(2,1) =  one
      mat_taylor_5(2,2) = -radius(1)
      mat_taylor_5(2,3) =  radius(1)*radius(1) / two
      mat_taylor_5(2,4) = -radius(1)**3 / six
      mat_taylor_5(2,5) =  radius(1)**4 / (six*four)
!
      mat_taylor_5(3,1) =  one
      mat_taylor_5(3,2) =  dr_p1
      mat_taylor_5(3,3) =  dr_p1*dr_p1 / two
      mat_taylor_5(3,4) =  dr_p1**3 / six
      mat_taylor_5(3,5) =  dr_p1**4 / (six*four)
!
      mat_taylor_5(4,1) =  zero
      mat_taylor_5(4,2) =  one
      mat_taylor_5(4,3) = -radius(1)
      mat_taylor_5(4,4) =  radius(1)*radius(1) / two
      mat_taylor_5(4,5) = -radius(1)**3 / six
!
      mat_taylor_5(5,1) =  one
      mat_taylor_5(5,2) =  dr_p2
      mat_taylor_5(5,3) =  dr_p2*dr_p2 / two
      mat_taylor_5(5,4) =  dr_p2**3 / six
      mat_taylor_5(5,5) =  dr_p2**4 / (six*four)
!
      call cal_inverse_nn_matrix(ifive, mat_taylor_5,                   &
     &                           mat_fdm_ctr1_fix_5, ierr)
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix cal_4th_to_center_fixed_fdm ',      &
     &            radius(1:2)
      end if
!
      fdm4_vpol_CTR%dmat_vp1(-2,1:5) = zero
      fdm4_vpol_CTR%dmat_vp1(-1,1:5) = zero
      fdm4_vpol_CTR%dmat_vp1( 0,1:5) = mat_fdm_ctr1_fix_5(1:5,1)
      fdm4_vpol_CTR%dmat_vp1( 1,1:5) = mat_fdm_ctr1_fix_5(1:5,3)
      fdm4_vpol_CTR%dmat_vp1( 2,1:5) = mat_fdm_ctr1_fix_5(1:5,5)
!
      end subroutine cal_fdm4_4th_vpol_center1
!
! -----------------------------------------------------------------------
!
      subroutine cal_fdm4_4th_vpol_center2(radius, fdm4_vpol_CTR)
!
      use cal_inverse_small_matrix
!
      real(kind = kreal), intent(in) :: radius(4)
      type(fdm4_center_vpol), intent(inout) :: fdm4_vpol_CTR
!
!>      Work matrix to evaluate fdm4_vpol_CTR%dmat_fix_fld(-1:1,3)
!!@verbatim
!!      dfdr =      mat_fdm_ctr2_fix_5(2,1) * d_rj(2)
!!                + mat_fdm_ctr2_fix_5(2,2) * d_rj(1)
!!                + mat_fdm_ctr2_fix_5(2,3) * d_rj(3)
!!                + mat_fdm_ctr2_fix_5(2,4) * d_center(0)
!!                + mat_fdm_ctr2_fix_5(2,5) * d_rj(4)
!!      d2fdr2 =    mat_fdm_ctr2_fix_5(3,1) * d_rj(2)
!!                + mat_fdm_ctr2_fix_5(3,2) * d_rj(1)
!!                + mat_fdm_ctr2_fix_5(3,3) * d_rj(3)
!!                + mat_fdm_ctr2_fix_5(3,4) * d_center(0)
!!                + mat_fdm_ctr2_fix_5(3,5) * d_rj(4)
!!      d3fdr3 =    mat_fdm_ctr2_fix_5(4,1) * d_rj(2)
!!                + mat_fdm_ctr2_fix_5(4,2) * d_rj(1)
!!                + mat_fdm_ctr2_fix_5(4,3) * d_rj(3)
!!                + mat_fdm_ctr2_fix_5(4,4) * d_center(0)
!!                + mat_fdm_ctr2_fix_5(4,5) * d_rj(4)
!!      d4fdr4 =    mat_fdm_ctr2_fix_5(5,1) * d_rj(2)
!!                + mat_fdm_ctr2_fix_5(5,2) * d_rj(1)
!!                + mat_fdm_ctr2_fix_5(5,3) * d_rj(3)
!!                + mat_fdm_ctr2_fix_5(5,4) * d_center(0)
!!                + mat_fdm_ctr2_fix_5(5,5) * d_rj(4)
!!@endverbatim
      real(kind = kreal) :: mat_fdm_ctr2_fix_5(5,5)
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: mat_taylor_5(5,5)
      real(kind = kreal) :: dr_p1, dr_n1, dr_p2
!
!
      dr_n1 = radius(2) - radius(1)
      dr_p1 = radius(3) - radius(2)
      dr_p2 = radius(4) - radius(2)
!
      mat_taylor_5(1,1) =  one
      mat_taylor_5(1,2) =  zero
      mat_taylor_5(1,3) =  zero
      mat_taylor_5(1,4) =  zero
      mat_taylor_5(1,5) =  zero
!
      mat_taylor_5(2,1) =  one
      mat_taylor_5(2,2) = -dr_n1
      mat_taylor_5(2,3) =  dr_n1*dr_n1 / two
      mat_taylor_5(2,4) = -dr_n1**3 / six
      mat_taylor_5(2,5) =  dr_n1**4 / (six*four)
!
      mat_taylor_5(3,1) =  one
      mat_taylor_5(3,2) =  dr_p1
      mat_taylor_5(3,3) =  dr_p1*dr_p1 / two
      mat_taylor_5(3,4) =  dr_p1**3 / six
      mat_taylor_5(3,5) =  dr_p1**4 / (six*four)
!
      mat_taylor_5(4,1) =  one
      mat_taylor_5(4,2) = -radius(2)
      mat_taylor_5(4,3) =  radius(2)*radius(2) / two
      mat_taylor_5(4,4) = -radius(2)**3 / six
      mat_taylor_5(4,5) =  radius(2)**4 / (six*four)
!
      mat_taylor_5(5,1) =  one
      mat_taylor_5(5,2) =  dr_p2
      mat_taylor_5(5,3) =  dr_p2*dr_p2 / two
      mat_taylor_5(5,4) =  dr_p2**3 / six
      mat_taylor_5(5,5) =  dr_p2**4 / (six*four)
!
      call cal_inverse_nn_matrix(ifive, mat_taylor_5,                   &
     &                           mat_fdm_ctr2_fix_5, ierr)
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix cal_4th_to_center_fixed_fdm ',      &
     &            radius(1:2)
      end if
!
      fdm4_vpol_CTR%dmat_vp1(-2,1:5) = zero
      fdm4_vpol_CTR%dmat_vp1(-1,1:5) = zero
      fdm4_vpol_CTR%dmat_vp2(-1,1:5) = mat_fdm_ctr2_fix_5(1:5,2)
      fdm4_vpol_CTR%dmat_vp2( 0,1:5) = mat_fdm_ctr2_fix_5(1:5,1)
      fdm4_vpol_CTR%dmat_vp2( 1,1:5) = mat_fdm_ctr2_fix_5(1:5,3)
      fdm4_vpol_CTR%dmat_vp2( 2,1:5) = mat_fdm_ctr2_fix_5(1:5,5)
!
      end subroutine cal_fdm4_4th_vpol_center2
!
! -----------------------------------------------------------------------
!
      end module t_coef_fdm4_centre

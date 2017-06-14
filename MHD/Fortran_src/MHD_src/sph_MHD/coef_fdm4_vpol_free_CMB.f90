!>@file   coef_fdm4_vpol_free_CMB.f90
!!@brief  module coef_fdm4_vpol_free_CMB
!!
!!@author H. Matsui
!!@date Programmed in May., 2013
!
!>@brief Matrix to evaluate radial derivative for free-slip at CMB
!!
!!@verbatim
!!      subroutine cal_fdm4_CMB0_free_vp(r_from_CMB)
!!      subroutine cal_fdm4_CMB1_free_vp(r_from_CMB)
!!        type(fdm4_CMB_vpol), intent(inout) :: fdm4_free_CMB
!!
!!      subroutine check_4th_CMB_free_vp_fdm
!!        type(fdm4_CMB_vpol), intent(in) :: fdm4_free_CMB
!!
!!   Matrix for poloidal velocity with free-slip boundary at CMB
!!      dfdr =      fdm4_free_CMB%dmat_vp0(-2,2) * d_rj(CMB-2)
!!                + fdm4_free_CMB%dmat_vp0(-1,2) * d_rj(CMB-1)
!!                + fdm4_free_CMB%dmat_vp0( 0,2) * d_rj(CMB  )
!!      d2fdr2 =    fdm4_free_CMB%dmat_vp0(-2,3) * d_rj(CMB-2)
!!                + fdm4_free_CMB%dmat_vp0(-1,3) * d_rj(CMB-1)
!!                + fdm4_free_CMB%dmat_vp0( 0,3) * d_rj(CMB  )
!!      d3fdr3 =    fdm4_free_CMB%dmat_vp0(-2,4) * d_rj(CMB-2)
!!                + fdm4_free_CMB%dmat_vp0(-1,4) * d_rj(CMB-1)
!!                + fdm4_free_CMB%dmat_vp0( 0,4) * d_rj(CMB  )
!!
!!   Matrix for poloidal velocity with free-slip boundary at next of CMB
!!      dfdr =      fdm4_free_CMB%dmat_vp1(-2,2) * d_rj(CMB-3)
!!                + fdm4_free_CMB%dmat_vp1(-1,2) * d_rj(CMB-2)
!!                + fdm4_free_CMB%dmat_vp1( 0,2) * d_rj(CMB-1)
!!                + fdm4_free_CMB%dmat_vp1( 1,2) * d_rj(CMB  )
!!      d2fdr2 =    fdm4_free_CMB%dmat_vp1(-2,3) * d_rj(CMB-3)
!!                + fdm4_free_CMB%dmat_vp1(-1,3) * d_rj(CMB-2)
!!                + fdm4_free_CMB%dmat_vp1( 0,3) * d_rj(CMB-1)
!!                + fdm4_free_CMB%dmat_vp1( 1,3) * d_rj(CMB  )
!!      d3fdr3 =    fdm4_free_CMB%dmat_vp1(-2,4) * d_rj(CMB-3)
!!                + fdm4_free_CMB%dmat_vp1(-1,4) * d_rj(CMB-2)
!!                + fdm4_free_CMB%dmat_vp1( 0,4) * d_rj(CMB-1)
!!                + fdm4_free_CMB%dmat_vp1( 1,4) * d_rj(CMB  )
!!      d4fdr4 =    fdm4_free_CMB%dmat_vp1(-2,5) * d_rj(CMB-3)
!!                + fdm4_free_CMB%dmat_vp1(-1,5) * d_rj(CMB-2)
!!                + fdm4_free_CMB%dmat_vp1( 0,5) * d_rj(CMB-1)
!!                + fdm4_free_CMB%dmat_vp1( 1,5) * d_rj(CMB  )
!!@endverbatim
!!
!!@n @param r_from_CMB(-3:0) radius from three next points of CMB
!!
      module coef_fdm4_vpol_free_CMB
!
      use m_precision
      use m_constants
!
      use t_coef_fdm4_MHD_boundaries
      use cal_inverse_small_matrix
!
      implicit none
!
!
!>      Work matrix to evaluate fdm4_free_CMB%dmat_vp0
!!@verbatim
!!      dfdr =      mat_fdm4_CMB_free_vp(2,4) * d_rj(CMB-2)
!!                + mat_fdm4_CMB_free_vp(2,3) * d_rj(CMB-1)
!!                + mat_fdm4_CMB_free_vp(2,1) * d_rj(CMB  )
!!                + mat_fdm4_CMB_free_vp(2,2) * B.C. (=0)
!!      d2fdr2 =    mat_fdm4_CMB_free_vp(2,4) * d_rj(CMB-2)
!!                + mat_fdm4_CMB_free_vp(2,3) * d_rj(CMB-1)
!!                + mat_fdm4_CMB_free_vp(2,1) * d_rj(CMB  )
!!                + mat_fdm4_CMB_free_vp(2,2) * B.C. (=0)
!!      d3fdr3 =    mat_fdm4_CMB_free_vp(4,4) * d_rj(CMB-2)
!!                + mat_fdm4_CMB_free_vp(4,3) * d_rj(CMB-1)
!!                + mat_fdm4_CMB_free_vp(4,1) * d_rj(CMB  )
!!                + mat_fdm4_CMB_free_vp(4,2) * B.C. (=0)
!!@endverbatim
      real(kind = kreal) :: mat_fdm4_CMB_free_vp(4,4)
!
!>      Work matrix to evaluate fdm4_free_CMB%dmat_vp1(-1:1,3)
!!@verbatim
!!      dfdr =      mat_fdm4_CMB1_free_vp(2,5) * d_rj(CMB-3)
!!                + mat_fdm4_CMB1_free_vp(2,4) * d_rj(CMB-2)
!!                + mat_fdm4_CMB1_free_vp(2,1) * d_rj(CMB-1)
!!                + mat_fdm4_CMB1_free_vp(2,3) * d_rj(CMB  )
!!                + mat_fdm4_CMB1_free_vp(2,2) * B.C. (=0)
!!      d2fdr2 =    mat_fdm4_CMB1_free_vp(3,5) * d_rj(CMB-3)
!!                + mat_fdm4_CMB1_free_vp(3,4) * d_rj(CMB-2)
!!                + mat_fdm4_CMB1_free_vp(3,1) * d_rj(CMB-1)
!!                + mat_fdm4_CMB1_free_vp(3,3) * d_rj(CMB  )
!!                + mat_fdm4_CMB1_free_vp(3,2) * B.C. (=0)
!!      d3fdr3 =    mat_fdm4_CMB1_free_vp(4,5) * d_rj(CMB-3)
!!                + mat_fdm4_CMB1_free_vp(4,4) * d_rj(CMB-2)
!!                + mat_fdm4_CMB1_free_vp(4,1) * d_rj(CMB-1)
!!                + mat_fdm4_CMB1_free_vp(4,3) * d_rj(CMB  )
!!                + mat_fdm4_CMB1_free_vp(4,2) * B.C. (=0)
!!      d4fdr4 =    mat_fdm4_CMB1_free_vp(5,5) * d_rj(CMB-3)
!!                + mat_fdm4_CMB1_free_vp(5,4) * d_rj(CMB-2)
!!                + mat_fdm4_CMB1_free_vp(5,1) * d_rj(CMB-1)
!!                + mat_fdm4_CMB1_free_vp(5,3) * d_rj(CMB  )
!!                + mat_fdm4_CMB1_free_vp(5,2) * B.C. (=0)
!!@endverbatim
      real(kind = kreal) :: mat_fdm4_CMB1_free_vp(5,5)
!
      private :: mat_fdm4_CMB_free_vp, mat_fdm4_CMB1_free_vp
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_fdm4_CMB0_free_vp(r_from_CMB, fdm4_free_CMB)
!
      real(kind = kreal), intent(in) :: r_from_CMB(-3:0)
      type(fdm4_CMB_vpol), intent(inout) :: fdm4_free_CMB
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: mat_taylor_4(4,4)
      real(kind = kreal) :: dr_n1, dr_n2, r0
!
!
      r0 = r_from_CMB(0)
      dr_n1 = r_from_CMB(0) - r_from_CMB(-1)
      dr_n2 = r_from_CMB(0) - r_from_CMB(-2)
!
      mat_taylor_4(1,1) = one
      mat_taylor_4(1,2) = zero
      mat_taylor_4(1,3) = zero
      mat_taylor_4(1,4) = zero
!
      mat_taylor_4(2,1) =  one
      mat_taylor_4(2,2) =  -r0
      mat_taylor_4(2,3) =  half * r0*r0
      mat_taylor_4(2,4) =  zero
!
      mat_taylor_4(3,1) =  one
      mat_taylor_4(3,2) = -dr_n1
      mat_taylor_4(3,3) =  dr_n1*dr_n1 / two
      mat_taylor_4(3,4) = -dr_n1**3 / six
!
      mat_taylor_4(4,1) =  one
      mat_taylor_4(4,2) = -dr_n2
      mat_taylor_4(4,3) =  dr_n2*dr_n2 / two
      mat_taylor_4(4,4) = -dr_n2**3 / six
!
      call cal_inverse_44_matrix(mat_taylor_4,                          &
     &    mat_fdm4_CMB_free_vp, ierr)
!
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix cal_fdm4_CMB0_free_vp ',            &
     &            r_from_CMB(0)
      end if
!
      fdm4_free_CMB%dmat_vp0(-2,2:4) = mat_fdm4_CMB_free_vp(2:4,4)
      fdm4_free_CMB%dmat_vp0(-1,2:4) = mat_fdm4_CMB_free_vp(2:4,3)
      fdm4_free_CMB%dmat_vp0( 0,2:4) = mat_fdm4_CMB_free_vp(2:4,1)
!
      end subroutine cal_fdm4_CMB0_free_vp
!
! -----------------------------------------------------------------------
!
      subroutine cal_fdm4_CMB1_free_vp(r_from_CMB, fdm4_free_CMB)
!
      real(kind = kreal), intent(in) :: r_from_CMB(-3:0)
      type(fdm4_CMB_vpol), intent(inout) :: fdm4_free_CMB
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: mat_taylor_5(5,5)
      real(kind = kreal) :: dr_p1, dr_n1, dr_n2, r0
!
!
      r0 = r_from_CMB(0)
      dr_p1 = r_from_CMB( 0) - r_from_CMB(-1)
      dr_n1 = r_from_CMB(-1) - r_from_CMB(-2)
      dr_n2 = r_from_CMB(-1) - r_from_CMB(-3)
!
      mat_taylor_5(1,1) = one
      mat_taylor_5(1,2) = zero
      mat_taylor_5(1,3) = zero
      mat_taylor_5(1,4) = zero
      mat_taylor_5(1,5) = zero
!
      mat_taylor_5(3,1) = one
      mat_taylor_5(3,2) = dr_p1                 - r0
      mat_taylor_5(3,3) = dr_p1*dr_p1 / two     - r0*dr_p1              &
     &                   + half*r0*r0
      mat_taylor_5(3,4) = dr_p1**3 / six        - r0*dr_p1*dr_p1 / two  &
     &                   + half*r0*r0 * dr_p1
      mat_taylor_5(3,5) = dr_p1**4 / (six*four) - r0*dr_p1**3 / six     &
     &                   + half*r0*r0 * dr_p1*dr_p1 / two
!
      mat_taylor_5(3,1) = one
      mat_taylor_5(3,2) = dr_p1
      mat_taylor_5(3,3) = dr_p1*dr_p1 / two
      mat_taylor_5(3,4) = dr_p1**3 / six
      mat_taylor_5(3,5) = dr_p1**4 / (six*four)
!
      mat_taylor_5(4,1) = one
      mat_taylor_5(4,2) =-dr_n1
      mat_taylor_5(4,3) = dr_n1*dr_n1 / two
      mat_taylor_5(4,4) =-dr_n1**3 / six
      mat_taylor_5(4,5) = dr_n1**4 / (six*four)
!
      mat_taylor_5(5,1) = one
      mat_taylor_5(5,2) =-dr_n2
      mat_taylor_5(5,3) = dr_n2*dr_n2 / two
      mat_taylor_5(5,4) =-dr_n2**3 / six
      mat_taylor_5(5,5) = dr_n2**4 / (six*four)
!
      call cal_inverse_nn_matrix(ifive, mat_taylor_5,                   &
     &    mat_fdm4_CMB1_free_vp, ierr)
!
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix mat_fdm4_CMB1_free_vp ',            &
     &            r_from_CMB(0)
      end if
!
      fdm4_free_CMB%dmat_vp1(-2,2:5) = mat_fdm4_CMB1_free_vp(2:5,5)
      fdm4_free_CMB%dmat_vp1(-1,2:5) = mat_fdm4_CMB1_free_vp(2:5,4)
      fdm4_free_CMB%dmat_vp1( 0,2:5) = mat_fdm4_CMB1_free_vp(2:5,1)
      fdm4_free_CMB%dmat_vp1( 1,2:5) = mat_fdm4_CMB1_free_vp(2:5,3)
!
      end subroutine cal_fdm4_CMB1_free_vp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_4th_CMB_free_vp_fdm(fdm4_free_CMB)
!
      type(fdm4_CMB_vpol), intent(in) :: fdm4_free_CMB
!
!
      write(50,*) ' free slip boundary'
      call check_4th_CMB_vpol_fdm(fdm4_free_CMB)
!
      end subroutine check_4th_CMB_free_vp_fdm
!
! -----------------------------------------------------------------------
!
      end module coef_fdm4_vpol_free_CMB

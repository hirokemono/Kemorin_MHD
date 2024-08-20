!>@file   coef_fdm3e_hdiv_ICB.f90
!!@brief  module coef_fdm3e_hdiv_ICB
!!
!!@author H. Matsui
!!@date Programmed in May., 2013
!
!>@brief Matrix to evaluate radial derivative for free-slip at ICB
!!
!!@verbatim
!!      subroutine cal_fdm3e_ICB_hdiv_vp(r_from_ICB)
!!        type(fdm3e_BC_hdiv), intent(inout) :: fdm3e_ICB
!!
!!      subroutine check_3rd_ICB_hdiv_vp_fdm
!!        type(fdm3e_BC_hdiv), intent(in) :: fdm3e_ICB
!!
!!   Matrix for poloidal velocity with free-slip boundary at ICB
!!      d_rj(ICB  ) =  mat_taylor_3e(1,1) * d_ele
!!                   + mat_taylor_3e(1,2) * dfdr
!!                   + mat_taylor_3e(1,3) * d2fdr2
!!                   + mat_taylor_3e(1,4) * d3fdr3
!!      d_rj(ICB-1) =  mat_taylor_3e(2,1) * d_ele
!!                   + mat_taylor_3e(2,2) * dfdr
!!                   + mat_taylor_3e(2,3) * d2fdr2
!!                   + mat_taylor_3e(2,4) * d3fdr3
!!      d_rj(ICB-2) =  mat_taylor_3e(3,1) * d_ele
!!                   + mat_taylor_3e(3,2) * dfdr
!!                   + mat_taylor_3e(3,3) * d2fdr2
!!                   + mat_taylor_3e(3,4) * d3fdr3
!!      dfdr(ICB  ) =  mat_taylor_3e(4,1) * d_ele
!!                   + mat_taylor_3e(4,2) * dfdr
!!                   + mat_taylor_3e(4,3) * d2fdr2
!!                   + mat_taylor_3e(4,4) * d3fdr3
!!     mat_fdm4_CMB1_free_vp = (mat_taylor_3e)^-1
!!@endverbatim
!!
      module coef_fdm3e_hdiv_ICB
!
      use m_precision
      use m_constants
!
      use t_coef_fdm3e_MHD_boundaries
      use cal_inverse_small_matrix
!
      implicit none
!
!
!>      Work matrix to evaluate fdm3e_ICB%dmat_vp0
!!@verbatim
!!      d_rj(ICB  ) =  mat_taylor_3e(1,1) * d_ele
!!                   + mat_taylor_3e(1,2) * dfdr
!!                   + mat_taylor_3e(1,3) * d2fdr2
!!                   + mat_taylor_3e(1,4) * d3fdr3
!!      d_rj(ICB+1) =  mat_taylor_3e(2,1) * d_ele
!!                   + mat_taylor_3e(2,2) * dfdr
!!                   + mat_taylor_3e(2,3) * d2fdr2
!!                   + mat_taylor_3e(2,4) * d3fdr3
!!      d_rj(ICB+2) =  mat_taylor_3e(3,1) * d_ele
!!                   + mat_taylor_3e(3,2) * dfdr
!!                   + mat_taylor_3e(3,3) * d2fdr2
!!                   + mat_taylor_3e(3,4) * d3fdr3
!!      dfdr(ICB  ) =  mat_taylor_3e(4,1) * d_ele
!!                   + mat_taylor_3e(4,2) * dfdr
!!                   + mat_taylor_3e(4,3) * d2fdr2
!!                   + mat_taylor_3e(4,4) * d3fdr3
!!     mat_fdm4_CMB1_free_vp = (mat_taylor_3e)^-1
!!@endverbatim
      real(kind = kreal) :: mat_fdm3e_ICB_hdiv_vp(4,4)
!!@endverbatim
!
      private :: mat_fdm3e_ICB_hdiv_vp
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_fdm3e_ICB_hdiv_vp(r_from_ICB, fdm3e_ICB)
!
      real(kind = kreal), intent(in) :: r_from_ICB(0:2)
      type(fdm3e_BC_hdiv), intent(inout) :: fdm3e_ICB
!
      integer(kind = kint) :: ierr
      real(kind = kreal) :: mat_taylor_3e(4,4)
      real(kind = kreal) :: dr_p1, dr_n1, dr_p2
!
!
      dr_n1 = half * (r_from_ICB(1) - r_from_ICB(0))
      dr_p1 = half * (r_from_ICB(1) - r_from_ICB(0))
      dr_p2 = half * (r_from_ICB(2) + r_from_ICB(1)) - r_from_ICB(0)
!
      mat_taylor_3e(1,1) =  one
      mat_taylor_3e(1,2) = -dr_n1
      mat_taylor_3e(1,3) =  half * dr_n1*dr_n1
      mat_taylor_3e(1,4) = =dr_n1*dr_n1*dr_n1 / six
!
      mat_taylor_3e(2,1) =   one
      mat_taylor_3e(2,2) =   dr_p1
      mat_taylor_3e(2,3) =   half * dr_p1*dr_p1
      mat_taylor_3e(2,4) =   dr_p1*dr_p1*dr_p1 / six
!
      mat_taylor_3e(3,1) =   one
      mat_taylor_3e(3,2) =   dr_p2
      mat_taylor_3e(3,3) =   half * dr_p2*dr_p2
      mat_taylor_3e(3,4) =   dr_p2*dr_p2*dr_p2 / six
!
      mat_taylor_3e(4,1) =  zero
      mat_taylor_3e(4,2) =  one
      mat_taylor_3e(4,3) = -dr_n1
      mat_taylor_3e(4,4) =  half * dr_n1*dr_n1
!
      call cal_inverse_44_matrix(mat_taylor_3e,                         &
     &                           mat_fdm3e_ICB_hdiv_vp, ierr)
!
      if(ierr .eq. 1) then
        write(*,*) 'singular matrix cal_fdm3e_ICB_hdiv_vp ',            &
     &            r_from_ICB(0)
      end if
!
      fdm3e_ICB%dmat_vp0(-1,0:3) = mat_fdm3e_ICB_hdiv_vp(1:4,1)
      fdm3e_ICB%dmat_vp0( 0,0:3) = mat_fdm3e_ICB_hdiv_vp(1:4,2)
      fdm3e_ICB%dmat_vp0( 1,0:3) = mat_fdm3e_ICB_hdiv_vp(1:4,3)
      fdm3e_ICB%dmat_vp0( 2,0:3) = mat_fdm3e_ICB_hdiv_vp(1:4,4)
!
      end subroutine cal_fdm3e_ICB_hdiv_vp
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_3rd_ICB_hdiv_vp_fdm(fdm3e_ICB)
!
      type(fdm3e_BC_hdiv), intent(in) :: fdm3e_ICB
!
!
      write(50,*) ' Horizontal divergence on ICB'
      call check_3rd_ele_ICB_vpol_fdm(fdm3e_ICB)
!
      end subroutine check_3rd_ICB_hdiv_vp_fdm
!
! -----------------------------------------------------------------------
!
      end module coef_fdm3e_hdiv_ICB

!>@file   set_sph_horizontal_div_CTR.f90
!!@brief  module set_sph_horizontal_div_CTR
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Set horizontal diffusivity at next of center 
!!      to FDM matrix and explicit term
!!
!!@verbatim
!!      subroutine add_exp7_sph_viscous_CTR1(nnod_rj, jmax,             &
!!     &          d_vpol, press_e, mat_grad_p_CTR1, mat_viscous_CTR1,   &
!!     &          d_viscous_p)
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: mat_grad_p_CTR1(0:1)
!!        real(kind = kreal), intent(in) :: mat_viscous_CTR1(0:1,jmax)
!!        real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!        real(kind = kreal), intent(in) :: press_e(nnod_rj)
!!        real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!!      subroutine add_exp9_sph_viscous_CTR1(nnod_rj, jmax,             &
!!     &          d_vpol, press_e, mat_grad_p_CTR1, mat_viscous_CTR1,   &
!!     &          d_viscous_p)
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: mat_grad_p_CTR1(0:2)
!!        real(kind = kreal), intent(in) :: mat_viscous_CTR1(0:2,jmax)
!!        real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!        real(kind = kreal), intent(in) :: press_e(nnod_rj)
!!        real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!!      subroutine sub_sph_pol_viscous_mat7_CTR1(nri, jmax,             &
!!     &          mat_grad_p_CTR1, mat_viscous_CTR1, mat7)
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: mat_grad_p_CTR1(0:1)
!!        real(kind = kreal), intent(in) :: mat_viscous_CTR1(-1:1,jmax)
!!        real(kind = kreal), intent(inout) :: mat7(7,nri,jmax)
!!      subroutine sub_sph_pol_viscous_mat9_CTR1(nri, jmax,             &
!!     &          mat_grad_p_CTR1, mat_viscous_CTR1, mat9)
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: mat_grad_p_CTR1(-1:2)
!!        real(kind = kreal), intent(in) :: mat_viscous_CTR1(-2:2,jmax)
!!        real(kind = kreal), intent(inout) :: mat9(9,nri,jmax)
!!
!!      subroutine add_exp_sph_hdiv_viscous_CTR(nnod_rj, jmax,          &
!!     &          coef_p, d_vpol, mat_hdiv_vcs_CTR, e_hdiv_viscous)
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind = kreal), intent(in) :: mat_hdiv_vcs_CTR(0:1,jmax)
!!        real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!        real(kind = kreal), intent(inout) :: e_hdiv_viscous(nnod_rj)
!!      subroutine sub_sph_hdiv_viscous_mat7_CTR(nri, jmax, coef_p,     &
!!     &                                         mat_hdiv_vcs_CTR, mat7)
!!      subroutine sub_sph_hdiv_viscous_mat9_CTR(nri, jmax, coef_p,     &
!!     &                                         mat_hdiv_vcs_CTR, mat9)
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind = kreal), intent(in) :: mat_hdiv_vcs_CTR(0:1,jmax)
!!        real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!!        real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!!@endverbatim
!
      module set_sph_horizontal_div_CTR
!
      use m_precision
      use m_constants
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine add_exp7_sph_viscous_CTR1(nnod_rj, jmax,               &
     &          d_vpol, press_e, mat_grad_p_CTR1, mat_viscous_CTR1,     &
     &          d_viscous_p)
!
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: mat_grad_p_CTR1(0:1)
      real(kind = kreal), intent(in) :: mat_viscous_CTR1(0:1,jmax)
      real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
      real(kind = kreal), intent(in) :: press_e(nnod_rj)
!
      real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!
      integer(kind = kint) :: j, i_p1, inod
!
!
!$omp parallel do private(j,i_p1,inod)
      do j = 1, jmax
        inod = j
!        i_n1 = inod - jmax
        i_p1 = inod + jmax
!
        d_viscous_p(inod) = d_viscous_p(inod)                           &
!     &                        + mat_viscous_CTR1(-1,j) * d_vpol(i_n1)  &
     &                        - mat_grad_p_CTR1( 0) *  press_e(inod)    &
     &                        + mat_viscous_CTR1( 0,j) * d_vpol(inod)   &
     &                        - mat_grad_p_CTR1( 1) *  press_e(i_p1)    &
     &                        + mat_viscous_CTR1( 1,j) * d_vpol(i_p1)
      end do
!$omp end parallel do
!
      end subroutine add_exp7_sph_viscous_CTR1
!
! -----------------------------------------------------------------------
!
      subroutine add_exp9_sph_viscous_CTR1(nnod_rj, jmax,               &
     &          d_vpol, press_e, mat_grad_p_CTR1, mat_viscous_CTR1,     &
     &          d_viscous_p)
!
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: mat_grad_p_CTR1(0:2)
      real(kind = kreal), intent(in) :: mat_viscous_CTR1(0:2,jmax)
      real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
      real(kind = kreal), intent(in) :: press_e(nnod_rj)
!
      real(kind = kreal), intent(inout) :: d_viscous_p(nnod_rj)
!
      integer(kind = kint) :: j, i_p1, i_p2, inod
!
!
!$omp parallel do private(j,i_p1,i_p2,inod)
      do j = 1, jmax
        inod = j
!        i_n2 = inod - jmax
!        i_n1 = inod - jmax
        i_p1 = inod + jmax
        i_p2 = i_p1 + jmax
!
        d_viscous_p(inod) = d_viscous_p(inod)                           &
!     &                        + mat_viscous_CTR1(-2,j) * d_vpol(i_n2)  &
!     &                        - mat_grad_p_CTR1(-1) *  press_e(i_n1)   &
!     &                        + mat_viscous_CTR1(-1,j) * d_vpol(i_n1)  &
     &                        - mat_grad_p_CTR1( 0) *  press_e(inod)    &
     &                        + mat_viscous_CTR1( 0,j) * d_vpol(inod)   &
     &                        - mat_grad_p_CTR1( 1) *  press_e(i_p1)    &
     &                        + mat_viscous_CTR1( 1,j) * d_vpol(i_p1)   &
     &                        - mat_grad_p_CTR1( 2) *  press_e(i_p2)    &
     &                        + mat_viscous_CTR1( 2,j) * d_vpol(i_p2)
      end do
!$omp end parallel do
!
      end subroutine add_exp9_sph_viscous_CTR1
!
! -----------------------------------------------------------------------
!
      subroutine sub_sph_pol_viscous_mat7_CTR1(nri, jmax,               &
     &          mat_grad_p_CTR1, mat_viscous_CTR1, mat7)
!
      integer(kind = kint), intent(in) :: nri, jmax
!
      real(kind = kreal), intent(in) :: mat_grad_p_CTR1(0:1)
      real(kind = kreal), intent(in) :: mat_viscous_CTR1(-1:1,jmax)
!
      real(kind = kreal), intent(inout) :: mat7(7,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
!        mat7(7,-1,j) = mat7(7,-1,j)
!        mat7(6, 0,j) = mat7(6, 0,j) - mat_viscous_CTR1(-1,j)
        mat7(5, 1,j) = mat7(5, 1,j) + mat_grad_p_CTR1(0)
!
        mat7(4, 2,j) = mat7(4, 2,j) - mat_viscous_CTR1( 0,j)
!
        mat7(3, 3,j) = mat7(3, 3,j) + mat_grad_p_CTR1(1)
        mat7(2, 4,j) = mat7(2, 4,j) - mat_viscous_CTR1( 1,j)
!        mat7(1, 5,j) = mat7(1, 5,j)
      end do
!
      end subroutine sub_sph_pol_viscous_mat7_CTR1
!
!  -------------------------------------------------------------------
!
      subroutine sub_sph_pol_viscous_mat9_CTR1(nri, jmax,               &
     &          mat_grad_p_CTR1, mat_viscous_CTR1, mat9)
!
      integer(kind = kint), intent(in) :: nri, jmax
!
      real(kind = kreal), intent(in) :: mat_grad_p_CTR1(-1:2)
      real(kind = kreal), intent(in) :: mat_viscous_CTR1(-2:2,jmax)
!
      real(kind = kreal), intent(inout) :: mat9(9,nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
!        mat9(9,-2,j) = mat9(9,-2,j) - mat_viscous_CTR1(-2,j)
!        mat9(8,-1,j) = mat9(8,-1,j) + mat_grad_p_CTR1(-1)
!        mat9(7, 0,j) = mat9(7, 0,j) - mat_viscous_CTR1(-1,j)
        mat9(6, 1,j) = mat9(6, 1,j) + mat_grad_p_CTR1( 0)
!
        mat9(5, 2,j) = mat9(5, 2,j) - mat_viscous_CTR1( 0,j)
!
        mat9(4, 3,j) = mat9(4, 3,j) + mat_grad_p_CTR1( 1)
        mat9(3, 4,j) = mat9(3, 4,j) - mat_viscous_CTR1( 1,j)
        mat9(2, 5,j) = mat9(2, 5,j) + mat_grad_p_CTR1( 2)
        mat9(1, 6,j) = mat9(1, 6,j) - mat_viscous_CTR1( 2,j)
      end do
!
      end subroutine sub_sph_pol_viscous_mat9_CTR1
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine add_exp_sph_hdiv_viscous_CTR(nnod_rj, jmax,            &
     &          coef_p, d_vpol, mat_hdiv_vcs_CTR, e_hdiv_viscous)
!
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: mat_hdiv_vcs_CTR(0:1,jmax)
      real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!
      real(kind = kreal), intent(inout) :: e_hdiv_viscous(nnod_rj)
!
      integer(kind = kint) :: j, iele, i_p1, inod
!
!
!$omp parallel do private(j,iele,i_p1,inod)
      do j = 1, jmax
        iele = j
        i_p1 = iele + jmax
        inod = iele
!
        e_hdiv_viscous(iele) = e_hdiv_viscous(iele) - coef_p            &
     &                        + mat_hdiv_vcs_CTR( 0,j) * d_vpol(inod)   &
     &                        + mat_hdiv_vcs_CTR( 1,j) * d_vpol(i_p1)
      end do
!$omp end parallel do
!
      end subroutine add_exp_sph_hdiv_viscous_CTR
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sub_sph_hdiv_viscous_mat7_CTR(nri, jmax, coef_p,       &
     &                                         mat_hdiv_vcs_CTR, mat7)
!
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: mat_hdiv_vcs_CTR(0:1,jmax)
!
      real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!
!$omp parallel do private(j)
      do j = 1, jmax
!        mat7(7,-2,j) = mat7(7,-2,j) - mat_hdiv_vcs_CTR(-2,j)
!        mat7(6,-1,j) = mat7(6,-1,j)
!        mat7(5, 0,j) = mat7(5, 0,j) - mat_hdiv_vcs_CTR(-1,j)
!
        mat7(4, 1,j) = mat7(4, 1,j) + coef_p
!
!
        mat7(3,2,j) = mat7(3,2,j) - mat_hdiv_vcs_CTR(0,j)
!       mat7(2,3,j) = mat7(2,3,j)
        mat7(1,4,j) = mat7(1,4,j) - mat_hdiv_vcs_CTR(1,j)
      end do
!$omp end parallel do
!
      end subroutine sub_sph_hdiv_viscous_mat7_CTR
!
! -----------------------------------------------------------------------
!
      subroutine sub_sph_hdiv_viscous_mat9_CTR(nri, jmax, coef_p,       &
     &                                         mat_hdiv_vcs_CTR, mat9)
!
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: mat_hdiv_vcs_CTR(0:1,jmax)
!
      real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!
!$omp parallel do private(j)
      do j = 1, jmax
!        mat9(9,-3,j) = mat9(9,-3,j)   zero
!        mat9(8,-2,j) = mat9(8,-2,j) - mat_hdiv_vcs_CTR(-2,j)
!        mat9(7,-1,j) = mat9(7,-1,j)
!        mat9(6, 0,j) = mat9(6, 0,j) - mat_hdiv_vcs_CTR(-1,j)
!
        mat9(5,1,j) = mat9(5,1,j) + coef_p
!
        mat9(4,2,j) = mat9(4,2,j) - mat_hdiv_vcs_CTR(0,j)
!       mat7(3,3,j) = mat7(3,3,j)
        mat9(2,4,j) = mat9(2,4,j) - mat_hdiv_vcs_CTR(1,j)
!       mat7(1,5,j) = mat7(1,5,j)
      end do
!$omp end parallel do
!
      end subroutine sub_sph_hdiv_viscous_mat9_CTR
!
! -----------------------------------------------------------------------
!
      end module set_sph_horizontal_div_CTR


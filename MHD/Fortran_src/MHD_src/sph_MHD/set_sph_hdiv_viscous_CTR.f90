!>@file   set_sph_hdiv_viscous_CTR.f90
!!@brief  module set_sph_hdiv_viscous_CTR
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2020
!
!>@brief Set horizontal diffusivity at Center
!!      to FDM matrix and explicit term
!!
!!@verbatim
!!      subroutine add_exp_sph_hdiv_viscous_CTR1 (nnod_rj, jmax, coef_p,&
!!     &          hdiv_visous_mat_CTR1, d_vpol, press_e, hdiv_viscous_e)
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind=kreal), intent(in) :: hdiv_visous_mat_CTR1(jmax,-1:1)
!!        real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!        real(kind = kreal), intent(in) :: press_e(nnod_rj)
!!        real(kind = kreal), intent(inout) :: hdiv_viscous_e(nnod_rj)
!!      subroutine add_exp_sph_hdiv_viscous_CTR(nnod_rj, jmax, coef_p,  &
!!     &          d_vpol, mat_hdiv_vcs_CTR, e_hdiv_viscous)
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind = kreal), intent(in) :: mat_hdiv_vcs_CTR(jmax,0:1)
!!        real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!        real(kind = kreal), intent(inout) :: e_hdiv_viscous(nnod_rj)
!!
!!      subroutine sub_sph_hdiv_viscous_mat7_CTR(nri, jmax, coef_p,     &
!!     &          hdiv_visous_mat_CTR, mat7)
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind = kreal), intent(in) :: hdiv_visous_mat_CTR(jmax,0:1)
!!        real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!!      subroutine sub_sph_hdiv_viscous_mat7_CTR1(nri, jmax, coef_p,    &
!!     &          hdiv_visous_mat_CTR1, mat7)
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind = kreal), intent(in)                                &
!!     &                   :: hdiv_visous_mat_CTR1(jmax,-1:1)
!!        real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!!      subroutine sub_sph_hdiv_viscous_mat9_CTR1(nri, jmax, coef_p,    &
!!     &           hdiv_visous_mat_CTR1, mat9)
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind=kreal), intent(in) :: hdiv_visous_mat_CTR1(jmax,-1:1)
!!        real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!!      subroutine sub_sph_hdiv_viscous_mat9_CTR(nri, jmax, coef_p,     &
!!     &          hdiv_visous_mat_CTR, mat9)
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind=kreal), intent(in) :: hdiv_visous_mat_CTR(jmax,-1:1)
!!        real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!!@endverbatim
!!
      module set_sph_hdiv_viscous_CTR
!
      use m_precision
      use m_constants
!
      implicit none
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine add_exp_sph_hdiv_viscous_CTR1 (nnod_rj, jmax, coef_p,  &
     &          hdiv_visous_mat_CTR1, d_vpol, press_e, hdiv_viscous_e)
!
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: hdiv_visous_mat_CTR1(jmax,-1:1)
      real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
      real(kind = kreal), intent(in) :: press_e(nnod_rj)
!
      real(kind = kreal), intent(inout) :: hdiv_viscous_e(nnod_rj)
!
      integer(kind = kint) :: j, i_n1, i_p1, inod, iele
!
!
!$omp parallel do private(j,iele,inod,i_n1,i_p1)
      do j = 1, jmax
        iele = j + jmax
        inod = iele
        i_n1 = j
        i_p1 = inod + jmax
!
        hdiv_viscous_e(iele) = hdiv_viscous_e(iele)                     &
     &                     + hdiv_visous_mat_CTR1(j,-1) * d_vpol(i_n1)  &
     &                     - coef_p *                press_e(iele)      &
     &                     + hdiv_visous_mat_CTR1(j, 0) * d_vpol(inod)  &
     &                     + hdiv_visous_mat_CTR1(j, 1) * d_vpol(i_p1)
!
        hdiv_viscous_e(i_n1) = hdiv_viscous_e(iele)
      end do
!$omp end parallel do
!
!
      end subroutine add_exp_sph_hdiv_viscous_CTR1
!
! -----------------------------------------------------------------------
!
      subroutine add_exp_sph_hdiv_viscous_CTR(nnod_rj, jmax, coef_p,    &
     &          d_vpol, mat_hdiv_vcs_CTR, e_hdiv_viscous)
!
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: mat_hdiv_vcs_CTR(jmax,0:1)
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
     &                        + mat_hdiv_vcs_CTR(j, 0) * d_vpol(inod)   &
     &                        + mat_hdiv_vcs_CTR(j, 1) * d_vpol(i_p1)
      end do
!$omp end parallel do
!
      end subroutine add_exp_sph_hdiv_viscous_CTR
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sub_sph_hdiv_viscous_mat7_CTR(nri, jmax, coef_p,       &
     &          hdiv_visous_mat_CTR, mat7)
!
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: hdiv_visous_mat_CTR(jmax,0:1)
!
      real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!$omp parallel do private(j)
      do j = 1, jmax
!       mat7(7,-2,j) = mat7(7,-2,j) - hdiv_visous_mat_CTR(j,-2)
!       mat7(6,-1,j) = mat7(6,-1,j)
!       mat7(5, 0,j) = mat7(5, 0,j) - hdiv_visous_mat_CTR(j,-1)
!
        mat7(4,1,j) = mat7(4,1,j) + coef_p
!
        mat7(3,2,j) = mat7(3,2,j) - hdiv_visous_mat_CTR(j, 0)
!       mat7(2,3,j) = mat7(2,3,j)
        mat7(1,4,j) = mat7(1,4,j) - hdiv_visous_mat_CTR(j, 1)
      end do
!$omp end parallel do
!
      end subroutine sub_sph_hdiv_viscous_mat7_CTR
!
!  -------------------------------------------------------------------
!
      subroutine sub_sph_hdiv_viscous_mat7_CTR1(nri, jmax, coef_p,      &
     &          hdiv_visous_mat_CTR1, mat7)
!
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in)                                    &
     &                   :: hdiv_visous_mat_CTR1(jmax,-1:1)
!
      real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!$omp parallel do private(j)
      do j = 1, jmax
!       mat7(7,0,j) = mat7(7,0,j) - hdiv_visous_mat_CTR1(j,-2)
!       mat7(6,1,j) = mat7(6,1,j)
        mat7(5,2,j) = mat7(5,2,j) - hdiv_visous_mat_CTR1(j,-1)
!
        mat7(4,3,j) = mat7(4,3,j) + coef_p
!
        mat7(3,4,j) = mat7(3,4,j) - hdiv_visous_mat_CTR1(j, 0)
!       mat7(2,5,j) = mat7(2,5,j)
        mat7(1,6,j) = mat7(1,6,j) - hdiv_visous_mat_CTR1(j, 1)
      end do
!$omp end parallel do
!
      end subroutine sub_sph_hdiv_viscous_mat7_CTR1
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine sub_sph_hdiv_viscous_mat9_CTR1(nri, jmax, coef_p,      &
     &           hdiv_visous_mat_CTR1, mat9)
!
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: hdiv_visous_mat_CTR1(jmax,-1:1)
!
      real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!$omp parallel do private(j)
      do j = 1, jmax
!        mat9(9,-1,j) = mat9(9,-1,j)
!        mat9(8, 0,j) = mat9(8, 0,j) - hdiv_visous_mat_CTR1(j,-2)
!        mat9(7, 1,j) = mat9(7, 1,j)
        mat9(6, 2,j) = mat9(6, 2,j) - hdiv_visous_mat_CTR1(j,-1)
!
        mat9(5,3,j) = mat9(5,3,j) + coef_p
!
        mat9(4,4,j) = mat9(4,4,j) - hdiv_visous_mat_CTR1(j, 0)
!        mat9(3,5,j) = mat9(3,5,j)
        mat9(2,6,j) = mat9(2,6,j) - hdiv_visous_mat_CTR1(j, 1)
!        mat9(1,7,j) = mat9(1,7,j)
      end do
!$omp end parallel do
!
      end subroutine sub_sph_hdiv_viscous_mat9_CTR1
!
!  -------------------------------------------------------------------
!
      subroutine sub_sph_hdiv_viscous_mat9_CTR(nri, jmax, coef_p,       &
     &          hdiv_visous_mat_CTR, mat9)
!
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: hdiv_visous_mat_CTR(jmax,0:1)
!
      real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!$omp parallel do private(j)
      do j = 1, jmax
!        mat9(9,-3,j) = mat9(9,-3,j)
!        mat9(8,-2,j) = mat9(8,-2,j) - hdiv_visous_mat_CTR(j,-2)
!        mat9(7,-1,j) = mat9(7,-1,j)
!        mat9(6, 0,j) = mat9(6, 0,j) - hdiv_visous_mat_CTR(j,-1)
!
        mat9(5,1,j) = mat9(5,1,j) + coef_p
!
        mat9(4,2,j) = mat9(4,2,j) - hdiv_visous_mat_CTR(j, 0)
!        mat9(3,3,j) = mat9(3,3,j)
        mat9(2,4,j) = mat9(2,4,j) - hdiv_visous_mat_CTR(j, 1)
!        mat9(1,5,j) = mat9(1,5,j)
      end do
!$omp end parallel do
!
      end subroutine sub_sph_hdiv_viscous_mat9_CTR
!
!  -------------------------------------------------------------------
!
      end module set_sph_hdiv_viscous_CTR

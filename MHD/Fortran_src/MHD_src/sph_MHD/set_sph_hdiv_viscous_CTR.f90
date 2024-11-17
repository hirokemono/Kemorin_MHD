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

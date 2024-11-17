!>@file   set_sph_hdiv_viscous_ICB.f90
!!@brief  module set_sph_hdiv_viscous_ICB
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2020
!
!>@brief Set horizontal diffusivity at ICB
!!      to FDM matrix and explicit term
!!
!!@verbatim
!!      subroutine sub_sph_hdiv_viscous_mat7_ICB1                       &
!!     &         (k_ICB, nri, jmax, coef_p, hdiv_visous_mat_ICB, mat7)
!!        integer(kind = kint), intent(in) :: k_ICB
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind=kreal), intent(in) :: hdiv_visous_mat_ICB(jmax,-1:1)
!!        real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!!      subroutine sub_sph_hdiv_viscous_mat9_ICB1(k_ICB, nri, jmax,     &
!!     &          coef_p, hdiv_visous_mat_CMB1, mat9)
!!      subroutine set_sph_hdiv_viscous_mat9_ICB(k_ICB, nri, jmax, mat9)
!!        integer(kind = kint), intent(in) :: k_ICB
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind=kreal), intent(in) :: hdiv_visous_mat_ICB(jmax,-1:1)
!!        real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!!@endverbatim
!!
      module set_sph_hdiv_viscous_ICB
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
! -----------------------------------------------------------------------

      subroutine sub_sph_hdiv_viscous_mat9_ICB1(k_ICB, nri, jmax,       &
     &          coef_p, hdiv_visous_mat_ICB, mat9)
!
      integer(kind = kint), intent(in) :: k_ICB
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: hdiv_visous_mat_ICB(jmax,-1:1)
!
      real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!$omp parallel do private(j)
      do j = 1, jmax
        if(2*k_ICB-3 .gt. 0) mat9(9,2*k_ICB-3,j) = zero
        if(2*k_ICB-2 .gt. 0) mat9(8,2*k_ICB-2,j) = zero
!        mat9(7,2*k_ICB-1,j) = mat9(7,2*k_ICB-1,j)
        mat9(6,2*k_ICB,  j) = mat9(6,2*k_ICB,  j)                       &
     &                       - hdiv_visous_mat_ICB(j,-1)
!
        mat9(5,2*k_ICB+1,j) = mat9(5,2*k_ICB+1,j) + coef_p
!
        mat9(4,2*k_ICB+2,j) = mat9(4,2*k_ICB+2,j)                       &
     &                       - hdiv_visous_mat_ICB(j, 0)
!        mat9(3,2*k_ICB+3,j) = mat9(3,2*k_ICB+3,j)
        mat9(2,2*k_ICB+4,j) = mat9(2,2*k_ICB+4,j)                       &
     &                       - hdiv_visous_mat_ICB(j, 1)
!        mat9(1,2*kr+5,j) = mat9(1,2*kr+5,j)
      end do
!$omp end parallel do
!
      end subroutine sub_sph_hdiv_viscous_mat9_ICB1
!
!  -------------------------------------------------------------------
!
      subroutine set_sph_hdiv_viscous_mat9_ICB(k_ICB, nri, jmax, mat9)
!
      integer(kind = kint), intent(in) :: k_ICB
      integer(kind = kint), intent(in) :: nri, jmax
!
      real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!$omp parallel do private(j)
      do j = 1, jmax
        if(2*k_ICB-5 .gt. 0) mat9(9,2*k_ICB-5,j) = zero
        if(2*k_ICB-4 .gt. 0) mat9(8,2*k_ICB-4,j) = zero
        if(2*k_ICB-3 .gt. 0) mat9(7,2*k_ICB-3,j) = zero
        if(2*k_ICB-2 .gt. 0) mat9(6,2*k_ICB-2,j) = zero
!
        mat9(5,2*k_ICB-1,j) = zero
!
        mat9(4,2*k_ICB,  j) = one
        mat9(3,2*k_ICB+1,j) = zero
        mat9(2,2*k_ICB+2,j) = zero
        mat9(1,2*k_ICB+3,j) = zero
      end do
!$omp end parallel do
!
      end subroutine set_sph_hdiv_viscous_mat9_ICB
!
!  -------------------------------------------------------------------
!
      end module set_sph_hdiv_viscous_ICB

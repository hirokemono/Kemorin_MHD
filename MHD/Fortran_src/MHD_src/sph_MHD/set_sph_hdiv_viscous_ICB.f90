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
!!      subroutine add_exp_sph_hdiv_viscous_ICB1                        &
!!     &         (k_ICB, nnod_rj, nri, jmax, coef_p,                    &
!!     &          hdiv_visous_mat_ICB1, d_vpol, press_e, hdiv_viscous_e)
!!        integer(kind = kint), intent(in) :: k_ICB
!!        integer(kind = kint), intent(in) :: nnod_rj, nri, jmax
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind=kreal), intent(in) :: hdiv_visous_mat_ICB1(jmax,-1:1)
!!        real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!        real(kind = kreal), intent(in) :: press_e(nnod_rj)
!!        real(kind = kreal), intent(inout) :: hdiv_viscous_e(nnod_rj)
!!
!!      subroutine sub_sph_hdiv_viscous_mat7_ICB                        &
!!     &         (k_ICB, nri, jmax, coef_p, hdiv_visous_mat_ICB1, mat7)
!!        integer(kind = kint), intent(in) :: k_ICB
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind=kreal), intent(in) :: hdiv_visous_mat_ICB1(jmax,-1:1)
!!        real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!!      subroutine sub_sph_hdiv_viscous_mat9_ICB1(k_ICB, nri, jmax,     &
!!     &          coef_p, hdiv_visous_mat_CMB1, mat9)
!!      subroutine set_sph_hdiv_viscous_mat9_ICB(k_ICB, nri, jmax, mat9)
!!        integer(kind = kint), intent(in) :: k_ICB
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind=kreal), intent(in) :: hdiv_visous_mat_ICB1(jmax,-1:1)
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
!
      subroutine add_exp_sph_hdiv_viscous_ICB1                          &
     &         (k_ICB, nnod_rj, nri, jmax, coef_p,                      &
     &          hdiv_visous_mat_ICB1, d_vpol, press_e, hdiv_viscous_e)
!
      integer(kind = kint), intent(in) :: k_ICB
      integer(kind = kint), intent(in) :: nnod_rj, nri, jmax
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: hdiv_visous_mat_ICB1(jmax,-1:1)
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
        iele = j + k_ICB * jmax
        inod = iele
        i_n1 = inod - jmax
        i_p1 = inod + jmax
!
        hdiv_viscous_e(iele) = hdiv_viscous_e(iele)                     &
     &                     + hdiv_visous_mat_ICB1(j,-1) * d_vpol(i_n1)  &
     &                     - coef_p *                press_e(iele)      &
     &                     + hdiv_visous_mat_ICB1(j, 0) * d_vpol(inod)  &
     &                     + hdiv_visous_mat_ICB1(j, 1) * d_vpol(i_p1)
!
        hdiv_viscous_e(i_n1) = hdiv_viscous_e(iele)
      end do
!$omp end parallel do
!
!
      end subroutine add_exp_sph_hdiv_viscous_ICB1
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sub_sph_hdiv_viscous_mat7_ICB                          &
     &         (k_ICB, nri, jmax, coef_p, hdiv_visous_mat_ICB1, mat7)
!
      integer(kind = kint), intent(in) :: k_ICB
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: hdiv_visous_mat_ICB1(jmax,-1:1)
!
      real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!$omp parallel do private(j)
      do j = 1, jmax
        if(2*k_ICB-2 .gt. 0) mat7(7,2*k_ICB-2,j) = zero
!       mat7(6,2*k_ICB-1,j) = mat7(6,2*k_ICB-1,j)
        mat7(5,2*k_ICB,  j) = mat7(5,2*k_ICB,  j)                       &
     &                       - hdiv_visous_mat_ICB1(j,-1)
!
        mat7(4,2*k_ICB+1,j) = mat7(4,2*k_ICB+1,j) + coef_p
!
        mat7(3,2*k_ICB+2,j) = mat7(3,2*k_ICB+2,j)                       &
     &                       - hdiv_visous_mat_ICB1(j, 0)
!        mat7(2,2*k_ICB+3,j) = mat7(2,2*k_ICB+3,j)
        mat7(1,2*k_ICB+4,j) = mat7(1,2*k_ICB+4,j)                       &
     &                       - hdiv_visous_mat_ICB1(j, 1)
      end do
!$omp end parallel do
!
      end subroutine sub_sph_hdiv_viscous_mat7_ICB
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_hdiv_viscous_mat7_ICB(k_ICB, nri, jmax, mat7)
!
      integer(kind = kint), intent(in) :: k_ICB
      integer(kind = kint), intent(in) :: nri, jmax
!
      real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!$omp parallel do private(j)
      do j = 1, jmax
        if(2*k_ICB-4 .gt. 0) mat7(7,2*k_ICB-4,j) = zero
        if(2*k_ICB-3 .gt. 0) mat7(6,2*k_ICB-3,j) = zero
        if(2*k_ICB-2 .gt. 0) mat7(5,2*k_ICB-2,j) = zero
!
        mat7(4,2*k_ICB-1,j) = zero
!
        mat7(3,2*k_ICB,  j) = one
        mat7(2,2*k_ICB+1,j) = zero
        mat7(1,2*k_ICB+2,j) = zero
      end do
!$omp end parallel do
!
      end subroutine set_sph_hdiv_viscous_mat7_ICB
!
!  -------------------------------------------------------------------
!
      subroutine sub_sph_hdiv_viscous_mat9_ICB1(k_ICB, nri, jmax,       &
     &          coef_p, hdiv_visous_mat_ICB1, mat9)
!
      integer(kind = kint), intent(in) :: k_ICB
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: hdiv_visous_mat_ICB1(jmax,-1:1)
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
     &                       - hdiv_visous_mat_ICB1(j,-1)
!
        mat9(5,2*k_ICB+1,j) = mat9(5,2*k_ICB+1,j) + coef_p
!
        mat9(4,2*k_ICB+2,j) = mat9(4,2*k_ICB+2,j)                       &
     &                       - hdiv_visous_mat_ICB1(j, 0)
!        mat9(3,2*k_ICB+3,j) = mat9(3,2*k_ICB+3,j)
        mat9(2,2*k_ICB+4,j) = mat9(2,2*k_ICB+4,j)                       &
     &                       - hdiv_visous_mat_ICB1(j, 1)
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

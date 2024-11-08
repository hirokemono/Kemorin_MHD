!>@file   set_sph_hdiv_viscous_CMB.f90
!!@brief  module set_sph_hdiv_viscous_CMB
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2020
!
!>@brief  Substitute viscousity matrix in each layer
!!
!!@verbatim
!!      subroutine add_exp_sph_hdiv_viscous_CMB                         &
!!     &         (k_CMB, nnod_rj, nri, jmax, coef_p,                    &
!!     &          hdiv_visous_mat_CMB, d_vpol, press_e, hdiv_viscous_e)
!!        integer(kind = kint), intent(in) :: k_CMB
!!        integer(kind = kint), intent(in) :: nnod_rj, nri, jmax
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind=kreal), intent(in) :: hdiv_visous_mat_CMB(jmax,-2:0)
!!        real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!        real(kind = kreal), intent(in) :: press_e(nnod_rj)
!!        real(kind = kreal), intent(inout) :: hdiv_viscous_e(nnod_rj)
!!
!!      subroutine sub_sph_hdiv_viscous_mat7_CMB                        &
!!     &         (k_CMB, nri, jmax, coef_p, hdiv_visous_mat_CMB, mat7)
!!      subroutine sub_sph_hdiv_viscous_mat7_CMB1                       &
!!     &         (k_CMB, nri, jmax, coef_p, hdiv_visous_mat_CMB1, mat7)
!!        integer(kind = kint), intent(in) :: k_CMB
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind=kreal), intent(in) :: hdiv_visous_mat_CMB1(jmax,-2:1)
!!        real(kind=kreal), intent(in) :: hdiv_visous_mat_CMB(jmax,-2:0)
!!        real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!!      subroutine sub_sph_hdiv_viscous_mat9_CMB                        &
!!     &         (k_CMB, nri, jmax, coef_p,                             &
!!     &          hdiv_visous_mat_CMB1, hdiv_visous_mat_CMB, mat9)
!!        integer(kind = kint), intent(in) :: k_CMB
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind=kreal), intent(in) :: hdiv_visous_mat_CMB1(jmax,-2:1)
!!        real(kind=kreal), intent(in) :: hdiv_visous_mat_CMB(jmax,-2:0)
!!        real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!!@endverbatim
!!
      module set_sph_hdiv_viscous_CMB
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
      subroutine add_exp_sph_hdiv_viscous_CMB                           &
     &         (k_CMB, nnod_rj, nri, jmax, coef_p,                      &
     &          hdiv_visous_mat_CMB, d_vpol, press_e, hdiv_viscous_e)
!
      integer(kind = kint), intent(in) :: k_CMB
      integer(kind = kint), intent(in) :: nnod_rj, nri, jmax
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: hdiv_visous_mat_CMB(jmax,-2:0)
      real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
      real(kind = kreal), intent(in) :: press_e(nnod_rj)
!
      real(kind = kreal), intent(inout) :: hdiv_viscous_e(nnod_rj)
!
      integer(kind = kint) :: j, i_n1, i_p1, i_n2, inod, iele
!
!
!$omp parallel do private(j,iele,inod,i_n2,i_n1)
      do j = 1, jmax
        iele = j + (k_CMB-1) * nri
        inod = iele
        i_n1 = inod - jmax
        i_n2 = i_n1 - jmax
!
        hdiv_viscous_e(iele) = hdiv_viscous_e(iele)                     &
     &                     + hdiv_visous_mat_CMB(j,-2) * d_vpol(i_n2)   &
     &                     + hdiv_visous_mat_CMB(j,-1) * d_vpol(i_n1)   &
     &                     - coef_p *                press_e(iele)      &
     &                     + hdiv_visous_mat_CMB(j, 0) * d_vpol(inod)
      end do
!$omp end parallel do
!
      end subroutine add_exp_sph_hdiv_viscous_CMB
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine sub_sph_hdiv_viscous_mat7_CMB1                         &
     &         (k_CMB, nri, jmax, coef_p, hdiv_visous_mat_CMB1, mat7)
!
      integer(kind = kint), intent(in) :: k_CMB
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: hdiv_visous_mat_CMB1(jmax,-2:1)
!
      real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!$omp parallel do private(j)
      do j = 1, jmax
        mat7(7,2*k_CMB-6,j) = mat7(7,2*k_CMB-6,j)                       &
     &                       - hdiv_visous_mat_CMB1(j,-2)
!       mat7(6,2*k_CMB-5,j) = mat7(6,2*k_CMB-5,j)
        mat7(5,2*k_CMB-4,j) = mat7(5,2*k_CMB-4,j)                       &
     &                       - hdiv_visous_mat_CMB1(j,-1)
!
        mat7(4,2*k_CMB-3,j) = mat7(4,2*k_CMB-3,j) + coef_p
!
        mat7(3,2*k_CMB-2,j) = mat7(3,2*k_CMB-2,j)                       &
     &                       - hdiv_visous_mat_CMB1(j, 0)
        mat7(2,2*k_CMB-1,j) = mat7(2,2*k_CMB-1,j)
        mat7(1,2*k_CMB,  j) = mat7(1,2*k_CMB,  j)                       &
     &                       - hdiv_visous_mat_CMB1(j, 1)
      end do
!$omp end parallel do
!
      end subroutine sub_sph_hdiv_viscous_mat7_CMB1
!
! -----------------------------------------------------------------------
!
      subroutine sub_sph_hdiv_viscous_mat7_CMB                          &
     &         (k_CMB, nri, jmax, coef_p, hdiv_visous_mat_CMB, mat7)
!
      integer(kind = kint), intent(in) :: k_CMB
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: hdiv_visous_mat_CMB(jmax,-2:0)
!
      real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!$omp parallel do private(j)
      do j = 1, jmax
        mat7(7,2*k_CMB-4,j) = mat7(7,2*k_CMB-4,j)                       &
     &                       - hdiv_visous_mat_CMB(j,-2)
!       mat7(6,2*k_CMB-3,j) = mat7(6,2*k_CMB-3,j)
        mat7(5,2*k_CMB-2,j) = mat7(5,2*k_CMB-2,j)                       &
     &                       - hdiv_visous_mat_CMB(j,-1)
!
        mat7(4,2*k_CMB-1,j) = mat7(4,2*k_CMB-1,j) + coef_p
!
        mat7(3,2*k_CMB,  j) = mat7(3,2*k_CMB,  j)                       &
     &                       - hdiv_visous_mat_CMB(j, 0)
!       mat7(2,2*k_CMB+1,j) = mat7(2,2*k_CMB+1,j)
!       mat7(1,2*k_CMB+2,j) = mat7(1,2*k_CMB+2,j)                       &
!     &                       - hdiv_visous_mat_CMB(j, 1)
      end do
!$omp end parallel do
!
      end subroutine sub_sph_hdiv_viscous_mat7_CMB
!
! -----------------------------------------------------------------------
!
      subroutine sub_sph_hdiv_viscous_mat9_CMB                          &
     &         (k_CMB, nri, jmax, coef_p,                               &
     &          hdiv_visous_mat_CMB1, hdiv_visous_mat_CMB, mat9)
!
      integer(kind = kint), intent(in) :: k_CMB
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: hdiv_visous_mat_CMB1(jmax,-2:1)
      real(kind = kreal), intent(in) :: hdiv_visous_mat_CMB(jmax,-2:0)
!
      real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!$omp parallel do private(j)
      do j = 1, jmax
!       mat9(9,2*k_CMB-7,j) = mat9(9,2*k_CMB-7,j)
        mat9(8,2*k_CMB-6,j) = mat9(8,2*k_CMB-6,j)                       &
     &                       - hdiv_visous_mat_CMB1(j,-2)
!       mat9(7,2*k_CMB-5,j) = mat9(7,2*k_CMB-5,j)
        mat9(6,2*k_CMB-4,j) = mat9(6,2*k_CMB-4,j)                       &
     &                       - hdiv_visous_mat_CMB1(j,-1)
!
        mat9(5,2*k_CMB-3,j) = mat9(5,2*k_CMB-3,j) + coef_p
!
        mat9(4,2*k_CMB-2,j) = mat9(4,2*k_CMB-2,j)                       &
     &                       - hdiv_visous_mat_CMB1(j, 0)
        mat9(3,2*k_CMB-1,j) = mat9(3,2*k_CMB-1,j)
        mat9(2,2*k_CMB,  j) = mat9(2,2*k_CMB,  j)                       &
     &                       - hdiv_visous_mat_CMB1(j, 1)
        if(2*k_CMB+1 .gt. 2*nri) mat9(1,2*k_CMB+1,j) = zero
      end do
!$omp end parallel do
!
!$omp parallel do private(j)
      do j = 1, jmax
!       mat9(9,2*k_CMB-5,j) = mat9(9,2*k_CMB-5,j)
        mat9(8,2*k_CMB-4,j) = mat9(8,2*k_CMB-4,j)                       &
     &                       - hdiv_visous_mat_CMB(j,-2)
!       mat9(7,2*k_CMB-3,j) = mat9(7,2*k_CMB-3,j)
        mat9(6,2*k_CMB-2,j) = mat9(6,2*k_CMB-2,j)                       &
     &                       - hdiv_visous_mat_CMB(j,-1)
!
        mat9(5,2*k_CMB-1,j) = mat9(5,2*k_CMB-1,j) + coef_p
!
        mat9(4,2*k_CMB,  j) = mat9(4,2*k_CMB,  j)                       &
     &                       - hdiv_visous_mat_CMB(j, 0)
        if(2*k_CMB+1 .gt. 2*nri) mat9(3,2*k_CMB+1,j) = zero
        if(2*k_CMB+2 .gt. 2*nri) mat9(2,2*k_CMB+2,j) = zero
        if(2*k_CMB+3 .gt. 2*nri) mat9(1,2*k_CMB+3,j) = zero
      end do
!$omp end parallel do
!
      end subroutine sub_sph_hdiv_viscous_mat9_CMB
!
!  -------------------------------------------------------------------
!
      end module set_sph_hdiv_viscous_CMB

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
!!      subroutine add_sph_CTR_exp_horiz_div(nnod_rj, jmax, istep_j,    &
!!     &          d_vpol, hdiv_visous_j, e_hdiv_viscous)
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax, istep_j
!!        real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!        real(kind = kreal), intent(in) :: hdiv_visous_j(0:1,jmax)
!!        real(kind = kreal), intent(inout) :: e_hdiv_viscous(nnod_rj)
!!
!!      subroutine subtract_sph_CTR_hdiv_mat7(nri, jmax, hdiv_visous_j, &
!!     &                                      mat7)
!!      subroutine subtract_sph_CTR_hdiv_mat9(nri, jmax, hdiv_visous_j, &
!!     &                                      mat9)
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: hdiv_visous_j(0:1,jmax)
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
      subroutine add_sph_CTR_exp_horiz_div(nnod_rj, jmax, istep_j,      &
     &          d_vpol, hdiv_visous_j, e_hdiv_viscous)
!
      integer(kind = kint), intent(in) :: nnod_rj, jmax, istep_j
      real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
      real(kind = kreal), intent(in) :: hdiv_visous_j(0:1,jmax)
!
      real(kind = kreal), intent(inout) :: e_hdiv_viscous(nnod_rj)
!
      integer(kind = kint) :: j, iele, i_p1, inod
!
!
!$omp parallel do private(j,iele,i_p1,inod)
      do j = 1, jmax
        iele = 1 + (j-1) * istep_j
        i_p1 = iele + istep_j
        inod = iele
!
        e_hdiv_viscous(iele) = e_hdiv_viscous(iele)                     &
     &                        + hdiv_visous_j( 0,j) * d_vpol(inod)      &
     &                        + hdiv_visous_j( 1,j) * d_vpol(i_p1)
      end do
!$omp end parallel do
!
      end subroutine add_sph_CTR_exp_horiz_div
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine subtract_sph_CTR_hdiv_mat7(nri, jmax, hdiv_visous_j,   &
     &                                      mat7)
!
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: hdiv_visous_j(0:1,jmax)
!
      real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!
!$omp parallel do private(j)
      do j = 1, jmax
!       mat7(4,1,j) = mat9(5,1,j)
!
        mat7(3,2,j) = mat7(3,2,j) - hdiv_visous_j(0,j)
!       mat7(2,3,j) = mat7(2,3,j)
        mat7(1,4,j) = mat7(1,4,j) - hdiv_visous_j(1,j)
      end do
!$omp end parallel do
!
      end subroutine subtract_sph_CTR_hdiv_mat7
!
! -----------------------------------------------------------------------
!
      subroutine subtract_sph_CTR_hdiv_mat9(nri, jmax, hdiv_visous_j,   &
     &                                      mat9)
!
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: hdiv_visous_j(0:1,jmax)
!
      real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!
!$omp parallel do private(j)
      do j = 1, jmax
!       mat9(5,1,j) = mat9(5,1,j)
!
        mat9(4,2,j) = mat9(4,2,j) - hdiv_visous_j(0,j)
!       mat7(3,3,j) = mat7(3,3,j)
        mat9(2,4,j) = mat9(2,4,j) - hdiv_visous_j(1,j)
!       mat7(1,5,j) = mat7(1,5,j)
      end do
!$omp end parallel do
!
      end subroutine subtract_sph_CTR_hdiv_mat9
!
! -----------------------------------------------------------------------
!
      end module set_sph_horizontal_div_CTR


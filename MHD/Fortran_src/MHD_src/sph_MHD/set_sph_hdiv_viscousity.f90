!>@file   set_sph_hdiv_viscousity.f90
!!@brief  module set_sph_hdiv_viscousity
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2020
!
!>@brief  Substitute viscousity matrix in each layer
!!
!!@verbatim
!!      subroutine add_exp_sph_hdiv_viscous(kr, nnod_rj, jmax, coef_p,  &
!!     &          hdiv_visous_mat, d_vpol, press_e, hdiv_viscous_e)
!!        integer(kind = kint), intent(in) :: kr
!!        integer(kind = kint), intent(in) :: nnod_rj, nri, jmax
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind = kreal), intent(in) :: hdiv_visous_mat(jmax,-2:1)
!!        real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!        real(kind = kreal), intent(in) :: press_e(nnod_rj)
!!        real(kind = kreal), intent(inout) :: hdiv_viscous_e(nnod_rj)
!!
!!      subroutine sub_sph_hdiv_viscous_FDM2_mat(kr, nri, jmax, coef_p, &
!!     &                                         hdiv_visous_mat, mat7)
!!        integer(kind = kint), intent(in) :: kr
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind = kreal), intent(in) :: hdiv_visous_mat(jmax,-2:1)
!!        real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!!      subroutine sub_sph_hdiv_viscous_FDM4_mat(kr, nri, jmax, coef_p, &
!!     &                                         hdiv_visous_mat, mat9)
!!        integer(kind = kint), intent(in) :: kr
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind = kreal), intent(in) :: hdiv_visous_mat(jmax,-2:1)
!!        real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!!@endverbatim
!!
      module set_sph_hdiv_viscousity
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
      subroutine add_exp_sph_hdiv_viscous(kr, nnod_rj, jmax, coef_p,    &
     &                                    press_e, hdiv_viscous_e)
!
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: press_e(nnod_rj)
!
      real(kind = kreal), intent(inout) :: hdiv_viscous_e(nnod_rj)
!
      integer(kind = kint) :: ist, ied
!
!
      ist = 1 + (kr-1) * jmax
      ied =      kr * jmax
      hdiv_viscous_e(ist:ied) = - coef_p * press_e(ist:ied)
!
      end subroutine add_exp_sph_hdiv_viscous
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine sub_sph_hdiv_viscous_FDM2_mat(kr, nri, jmax, coef_p,   &
     &                                         hdiv_visous_mat, mat7)
!
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: hdiv_visous_mat(jmax,-2:1)
!
      real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
        mat7(7,2*kr-4,j) = mat7(7,2*kr-4,j) - hdiv_visous_mat(j,-2)
!        mat7(6,2*kr-3,j) = mat7(6,2*kr-3,j)
        mat7(5,2*kr-2,j) = mat7(5,2*kr-2,j) - hdiv_visous_mat(j,-1)
!
        mat7(4,2*kr-1,j) = mat7(4,2*kr-1,j) + coef_p
!
        mat7(3,2*kr,  j) = mat7(3,2*kr,  j) - hdiv_visous_mat(j, 0)
!        mat7(2,2*kr+1,j) = mat7(2,2*kr+1,j)
        mat7(1,2*kr+2,j) = mat7(1,2*kr+2,j) - hdiv_visous_mat(j, 1)
      end do
!
      end subroutine sub_sph_hdiv_viscous_FDM2_mat
!
!  -------------------------------------------------------------------
!
      subroutine sub_sph_hdiv_viscous_FDM4_mat(kr, nri, jmax, coef_p,   &
     &                                         hdiv_visous_mat, mat9)
!
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in) :: hdiv_visous_mat(jmax,-2:1)
!
      real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!
      integer(kind = kint) :: j
!
!
      do j = 1, jmax
!        mat9(9,2*kr-5,j) = mat9(9,2*kr-5,j)
        mat9(8,2*kr-4,j) = mat9(8,2*kr-4,j) - hdiv_visous_mat(j,-2)
!        mat9(7,2*kr-3,j) = mat9(7,2*kr-3,j)
        mat9(6,2*kr-2,j) = mat9(6,2*kr-2,j) - hdiv_visous_mat(j,-1)
!
        mat9(5,2*kr-1,j) = mat9(5,2*kr-1,j) + coef_p
!
        mat9(4,2*kr,  j) = mat9(4,2*kr,  j) - hdiv_visous_mat(j, 0)
!        mat9(3,2*kr+1,j) = mat9(3,2*kr+1,j)
        mat9(2,2*kr+2,j) = mat9(2,2*kr+2,j) - hdiv_visous_mat(j, 1)
!        mat9(1,2*kr+3,j) = mat9(1,2*kr+3,j)
      end do
!
      end subroutine sub_sph_hdiv_viscous_FDM4_mat
!
!  -------------------------------------------------------------------
!
      end module set_sph_hdiv_viscousity

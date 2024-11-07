!>@file   sph_hdiv_viscousity_CTR.f90
!!@brief  module sph_hdiv_viscousity_CTR
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Set FDM matrix and explicit horizontal diffusivity
!!      for Valuable density
!!
!!@verbatim
!!      subroutine set_sph_exp_hdiv_viscous_CTR                         &
!!     &         (flag_viscous_variation, flag_ref_density_valiation,   &
!!     &          nnod_rj, jmax, r_innermost, g_sph_rj,                 &
!!     &          coef_p, coef_d, relative_d, h_nu, h_rho, h_drhodr,    &
!!     &          fdm3e_center_mat, d_vpol, hdiv_visous_j,              &
!!     &          e_hdiv_viscous)
!!        logical, intent(in) :: flag_viscous_variation
!!        logical, intent(in) :: flag_ref_density_valiation
!!        integer(kind = kint), intent(in) :: nnod_rj, jmax
!!        real(kind = kreal), intent(in) :: r_innermost
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
!!        real(kind = kreal), intent(in) :: coef_p, coef_d
!!        real(kind = kreal), intent(in) :: relative_d
!!        real(kind = kreal), intent(in) :: h_nu, h_rho, h_drhodr
!!        real(kind = kreal), intent(in) :: fdm3e_center_mat(-2:1,4)
!!        real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!!        real(kind = kreal), intent(inout) :: hdiv_visous_j(jmax,0:1)
!!        real(kind = kreal), intent(inout) :: e_hdiv_viscous(nnod_rj)
!!      subroutine set_sph_vpol_press_CTR_mat7                          &
!!     &         (flag_viscous_variation, flag_ref_density_valiation,   &
!!     &          nri, jmax, r_innermost, g_sph_rj, coef_p, coef_d,     &
!!     &          relative_d, h_nu, h_rho, h_drhodr, fdm3e_center_mat,  &
!!     &          hdiv_visous_j, mat7)
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!!      subroutine set_sph_vpol_press_CTR_mat9                          &
!!     &         (flag_viscous_variation, flag_ref_density_valiation,   &
!!     &          nri, jmax, r_innermost, g_sph_rj, coef_p, coef_d,     &
!!     &          relative_d, h_nu, h_rho, h_drhodr, fdm3e_center_mat,  &
!!     &          hdiv_visous_j, mat9)
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!!@endverbatim
!
      module sph_hdiv_viscousity_CTR
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
      subroutine set_sph_exp_hdiv_viscous_CTR                           &
     &         (flag_viscous_variation, flag_ref_density_valiation,     &
     &          nnod_rj, jmax, r_innermost, g_sph_rj,                   &
     &          coef_p, coef_d, relative_d, h_nu, h_rho, h_drhodr,      &
     &          fdm3e_center_mat, d_vpol, hdiv_visous_j,                &
     &          e_hdiv_viscous)
!
      use sph_hdiv_viscous_coefs_CTR
      use set_sph_pol_hdiv_viscs_CTR
!
      logical, intent(in) :: flag_viscous_variation
      logical, intent(in) :: flag_ref_density_valiation
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      real(kind = kreal), intent(in) :: r_innermost
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: coef_p, coef_d
      real(kind = kreal), intent(in) :: relative_d
      real(kind = kreal), intent(in) :: h_nu, h_rho, h_drhodr
      real(kind = kreal), intent(in) :: fdm3e_center_mat(-2:1,4)
!
      real(kind = kreal), intent(in) :: d_vpol(nnod_rj)
!
      real(kind = kreal), intent(inout) :: hdiv_visous_j(jmax,0:1)
      real(kind = kreal), intent(inout) :: e_hdiv_viscous(nnod_rj)
!
      integer(kind = kint) :: iele
!
!
!$omp parallel do private(iele)
      do iele = 1, jmax
        e_hdiv_viscous(iele) = 0.0d0
      end do
!$omp end parallel do
!
      call s_sph_hdiv_coefficients_CTR                                  &
     &   (flag_viscous_variation, flag_ref_density_valiation,           &
     &    jmax, r_innermost, g_sph_rj, coef_d, relative_d,              &
     &    h_nu, h_rho, h_drhodr, fdm3e_center_mat, hdiv_visous_j)
      call add_exp_sph_hdiv_viscous_CTR(nnod_rj, jmax,                  &
     &    coef_p, hdiv_visous_j, d_vpol, e_hdiv_viscous)
!
      end subroutine set_sph_exp_hdiv_viscous_CTR
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_sph_vpol_press_CTR_mat7                            &
     &         (flag_viscous_variation, flag_ref_density_valiation,     &
     &          nri, jmax, r_innermost, g_sph_rj, coef_p, coef_d,       &
     &          relative_d, h_nu, h_rho, h_drhodr, fdm3e_center_mat,    &
     &          hdiv_visous_j, mat7)
!
      use sph_hdiv_viscous_coefs_CTR
      use set_sph_pol_hdiv_viscs_CTR
!
      logical, intent(in) :: flag_viscous_variation
      logical, intent(in) :: flag_ref_density_valiation
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: r_innermost
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: coef_p, coef_d
      real(kind = kreal), intent(in) :: relative_d
      real(kind = kreal), intent(in) :: h_nu, h_rho, h_drhodr
      real(kind = kreal), intent(in) :: fdm3e_center_mat(-2:1,4)
!
      real(kind = kreal), intent(inout) :: hdiv_visous_j(jmax,0:1)
      real(kind = kreal), intent(inout) :: mat7(7,2*nri,jmax)
!
!
!$omp parallel workshare
      hdiv_visous_j(1:jmax,0:1) = zero
!$omp end parallel workshare
!
!$omp parallel workshare
      mat7(4,1,1:jmax) = 0
      mat7(3,2,1:jmax) = zero
      mat7(2,3,1:jmax) = zero
      mat7(1,4,1:jmax) = zero
!$omp end parallel workshare
!
      call s_sph_hdiv_coefficients_CTR                                  &
     &   (flag_viscous_variation, flag_ref_density_valiation,           &
     &    jmax, r_innermost, g_sph_rj, coef_d, relative_d,              &
     &    h_nu, h_rho, h_drhodr, fdm3e_center_mat, hdiv_visous_j)
      call sub_sph_hdiv_viscous_mat7_CTR(nri, jmax, coef_p,             &
     &                                   hdiv_visous_j, mat7)
!
      end subroutine set_sph_vpol_press_CTR_mat7
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_vpol_press_CTR_mat9                            &
     &         (flag_viscous_variation, flag_ref_density_valiation,     &
     &          nri, jmax, r_innermost, g_sph_rj, coef_p, coef_d,       &
     &          relative_d, h_nu, h_rho, h_drhodr, fdm3e_center_mat,    &
     &          hdiv_visous_j, mat9)
!
      use sph_hdiv_viscous_coefs_CTR
      use set_sph_pol_hdiv_viscs_CTR
!
      logical, intent(in) :: flag_viscous_variation
      logical, intent(in) :: flag_ref_density_valiation
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: r_innermost
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: coef_p, coef_d
      real(kind = kreal), intent(in) :: relative_d
      real(kind = kreal), intent(in) :: h_nu, h_rho, h_drhodr
      real(kind = kreal), intent(in) :: fdm3e_center_mat(-2:1,4)
!
      real(kind = kreal), intent(inout) :: hdiv_visous_j(jmax,0:1)
      real(kind = kreal), intent(inout) :: mat9(9,2*nri,jmax)
!
!
!$omp parallel workshare
      hdiv_visous_j(1:jmax,0:1) = zero
!$omp end parallel workshare
!
!$omp parallel workshare
      mat9(5,1,1:jmax) = zero
      mat9(4,2,1:jmax) = zero
      mat9(3,3,1:jmax) = zero
      mat9(2,4,1:jmax) = zero
      mat9(1,5,1:jmax) = zero
!$omp end parallel workshare
!
      call s_sph_hdiv_coefficients_CTR                                  &
     &   (flag_viscous_variation, flag_ref_density_valiation,           &
     &    jmax, r_innermost, g_sph_rj, coef_d, relative_d,              &
     &    h_nu, h_rho, h_drhodr, fdm3e_center_mat, hdiv_visous_j)
      call sub_sph_hdiv_viscous_mat9_CTR(nri, jmax, coef_p,             &
     &                                   hdiv_visous_j, mat9)
!
      end subroutine set_sph_vpol_press_CTR_mat9
!
! -----------------------------------------------------------------------
!
      end module sph_hdiv_viscousity_CTR

!>@file   sph_hdiv_viscous_coefs_CTR.f90
!!@brief  module sph_hdiv_viscous_coefs_CTR
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Set FDM matrix and explicit horizontal diffusivity
!!      for Valuable density
!!
!!@verbatim
!!      subroutine s_sph_hdiv_coefficients_CTR                          &
!!     &         (flag_viscous_variation, flag_ref_density_valiation,   &
!!     &          jmax, r_innermost, g_sph_rj, coef_d, relative_d,      &
!!     &          h_nu, h_rho, h_drhodr, fdm3e_center_mat,              &
!!     &          hdiv_visous_j)
!!        logical, intent(in) :: flag_viscous_variation
!!        logical, intent(in) :: flag_ref_density_valiation
!!        integer(kind = kint), intent(in) :: jmax
!!        real(kind = kreal), intent(in) :: r_innermost
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
!!        real(kind = kreal), intent(in) :: coef_d
!!        real(kind = kreal), intent(in) :: relative_d
!!        real(kind = kreal), intent(in) :: h_nu, h_rho, h_drhodr
!!        real(kind = kreal), intent(in) :: fdm3e_center_mat(-2:1,4)
!!        real(kind = kreal), intent(inout) :: hdiv_visous_j(0:1,jmax)
!!@endverbatim
!
      module sph_hdiv_viscous_coefs_CTR
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: cal_sph_hdiv_viscousity_CTR
      private :: add_valuable_viscosity_CTR
      private :: add_hdiv_viscous_val_rho_CTR
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_sph_hdiv_coefficients_CTR                            &
     &         (flag_viscous_variation, flag_ref_density_valiation,     &
     &          jmax, r_innermost, g_sph_rj, coef_d, relative_d,        &
     &          h_nu, h_rho, h_drhodr, fdm3e_center_mat,                &
     &          hdiv_visous_j)
!
      logical, intent(in) :: flag_viscous_variation
      logical, intent(in) :: flag_ref_density_valiation
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(in) :: r_innermost
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d
      real(kind = kreal), intent(in) :: h_nu, h_rho, h_drhodr
      real(kind = kreal), intent(in) :: fdm3e_center_mat(-2:1,4)
!
      real(kind = kreal), intent(inout) :: hdiv_visous_j(jmax,0:1)
!
      real(kind = kreal) :: coef_CTR
      real(kind = kreal) :: ar_mid(3)
!
!
      ar_mid(1) = two / r_innermost
      ar_mid(2) = ar_mid(1) * ar_mid(1)
      ar_mid(3) = ar_mid(1) * ar_mid(2)
!
      call cal_sph_hdiv_viscousity_CTR(jmax, g_sph_rj, ar_mid,          &
     &                                 fdm3e_center_mat, hdiv_visous_j)
!
      if(flag_viscous_variation .and. flag_ref_density_valiation) then
        call add_valuable_viscosity_CTR(jmax, g_sph_rj, ar_mid,         &
     &      h_nu, fdm3e_center_mat, hdiv_visous_j)
        call add_hdiv_viscous_val_rho_CTR(jmax, g_sph_rj, ar_mid,       &
     &      h_nu, h_rho, h_drhodr, fdm3e_center_mat, hdiv_visous_j)
        coef_CTR = coef_d * relative_d
      else if(flag_viscous_variation) then
        call add_valuable_viscosity_CTR(jmax, g_sph_rj, ar_mid,         &
     &      h_nu, fdm3e_center_mat, hdiv_visous_j)
        coef_CTR = coef_d * relative_d
      else if(flag_ref_density_valiation) then
        call add_hdiv_viscous_val_rho_CTR(jmax, g_sph_rj, ar_mid,       &
     &      zero, h_rho, h_drhodr, fdm3e_center_mat, hdiv_visous_j)
        coef_CTR = coef_d * relative_d
      else
        coef_CTR = coef_d
      end if
!
!$omp parallel workshare
      hdiv_visous_j(1:jmax,0:1) = coef_CTR * hdiv_visous_j(1:jmax,0:1)
!$omp end parallel workshare
!
      end subroutine s_sph_hdiv_coefficients_CTR
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_hdiv_viscousity_CTR(jmax, g_sph_rj, ar_mid,    &
     &          fdm3e_center_mat, hdiv_visous_j)
!
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: ar_mid(3)
      real(kind = kreal), intent(in) :: fdm3e_center_mat(-2:1,4)
!
      real(kind = kreal), intent(inout) :: hdiv_visous_j(jmax,0:1)
!
      integer(kind = kint) :: j
      real(kind = kreal) :: c_d3, c_d1, c_d0
!
!
        c_d3 = -one
!$omp parallel do private(j,c_d1,c_d0)
        do j = 1, jmax
          c_d1 =        g_sph_rj(j,3)*ar_mid(2)
          c_d0 = -two * g_sph_rj(j,3)*ar_mid(3)
          hdiv_visous_j(j,0:1) =        c_d3 * fdm3e_center_mat(0:1,4)  &
     &                                + c_d1 * fdm3e_center_mat(0:1,2)  &
     &                                + c_d0 * fdm3e_center_mat(0:1,1)
        end do
!$omp end parallel do
!
      end subroutine cal_sph_hdiv_viscousity_CTR
!
! -----------------------------------------------------------------------
!
      subroutine add_valuable_viscosity_CTR(jmax, g_sph_rj, ar_mid,     &
     &          h_nu, fdm3e_center_mat, hdiv_visous_j)
!
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: ar_mid(3)
      real(kind = kreal), intent(in) :: h_nu
      real(kind = kreal), intent(in) :: fdm3e_center_mat(-2:1,4)
!
      real(kind = kreal), intent(inout) :: hdiv_visous_j(jmax,0:1)
!
      integer(kind = kint) :: j
      real(kind = kreal) :: c_d2, c_d1, c_d0
!
!
        c_d2 = - h_nu
        c_d1 = two * ar_mid(1) * h_nu
!$omp parallel do private(j,c_d0)
        do j = 1, jmax
          c_d0 = - g_sph_rj(j,3)*ar_mid(2) * h_nu
          hdiv_visous_j(j,0:1) = hdiv_visous_j(j,0:1)                   &
     &                                + c_d2 * fdm3e_center_mat(0:1,3)  &
     &                                + c_d1 * fdm3e_center_mat(0:1,2)  &
     &                                + c_d0 * fdm3e_center_mat(0:1,1)
        end do
!$omp end parallel do
!
      end subroutine add_valuable_viscosity_CTR
!
! -----------------------------------------------------------------------
!
      subroutine add_hdiv_viscous_val_rho_CTR(jmax, g_sph_rj, ar_mid,   &
     &          h_nu, h_rho, h_drhodr, fdm3e_center_mat, hdiv_visous_j)
!
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: ar_mid(3)
      real(kind = kreal), intent(in) :: h_nu, h_rho, h_drhodr
      real(kind = kreal), intent(in) :: fdm3e_center_mat(-2:1,4)
!
      real(kind = kreal), intent(inout) :: hdiv_visous_j(jmax,0:1)
!
      integer(kind = kint) :: j
      real(kind = kreal) :: c_d2, c_d1, c_d0
!
!
        c_d2 = h_rho
        c_d1 = two * ar_mid(1) * h_rho  + h_drhodr + h_nu * h_rho
!$omp parallel do private(j,c_d0)
        do j = 1, jmax
          c_d0 = - g_sph_rj(j,3)*ar_mid(2)  * h_rho * two / three
          hdiv_visous_j(j,0:1) = hdiv_visous_j(j,0:1)                   &
     &                                + c_d2 * fdm3e_center_mat(0:1,3)  &
     &                                + c_d1 * fdm3e_center_mat(0:1,2)  &
     &                                + c_d0 * fdm3e_center_mat(0:1,1)
        end do
!$omp end parallel do
!
      end subroutine add_hdiv_viscous_val_rho_CTR
!
! -----------------------------------------------------------------------
!
      end module sph_hdiv_viscous_coefs_CTR

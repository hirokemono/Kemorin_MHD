!>@file   sph_valuable_viscosity_CTR.f90
!!@brief  module sph_valuable_viscosity_CTR
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Set FDM matrix and explicit horizontal diffusivity
!!      for Valuable density
!!
!!@verbatim
!!      subroutine add_sph_exp_hdiv_val_nu_CTR(sph_rj, g_sph_rj, coef_d,&
!!     &          relative_d, h_nu, fdm3e_center_mat, is_velo,          &
!!     &          n_point, ntot_phys_rj, d_rj,                          &
!!     &          e_hdiv_viscous, hdiv_visous_j)
!!      subroutine add_sph_val_viscosity_CTR_mat7(sph_rj, g_sph_rj,     &
!!     &          coef_d, relative_d, h_nu, fdm3e_center_mat,           &
!!     &          mat7, hdiv_visous_j)
!!      subroutine add_sph_val_viscosity_CTR_mat9(sph_rj, g_sph_rj,     &
!!     &          coef_d, relative_d, h_nu, fdm3e_center_mat,           &
!!     &          mat9, hdiv_visous_j)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        real(kind = kreal), intent(in)                                &
!!     &                     :: g_sph_rj(sph_rj%nidx_rj(2),17)
!!        real(kind = kreal), intent(in) :: coef_d
!!        real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
!!        real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
!!        real(kind = kreal), intent(in) :: fdm3e_center_mat(-2:1,4)
!!        integer(kind = kint), intent(in) :: is_velo
!!        integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
!!        real(kind = kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
!!
!!        real(kind = kreal), intent(inout) :: e_hdiv_viscous(n_point)
!!        real(kind = kreal), intent(inout)                             &
!!     &             :: mat7(7,2*sph_rj%nidx_rj(1),sph_rj%nidx_rj(2))
!!        real(kind = kreal), intent(inout)                             &
!!     &             :: mat9(9,2*sph_rj%nidx_rj(1),sph_rj%nidx_rj(2))
!!        real(kind = kreal), intent(inout)                             &
!!     &             :: hdiv_visous_j(0:1,sph_rj%nidx_rj(2))
!!@endverbatim
!
      module sph_valuable_viscosity_CTR
!
      use m_precision
      use m_constants
      use t_spheric_rj_data
!
      implicit none
!
      private :: s_sph_valuable_viscosity_CTR
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine add_sph_exp_hdiv_val_nu_CTR(sph_rj, g_sph_rj, coef_d,  &
     &          relative_d, h_nu, fdm3e_center_mat, is_velo,            &
     &          n_point, ntot_phys_rj, d_rj,                            &
     &          e_hdiv_viscous, hdiv_visous_j)
!
      use set_sph_horizontal_div_CTR
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: fdm3e_center_mat(-2:1,4)
!
      integer(kind = kint), intent(in) :: is_velo
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal), intent(inout) :: e_hdiv_viscous(n_point)
      real(kind = kreal), intent(inout)                                 &
     &                   :: hdiv_visous_j(0:1,sph_rj%nidx_rj(2))
!
!
      call s_sph_valuable_viscosity_CTR                                 &
     &   (sph_rj%nidx_rj(2), sph_rj%radius_1d_rj_r(1), g_sph_rj,        &
     &    coef_d, relative_d(1), h_nu(1), fdm3e_center_mat,             &
     &    hdiv_visous_j)
!
      call add_sph_CTR_exp_horiz_div                                    &
     &   (sph_rj%nnod_rj, sph_rj%nidx_rj(2), sph_rj%istep_rj(2),        &
     &    d_rj(1,is_velo), hdiv_visous_j, e_hdiv_viscous)
!
      end subroutine add_sph_exp_hdiv_val_nu_CTR
!
! -----------------------------------------------------------------------
!
      subroutine add_sph_val_viscosity_CTR_mat7(sph_rj, g_sph_rj,       &
     &          coef_d, relative_d, h_nu, fdm3e_center_mat,             &
     &          mat7, hdiv_visous_j)
!
      use set_sph_horizontal_div_CTR
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: fdm3e_center_mat(-2:1,4)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat7(7,2*sph_rj%nidx_rj(1),sph_rj%nidx_rj(2))
      real(kind = kreal), intent(inout)                                 &
     &                   :: hdiv_visous_j(0:1,sph_rj%nidx_rj(2))
!
!
!
      call s_sph_valuable_viscosity_CTR                                 &
     &   (sph_rj%nidx_rj(2), sph_rj%radius_1d_rj_r(1), g_sph_rj,        &
     &    coef_d, relative_d(1), h_nu(1), fdm3e_center_mat,             &
     &    hdiv_visous_j)
!
      call subtract_sph_CTR_hdiv_mat7                                   &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), hdiv_visous_j, mat7)
!
      end subroutine add_sph_val_viscosity_CTR_mat7
!
! -----------------------------------------------------------------------
!
      subroutine add_sph_val_viscosity_CTR_mat9(sph_rj, g_sph_rj,       &
     &          coef_d, relative_d, h_nu, fdm3e_center_mat,             &
     &          mat9, hdiv_visous_j)
!
      use set_sph_horizontal_div_CTR
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      real(kind = kreal), intent(in) :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: fdm3e_center_mat(-2:1,4)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat9(9,2*sph_rj%nidx_rj(1),sph_rj%nidx_rj(2))
      real(kind = kreal), intent(inout)                                 &
     &                   :: hdiv_visous_j(0:1,sph_rj%nidx_rj(2))
!
!
      call s_sph_valuable_viscosity_CTR                                 &
     &   (sph_rj%nidx_rj(2), sph_rj%radius_1d_rj_r(1), g_sph_rj,        &
     &    coef_d, relative_d(1), h_nu(1), fdm3e_center_mat,             &
     &    hdiv_visous_j)
!
      call subtract_sph_CTR_hdiv_mat9                                   &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), hdiv_visous_j, mat9)
!
      end subroutine add_sph_val_viscosity_CTR_mat9
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine s_sph_valuable_viscosity_CTR(jmax, r_innermost,        &
     &          g_sph_rj, coef_d, relative_d, h_nu, fdm3e_center_mat,   &
     &          hdiv_visous_j)
!
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(in) :: r_innermost
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d
      real(kind = kreal), intent(in) :: h_nu
      real(kind = kreal), intent(in) :: fdm3e_center_mat(-2:1,4)
!
      real(kind = kreal), intent(inout) :: hdiv_visous_j(0:1,jmax)
!
      integer(kind = kint) :: j
      real(kind = kreal) :: r_mid, ar_mid(3), d_mid, c_d2, c_d1, c_d0
!
!
        d_mid =        relative_d
        r_mid = half * r_innermost
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d2 = - h_nu
        c_d1 = two * ar_mid(1) * h_nu
!$omp parallel do private(j,c_d0)
        do j = 1, jmax
          c_d0 = - g_sph_rj(j,3)*ar_mid(2) * h_nu
          hdiv_visous_j(0:1,j)                                          &
     &              = coef_d * d_mid * (c_d2 * fdm3e_center_mat(0:1,3)  &
     &                                + c_d1 * fdm3e_center_mat(0:1,2)  &
     &                                + c_d0 * fdm3e_center_mat(0:1,1))
        end do
!$omp end parallel do
!
      end subroutine s_sph_valuable_viscosity_CTR
!
! -----------------------------------------------------------------------
!
      end module sph_valuable_viscosity_CTR

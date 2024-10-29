!>@file   sph_valuable_density_CTR.f90
!!@brief  module sph_valuable_density_CTR
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Set FDM matrix and explicit horizontal diffusivity
!!      for Valuable density
!!
!!@verbatim
!!      subroutine add_sph_exp_hdiv_val_rho_CTR(sph_rj, g_sph_rj,       &
!!     &          coef_d, relative_d, h_nu, h_rho, fdm3e_center_mat,    &
!!     &          is_velo, n_point, ntot_phys_rj, d_rj,                 &
!!     &          e_hdiv_viscous, hdiv_visous_j)
!!      subroutine add_sph_val_density_CTR_mat7(sph_rj, g_sph_rj,       &
!!     &          coef_d, relative_d, h_nu, h_rho, fdm3e_center_mat,    &
!!     &          mat7, hdiv_visous_j)
!!      subroutine add_sph_val_density_CTR_mat9(sph_rj, g_sph_rj,       &
!!     &          coef_d, relative_d, h_nu, h_rho, fdm3e_center_mat,    &
!!     &          mat9, hdiv_visous_j)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        real(kind = kreal), intent(in)                                &
!!     &                     :: g_sph_rj(sph_rj%nidx_rj(2),17)
!!        real(kind = kreal), intent(in) :: coef_d
!!        real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
!!        real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
!!        real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1),0:1)
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
      module sph_valuable_density_CTR
!
      use m_precision
      use m_constants
      use t_spheric_rj_data
!
      implicit none
!
      private :: s_sph_valuable_density_CTR
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine add_sph_exp_hdiv_val_rho_CTR(sph_rj, g_sph_rj,         &
     &          coef_d, relative_d, h_nu, h_rho, fdm3e_center_mat,      &
     &          is_velo, n_point, ntot_phys_rj, d_rj,                   &
     &          e_hdiv_viscous, hdiv_visous_j)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      real(kind = kreal), intent(in)                                    &
     &                   :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1),0:1)
      real(kind = kreal), intent(in) :: fdm3e_center_mat(-2:1,4)
!
      integer(kind = kint), intent(in) :: is_velo
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real(kind = kreal), intent(in) :: d_rj(n_point,ntot_phys_rj)
!
      real(kind = kreal), intent(inout) :: e_hdiv_viscous(n_point)
      real(kind = kreal), intent(inout)                                 &
     &                   :: hdiv_visous_j(2,sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: j, iele
!
!
      call s_sph_valuable_density_CTR(sph_rj, g_sph_rj,                 &
     &    coef_d, relative_d(1), h_nu(1), h_rho(1,0), h_rho(1,1),       &
     &    fdm3e_center_mat, hdiv_visous_j(1,1))
!
!$omp parallel do private(j,iele)
      do j = 1, sph_rj%nidx_rj(2)
        iele = 1 + (j-1) * sph_rj%istep_rj(2)
        e_hdiv_viscous(iele) = relative_d(1) * e_hdiv_viscous(iele)
      end do
!$omp end parallel do
!
      call add_sph_CTR_exp_horiz_div                                    &
     &   (sph_rj%nnod_rj, sph_rj%nidx_rj(2), sph_rj%istep_rj(2),        &
     &    d_rj(1,is_velo), hdiv_visous_j, e_hdiv_viscous)
!
      end subroutine add_sph_exp_hdiv_val_rho_CTR
!
! -----------------------------------------------------------------------
!
      subroutine add_sph_val_density_CTR_mat7(sph_rj, g_sph_rj,         &
     &          coef_d, relative_d, h_nu, h_rho, fdm3e_center_mat,      &
     &          mat7, hdiv_visous_j)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      real(kind = kreal), intent(in)                                    &
     &                   :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1),0:1)
      real(kind = kreal), intent(in) :: fdm3e_center_mat(-2:1,4)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat7(7,2*sph_rj%nidx_rj(1),sph_rj%nidx_rj(2))
      real(kind = kreal), intent(inout)                                 &
     &                   :: hdiv_visous_j(0:1,sph_rj%nidx_rj(2))
!
!
      call s_sph_valuable_density_CTR(sph_rj, g_sph_rj,                 &
     &    coef_d, relative_d(1), h_nu(1), h_rho(1,0), h_rho(1,1),       &
     &    fdm3e_center_mat, hdiv_visous_j)
      call subtract_sph_CTR_hdiv_mat7                                   &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), hdiv_visous_j, mat7)
!
      end subroutine add_sph_val_density_CTR_mat7
!
! -----------------------------------------------------------------------
!
      subroutine add_sph_val_density_CTR_mat9(sph_rj, g_sph_rj,         &
     &          coef_d, relative_d, h_nu, h_rho, fdm3e_center_mat,      &
     &          mat9, hdiv_visous_j)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      real(kind = kreal), intent(in)                                    &
     &                   :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_nu(sph_rj%nidx_rj(1))
      real(kind = kreal), intent(in) :: h_rho(sph_rj%nidx_rj(1),0:1)
      real(kind = kreal), intent(in) :: fdm3e_center_mat(-2:1,4)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat9(9,2*sph_rj%nidx_rj(1),sph_rj%nidx_rj(2))
      real(kind = kreal), intent(inout)                                 &
     &           :: hdiv_visous_j(0:1,sph_rj%nidx_rj(2))
!
!
      call s_sph_valuable_density_CTR(sph_rj, g_sph_rj,                 &
     &    coef_d, relative_d(1), h_nu(1), h_rho(1,0), h_rho(1,1),       &
     &    fdm3e_center_mat, hdiv_visous_j)
!
      call subtract_sph_CTR_hdiv_mat9                                   &
     &   (sph_rj%nidx_rj(1), sph_rj%nidx_rj(2), hdiv_visous_j, mat9)
!
      end subroutine add_sph_val_density_CTR_mat9
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine s_sph_valuable_density_CTR(sph_rj, g_sph_rj,           &
     &           coef_d, relative_d, h_nu, h_rho, h_drhodr,             &
     &           fdm3e_center_mat, hdiv_visous_j)
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      real(kind = kreal), intent(in)                                    &
     &                   :: g_sph_rj(sph_rj%nidx_rj(2),17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d
      real(kind = kreal), intent(in) :: h_nu, h_rho, h_drhodr
      real(kind = kreal), intent(in) :: fdm3e_center_mat(-2:1,4)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: hdiv_visous_j(0:1,sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: j
      real(kind = kreal) :: r_mid, ar_mid(3), d_mid, c_d2, c_d1, c_d0
!
!
        d_mid = relative_d
        r_mid = half * sph_rj%radius_1d_rj_r(1)
        ar_mid(1) = one / r_mid
        ar_mid(2) = ar_mid(1) * ar_mid(1)
        ar_mid(3) = ar_mid(1) * ar_mid(2)
!
        c_d2 = h_rho
        c_d1 = two * ar_mid(1) * h_rho  + h_drhodr + h_nu * h_rho
!$omp parallel do private(j,c_d0,hdiv_visous_j)
        do j = 1, sph_rj%nidx_rj(2)
          c_d0 = - g_sph_rj(j,3)*ar_mid(2)  * h_rho * two / three
          hdiv_visous_j(0:1,j)                                          &
     &              = coef_d * d_mid * (c_d2 * fdm3e_center_mat(0:1,3)  &
     &                                + c_d1 * fdm3e_center_mat(0:1,2)  &
     &                                + c_d0 * fdm3e_center_mat(0:1,1))
        end do
!$omp end parallel do
!
      end subroutine s_sph_valuable_density_CTR
!
! -----------------------------------------------------------------------
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
!        mat7(4,1,j) = coef_p
!
        mat7(3,2,j) = mat7(3,2,j) - hdiv_visous_j(0,j)
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
!        mat9(5,1,j) = coef_p
!
        mat9(4,2,j) = mat9(4,2,j) - hdiv_visous_j(0,j)
        mat9(2,4,j) = mat9(2,4,j) - hdiv_visous_j(1,j)
      end do
!$omp end parallel do
!
      end subroutine subtract_sph_CTR_hdiv_mat9
!
! -----------------------------------------------------------------------
!
      end module sph_valuable_density_CTR

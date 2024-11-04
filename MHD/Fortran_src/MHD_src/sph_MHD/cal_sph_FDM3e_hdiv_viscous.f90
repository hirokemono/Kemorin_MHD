!>@file   cal_sph_FDM3e_hdiv_viscous.f90
!!@brief  module cal_sph_FDM3e_hdiv_viscous
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Set FDM matrix and explicit horizontal diffusivity
!!      for Valuable density
!!
!!@verbatim
!!      subroutine cal_sph_hdiv_viscousity(kr_st, kr_ed,                &
!!     &         flag_viscous_variation, flag_ref_density_valiation,    &
!!     &         nri, jmax, a1r_ele_rj, a2r_ele_rj, a3r_ele_rj,         &
!!     &         g_sph_rj, coef_d, relative_d, h_nu, h_rho, h_drhodr,   &
!!     &         fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat, fdm3e_d3_mat,&
!!     &         hdiv_visous_mat)
!!        logical, intent(in) :: flag_viscous_variation
!!        logical, intent(in) :: flag_ref_density_valiation
!!        integer(kind = kint), intent(in) :: kr_st, kr_ed
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: a1r_ele_rj(nri)
!!        real(kind = kreal), intent(in) :: a2r_ele_rj(nri)
!!        real(kind = kreal), intent(in) :: a3r_ele_rj(nri)
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
!!        real(kind = kreal), intent(in) :: coef_d
!!        real(kind = kreal), intent(in) :: relative_d(nri), h_nu(nri)
!!        real(kind = kreal), intent(in) :: h_rho(nri), h_drhodr(nri)
!!        real(kind = kreal), intent(in) :: fdm3e_d0_mat(nri,-2:1)
!!        real(kind = kreal), intent(in) :: fdm3e_d1_mat(nri,-2:1)
!!        real(kind = kreal), intent(in) :: fdm3e_d2_mat(nri,-2:1)
!!        real(kind = kreal), intent(in) :: fdm3e_d3_mat(nri,-2:1)
!!        real(kind = kreal), intent(inout)                             &
!!     &                   :: hdiv_visous_mat(-2:1,jmax,nri)
!!@endverbatim
!
      module cal_sph_FDM3e_hdiv_viscous
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: cal_sph_hdiv_fixed_viscosity
      private :: add_sph_hdiv_viscous_rho_depend
      private :: add_sph_hdiv_viscous_rho_nu_dep
      private :: add_sph_hdiv_viscous_nu_depend
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_hdiv_viscousity(kr_st, kr_ed,                  &
     &         flag_viscous_variation, flag_ref_density_valiation,      &
     &         nri, jmax, a1r_ele_rj, a2r_ele_rj, a3r_ele_rj,           &
     &         g_sph_rj, coef_d, relative_d, h_nu, h_rho, h_drhodr,     &
     &         fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat, fdm3e_d3_mat,  &
     &         hdiv_visous_mat)
!
      logical, intent(in) :: flag_viscous_variation
      logical, intent(in) :: flag_ref_density_valiation
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: a1r_ele_rj(nri)
      real(kind = kreal), intent(in) :: a2r_ele_rj(nri)
      real(kind = kreal), intent(in) :: a3r_ele_rj(nri)
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(nri), h_nu(nri)
      real(kind = kreal), intent(in) :: h_rho(nri), h_drhodr(nri)
      real(kind = kreal), intent(in) :: fdm3e_d0_mat(nri,-2:1)
      real(kind = kreal), intent(in) :: fdm3e_d1_mat(nri,-2:1)
      real(kind = kreal), intent(in) :: fdm3e_d2_mat(nri,-2:1)
      real(kind = kreal), intent(in) :: fdm3e_d3_mat(nri,-2:1)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: hdiv_visous_mat(-2:1,jmax,nri)
!
      integer(kind = kint) :: ist, ied
!
!
      ist =  1 + (kr_st - 1) * jmax
      ied =  kr_ed * jmax
!
      call cal_sph_hdiv_fixed_viscosity(ist, ied, nri, jmax,            &
     &    a2r_ele_rj, a3r_ele_rj, g_sph_rj,                             &
     &    fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d3_mat, hdiv_visous_mat)
!
      if(flag_ref_density_valiation) then
        call add_sph_hdiv_viscous_rho_depend(ist, ied, nri, jmax,       &
     &      a1r_ele_rj, a2r_ele_rj, g_sph_rj, h_rho, h_drhodr,          &
     &      fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat, hdiv_visous_mat)
!
        if(flag_viscous_variation) then
          call add_sph_hdiv_viscous_rho_nu_dep(ist, ied, nri, jmax,     &
     &        h_nu, h_rho, fdm3e_d1_mat, hdiv_visous_mat)
        end if
      end if
      if(flag_viscous_variation) then
        call add_sph_hdiv_viscous_nu_depend(ist, ied, nri, jmax,        &
     &      a1r_ele_rj, a2r_ele_rj, g_sph_rj, relative_d, h_nu,         &
     &      fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat, hdiv_visous_mat)
      end if
!
!$omp parallel workshare
      hdiv_visous_mat(-2:1,1:jmax,kr_st:kr_ed)                          &
     &       = coef_d * hdiv_visous_mat(-2:1,1:jmax,kr_st:kr_ed)
!$omp end parallel workshare
!
      end subroutine cal_sph_hdiv_viscousity
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_hdiv_fixed_viscosity(ist, ied, nri, jmax,      &
     &          a2r_ele_rj, a3r_ele_rj, g_sph_rj,                       &
     &          fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d3_mat,               &
     &          hdiv_visous_mat)
!
      integer(kind = kint), intent(in) :: ist, ied
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: a2r_ele_rj(nri)
      real(kind = kreal), intent(in) :: a3r_ele_rj(nri)
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: fdm3e_d0_mat(nri,-2:1)
      real(kind = kreal), intent(in) :: fdm3e_d1_mat(nri,-2:1)
      real(kind = kreal), intent(in) :: fdm3e_d3_mat(nri,-2:1)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: hdiv_visous_mat(-2:1,jmax,nri)
!
      integer(kind = kint) :: iele
      integer(kind = kint) :: k, j
      real(kind = kreal) :: c_d3, c_d1, c_d0
!
!
!$omp parallel do private(k,j,c_d3,c_d1,c_d0)
      do iele = ist, ied
        j = 1 + mod((iele-1), jmax)
        k = 1 + (iele - j) / jmax
!
        c_d3 = -one
        c_d1 =        g_sph_rj(j,3)*a2r_ele_rj(k)
        c_d0 = -two * g_sph_rj(j,3)*a3r_ele_rj(k)
!
        hdiv_visous_mat(-2:1,j,k) =  c_d3 * fdm3e_d3_mat(k,-2:1)        &
     &                             + c_d1 * fdm3e_d1_mat(k,-2:1)        &
     &                             + c_d0 * fdm3e_d0_mat(k,-2:1)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_hdiv_fixed_viscosity
!
! -----------------------------------------------------------------------
!
      subroutine add_sph_hdiv_viscous_rho_depend(ist, ied, nri, jmax,   &
     &          a1r_ele_rj, a2r_ele_rj, g_sph_rj, h_rho, h_drhodr,      &
     &          fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat,               &
     &          hdiv_visous_mat)
!
      integer(kind = kint), intent(in) :: ist, ied
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: a1r_ele_rj(nri)
      real(kind = kreal), intent(in) :: a2r_ele_rj(nri)
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: h_rho(nri), h_drhodr(nri)
      real(kind = kreal), intent(in) :: fdm3e_d0_mat(nri,-2:1)
      real(kind = kreal), intent(in) :: fdm3e_d1_mat(nri,-2:1)
      real(kind = kreal), intent(in) :: fdm3e_d2_mat(nri,-2:1)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: hdiv_visous_mat(-2:1,jmax,nri)
!
      integer(kind = kint) :: iele, k, j
      real(kind = kreal) :: c_d2, c_d1, c_d0
!
!
!$omp parallel do private(iele,k,j,c_d2,c_d1,c_d0)
      do iele = ist, ied
        j = 1 + mod((iele-1), jmax)
        k = 1 + (iele - j) / jmax
!
        c_d2 = h_rho(k)
        c_d1 = two * a1r_ele_rj(k) * h_rho(k) + h_drhodr(k)
        c_d0 = - g_sph_rj(j,3)*a2r_ele_rj(k)                            &
     &          * h_rho(k) * two / three
        hdiv_visous_mat(-2:1,j,k) = hdiv_visous_mat(-2:1,j,k)           &
     &                             + c_d2 * fdm3e_d2_mat(k,-2:1)        &
     &                             + c_d1 * fdm3e_d1_mat(k,-2:1)        &
     &                             + c_d0 * fdm3e_d0_mat(k,-2:1)
      end do
!$omp end parallel do
!
      end subroutine add_sph_hdiv_viscous_rho_depend
!
! -----------------------------------------------------------------------
!
      subroutine add_sph_hdiv_viscous_rho_nu_dep(ist, ied, nri, jmax,   &
     &          h_nu, h_rho, fdm3e_d1_mat, hdiv_visous_mat)
!
      integer(kind = kint), intent(in) :: ist, ied
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: h_nu(nri)
      real(kind = kreal), intent(in) :: h_rho(nri)
      real(kind = kreal), intent(in) :: fdm3e_d1_mat(nri,-2:1)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: hdiv_visous_mat(-2:1,jmax,nri)
!
      integer(kind = kint) :: iele, k, j
      real(kind = kreal) :: c_d1
!
!
!$omp parallel do private(iele,k,j,c_d1)
      do iele = ist, ied
        j = 1 + mod((iele-1), jmax)
        k = 1 + (iele - j) / jmax
!
        c_d1 = h_nu(k) * h_rho(k)
        hdiv_visous_mat(-2:1,j,k) = hdiv_visous_mat(-2:1,j,k)           &
     &                             + c_d1 * fdm3e_d1_mat(k,-2:1)
      end do
!$omp end parallel do
!
      end subroutine add_sph_hdiv_viscous_rho_nu_dep
!
! -----------------------------------------------------------------------
!
      subroutine add_sph_hdiv_viscous_nu_depend(ist, ied, nri, jmax,    &
     &          a1r_ele_rj, a2r_ele_rj, g_sph_rj, relative_d, h_nu,     &
     &          fdm3e_d0_mat, fdm3e_d1_mat, fdm3e_d2_mat,               &
     &          hdiv_visous_mat)
!
      integer(kind = kint), intent(in) :: ist, ied
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: a1r_ele_rj(nri)
      real(kind = kreal), intent(in) :: a2r_ele_rj(nri)
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: relative_d(nri), h_nu(nri)
      real(kind = kreal), intent(in) :: fdm3e_d0_mat(nri,-2:1)
      real(kind = kreal), intent(in) :: fdm3e_d1_mat(nri,-2:1)
      real(kind = kreal), intent(in) :: fdm3e_d2_mat(nri,-2:1)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: hdiv_visous_mat(-2:1,jmax,nri)
!
      integer(kind = kint) :: iele
      integer(kind = kint) :: k, j
      real(kind = kreal) :: c_d2, c_d1, c_d0
!
!
!$omp parallel do private(k,j,c_d2,c_d1,c_d0)
      do iele = ist, ied
        j = 1 + mod((iele-1), jmax)
        k = 1 + (iele - j) / jmax
!
        c_d2 = - h_nu(k)
        c_d1 = two * a1r_ele_rj(k) * h_nu(k)
        c_d0 = - g_sph_rj(j,3)*a2r_ele_rj(k) * h_nu(k)
        hdiv_visous_mat(-2:1,j,k) = hdiv_visous_mat(-2:1,j,k)           &
     &                             + c_d2 * fdm3e_d2_mat(k,-2:1)        &
     &                             + c_d1 * fdm3e_d1_mat(k,-2:1)        &
     &                             + c_d0 * fdm3e_d0_mat(k,-2:1)
        hdiv_visous_mat(-2:1,j,k) = relative_d(k)                       &
     &                             * hdiv_visous_mat(-2:1,j,k)
      end do
!$omp end parallel do
!
      end subroutine add_sph_hdiv_viscous_nu_depend
!
! -----------------------------------------------------------------------
!
      end module cal_sph_FDM3e_hdiv_viscous

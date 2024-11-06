!>@file   cal_sph_FDM_viscosity_mat.f90
!!@brief  module cal_sph_FDM_viscosity_mat
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2020
!
!>@brief  Forth order FDM on nodes
!!
!!@verbatim
!!      subroutine sph_FDM_viscosity_mat(n_next, kr,                    &
!!     &         (flag_viscous_variation, flag_ref_density_valiation,   &
!!     &          nri, jmax, a1_radius, a2_radius, g_sph_rj,            &
!!     &          coef_d, relative_d, h_nu, h_rho, h_drhodr,            &
!!     &          fdm_d1_mat, fdm_d2_mat, mat_viscous)
!!        logical, intent(in) :: flag_viscous_variation
!!        logical, intent(in) :: flag_ref_density_valiation
!!        integer(kind = kint), intent(in) :: n_next
!!        integer(kind = kint), intent(in) :: kr
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: a1_radius(nri)
!!        real(kind = kreal), intent(in) :: a2_radius(nri)
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
!!        real(kind = kreal), intent(in) :: coef_p, coef_d
!!        real(kind = kreal), intent(in) :: relative_d(nri), h_nu(nri)
!!        real(kind = kreal), intent(in) :: h_rho(nri), h_drhodr(nri)
!!        real(kind=kreal), intent(in) :: fdm_d1_mat(nri,-n_next:n_next)
!!        real(kind=kreal), intent(in) :: fdm_d2_mat(nri,-n_next:n_next)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: mat_viscous(jmax,-n_next:n_next)
!!
!!      subroutine set_FDM_pressure_grad_mat(n_next, kr, nri, coef_p,   &
!!     &                                     fdm_e2n_d1_mat, mat_grad_p)
!!        integer(kind = kint), intent(in) :: n_next
!!        integer(kind = kint), intent(in) :: kr
!!        integer(kind = kint), intent(in) :: nri
!!        real(kind = kreal), intent(in) :: coef_p
!!        real(kind = kreal), intent(in)                                &
!!     &                    :: fdm_e2n_d1_mat(nri,-n_next+1:n_next)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: mat_grad_p(-n_next+1:n_next)
!!@endverbatim
!!
      module cal_sph_FDM_viscosity_mat
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: set_FDM_fixed_viscosity
      private :: add_FDM_viscosity_rho_depend
      private :: add_FDM_viscosity_rho_nu_dep
      private :: add_FDM_viscosity_nu_depend
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine sph_FDM_viscosity_mat(n_next, kr,                      &
     &          flag_viscous_variation, flag_ref_density_valiation,     &
     &          nri, jmax, a1_radius, a2_radius, g_sph_rj,              &
     &          coef_d, relative_d, h_nu, h_rho, h_drhodr,              &
     &          fdm_d1_mat, fdm_d2_mat, mat_viscous)
!
      logical, intent(in) :: flag_viscous_variation
      logical, intent(in) :: flag_ref_density_valiation
      integer(kind = kint), intent(in) :: n_next
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: a1_radius(nri)
      real(kind = kreal), intent(in) :: a2_radius(nri)
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(nri), h_nu(nri)
      real(kind = kreal), intent(in) :: h_rho(nri), h_drhodr(nri)
      real(kind = kreal), intent(in) :: fdm_d1_mat(nri,-n_next:n_next)
      real(kind = kreal), intent(in) :: fdm_d2_mat(nri,-n_next:n_next)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat_viscous(jmax,-n_next:n_next)
!
!
      call set_FDM_fixed_viscosity(n_next, kr, nri, jmax,               &
     &    a2_radius(kr), g_sph_rj, fdm_d2_mat, mat_viscous)
!
      if(flag_ref_density_valiation) then
        call add_FDM_viscosity_rho_depend(n_next, kr, nri, jmax,        &
     &      a1_radius(kr), h_rho(kr), h_drhodr(kr), fdm_d1_mat,         &
     &      mat_viscous)
        if(flag_viscous_variation) then
          call add_FDM_viscosity_rho_nu_dep                             &
     &       (jmax, h_nu(kr), h_rho(kr), mat_viscous(1,0))
        end if
      end if
!
      if(flag_viscous_variation) then
        call add_FDM_viscosity_nu_depend(n_next, kr, nri, jmax,         &
     &      a1_radius(kr), relative_d(kr), h_nu(kr), fdm_d1_mat,        &
     &      mat_viscous)
      end if
!
!$omp parallel workshare
      mat_viscous(1:jmax,-n_next:n_next)                                &
     &       = coef_d * mat_viscous(1:jmax,-n_next:n_next)
!$omp end parallel workshare
!
      end subroutine sph_FDM_viscosity_mat
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine set_FDM_pressure_grad_mat(n_next, kr, nri, coef_p,     &
     &                                     fdm_e2n_d1_mat, mat_grad_p)
!
      integer(kind = kint), intent(in) :: n_next
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in)                                    &
     &                    :: fdm_e2n_d1_mat(nri,-n_next+1:n_next)
!
      real(kind = kreal), intent(inout) :: mat_grad_p(-n_next+1:n_next)
!
!
      mat_grad_p(-n_next+1:n_next)                                      &
     &      = coef_p * fdm_e2n_d1_mat(kr,-n_next+1:n_next)
!
      end subroutine set_FDM_pressure_grad_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_FDM_fixed_viscosity(n_next, kr, nri, jmax,         &
     &          a2_radius, g_sph_rj, fdm_d2_mat, mat_viscous)
!
      integer(kind = kint), intent(in) :: n_next
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: a2_radius
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: fdm_d2_mat(nri,-n_next:n_next)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat_viscous(jmax,-n_next:n_next)
!
      integer(kind = kint) :: i_next
!
!
      do i_next = -n_next, n_next
        mat_viscous(1:jmax,i_next) = fdm_d2_mat(kr,i_next)
      end do
      mat_viscous(1:jmax,0) = mat_viscous(1:jmax,0)                     &
     &                       - g_sph_rj(1:jmax,3) * a2_radius
!
      end subroutine set_FDM_fixed_viscosity
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine add_FDM_viscosity_rho_depend(n_next, kr,               &
     &          nri, jmax, a1_radius, h_rho, h_drhodr,                  &
     &          fdm_d1_mat, mat_viscous)
!
      integer(kind = kint), intent(in) :: n_next
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: a1_radius
      real(kind = kreal), intent(in) :: h_rho, h_drhodr
      real(kind = kreal), intent(in) :: fdm_d1_mat(nri,-n_next:n_next)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat_viscous(jmax,-n_next:n_next)
!
      integer(kind = kint) :: i_next
      real(kind = kreal) :: c_d1, c_d0
      real(kind = kreal) :: mat_tmp(-n_next:n_next)
!
!
      c_d0 = -(four / three) * (h_rho*a1_radius + h_drhodr)
      c_d1 = - h_rho / three
      mat_tmp(-n_next:n_next) = c_d1 * fdm_d1_mat(kr,-n_next:n_next)
      mat_tmp(0) = mat_tmp(0) + c_d0
!
      do i_next = -n_next, n_next
        mat_viscous(1:jmax,i_next)                                      &
     &     = mat_viscous(1:jmax,i_next) + mat_tmp(i_next)
      end do
!
      end subroutine add_FDM_viscosity_rho_depend
!
! -----------------------------------------------------------------------
!
      subroutine add_FDM_viscosity_rho_nu_dep(jmax, h_nu, h_rho,        &
     &                                        mat_viscous_diag)
!
      integer(kind = kint), intent(in) :: jmax
      real(kind = kreal), intent(in) :: h_nu, h_rho
!
      real(kind = kreal), intent(inout) :: mat_viscous_diag(jmax)
!
      real(kind = kreal) :: c_d0
!
!
      c_d0 = - (four / three) * h_rho * h_nu
      mat_viscous_diag(1:jmax) = mat_viscous_diag(1:jmax) + c_d0
!
      end subroutine add_FDM_viscosity_rho_nu_dep
!
! -----------------------------------------------------------------------
!
      subroutine add_FDM_viscosity_nu_depend                            &
     &         (n_next, kr, nri, jmax, a1_radius,                       &
     &          relative_d, h_nu, fdm_d1_mat, mat_viscous)
!
      integer(kind = kint), intent(in) :: n_next
      integer(kind = kint), intent(in) :: kr
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: a1_radius
      real(kind = kreal), intent(in) :: relative_d, h_nu
      real(kind = kreal), intent(in) :: fdm_d1_mat(nri,-n_next:n_next)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat_viscous(jmax,-n_next:n_next)
!
      integer(kind = kint) :: i_next
      real(kind = kreal) :: c_d1, c_d0
      real(kind = kreal) :: mat_tmp(-n_next:n_next)
!
!
      c_d0 = - four * h_nu * a1_radius
      c_d1 =   two * h_nu
      mat_tmp(-n_next:n_next) = c_d1 * fdm_d1_mat(kr,-n_next:n_next)
      mat_tmp(0) =              mat_tmp(0) + c_d0
!
      do i_next = -n_next, n_next
        mat_viscous(1:jmax,i_next)                                      &
     &                = mat_viscous(1:jmax,i_next) + mat_tmp(i_next)
        mat_viscous(1:jmax,i_next)                                      &
     &                = relative_d * mat_viscous(1:jmax,i_next)
      end do
!
      end subroutine add_FDM_viscosity_nu_depend
!
! -----------------------------------------------------------------------
!
      end module cal_sph_FDM_viscosity_mat

!>@file   cal_whole_sph_FDM_viscosity.f90
!!@brief  module cal_whole_sph_FDM_viscosity
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2020
!
!>@brief  Forth order FDM on nodes
!!
!!@verbatim
!!      subroutine sph_FDM_viscosity_mat(n_next, kr_st, kr_ed,          &
!!     &         (flag_viscous_variation, flag_ref_density_valiation,   &
!!     &          nri, jmax, a1_radius, a2_radius, g_sph_rj,            &
!!     &          coef_d, relative_d, h_nu, h_rho, h_drhodr,            &
!!     &          fdm_d1_mat, fdm_d2_mat, mat_viscous)
!!        logical, intent(in) :: flag_viscous_variation
!!        logical, intent(in) :: flag_ref_density_valiation
!!        integer(kind = kint), intent(in) :: n_next
!!        integer(kind = kint), intent(in) :: kr_st, kr_ed
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
!!     &           :: mat_viscous(-n_next:n_next,jmax,nri)
!!        real(kind=kreal), intent(in)                                  &
!!     &                   :: fdm_e2n_d1_mat(nri,-n_next+1:n_next)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: mat_grad_p(-n_next+1:n_next,nri)
!!@endverbatim
!!
      module cal_whole_sph_FDM_viscosity
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
      subroutine sph_FDM_viscosity_mat(n_next, kr_st, kr_ed,            &
     &          flag_viscous_variation, flag_ref_density_valiation,     &
     &          nri, jmax, a1_radius, a2_radius, g_sph_rj,              &
     &          coef_d, relative_d, h_nu, h_rho, h_drhodr,              &
     &          fdm_d1_mat, fdm_d2_mat, mat_viscous)
!
      logical, intent(in) :: flag_viscous_variation
      logical, intent(in) :: flag_ref_density_valiation
      integer(kind = kint), intent(in) :: n_next
      integer(kind = kint), intent(in) :: kr_st, kr_ed
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
     &           :: mat_viscous(-n_next:n_next,jmax,nri)
!
      integer(kind = kint) :: ist, ied
!
!
      ist =  1 + (kr_st - 1) * jmax
      ied =  kr_ed * jmax
!
      call set_FDM_fixed_viscosity(n_next, ist, ied, nri, jmax,         &
     &    a2_radius, g_sph_rj, fdm_d2_mat, mat_viscous)
!
      if(flag_ref_density_valiation) then
        call add_FDM_viscosity_rho_depend(n_next, ist, ied, nri, jmax,  &
     &      a1_radius, h_rho, h_drhodr, fdm_d1_mat, mat_viscous)
        if(flag_viscous_variation) then
          call add_FDM_viscosity_rho_nu_dep(n_next, ist, ied,           &
     &        nri, jmax, h_nu, h_rho, mat_viscous)
        end if
      end if
!
      if(flag_viscous_variation) then
        call add_FDM_viscosity_nu_depend(n_next, ist, ied, nri,         &
     &      jmax, a1_radius, relative_d, h_nu, fdm_d1_mat, mat_viscous)
      end if
!
!$omp parallel workshare
      mat_viscous(-n_next:n_next,1:jmax,kr_st:kr_ed)                    &
     &       = coef_d * mat_viscous(-n_next:n_next,1:jmax,kr_st:kr_ed)
!$omp end parallel workshare
!
      end subroutine sph_FDM_viscosity_mat
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine set_FDM_pressure_grad_mat(n_next, kr_st, kr_ed, nri,   &
     &          coef_p, fdm_e2n_d1_mat, mat_grad_p)
!
      integer(kind = kint), intent(in) :: n_next
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: nri
      real(kind = kreal), intent(in) :: coef_p
      real(kind = kreal), intent(in)                                    &
     &                    :: fdm_e2n_d1_mat(nri,-n_next+1:n_next)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat_grad_p(-n_next+1:n_next,nri)
!
!
!$omp parallel workshare
        mat_grad_p(-n_next+1:n_next,kr_st:kr_ed)                        &
     &      = coef_p * fdm_e2n_d1_mat(kr_st:kr_ed,-n_next+1:n_next)
!$omp end parallel workshare
!
      end subroutine set_FDM_pressure_grad_mat
!
! -----------------------------------------------------------------------
!
      subroutine set_FDM_fixed_viscosity(n_next, ist, ied, nri, jmax,   &
     &          a2_radius, g_sph_rj, fdm_d2_mat, mat_viscous)
!
      integer(kind = kint), intent(in) :: n_next
      integer(kind = kint), intent(in) :: ist, ied
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: a2_radius(nri)
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: fdm_d2_mat(nri,-n_next:n_next)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat_viscous(-n_next:n_next,jmax,nri)
!
      integer(kind = kint) :: inod, k, j
      real(kind = kreal) :: c_d0
!
!
!$omp parallel do private(inod,k,j,c_d0)
      do inod = ist, ied
        j = 1 + mod((inod-1), jmax)
        k = 1 + (inod - j) / jmax
        mat_viscous(-n_next:n_next,j,k) = fdm_d2_mat(k,-n_next:n_next)
!
        c_d0 = - g_sph_rj(j,3) * a2_radius(k)
        mat_viscous(0,j,k) = mat_viscous(0,j,k) + c_d0
      end do
!$omp end parallel do
!
      end subroutine set_FDM_fixed_viscosity
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine add_FDM_viscosity_rho_depend(n_next,ist, ied,          &
     &          nri, jmax, a1_radius, h_rho, h_drhodr,                  &
     &          fdm_d1_mat, mat_viscous)
!
      integer(kind = kint), intent(in) :: n_next
      integer(kind = kint), intent(in) :: ist, ied
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: a1_radius(nri)
      real(kind = kreal), intent(in) :: h_rho(nri), h_drhodr(nri)
      real(kind = kreal), intent(in) :: fdm_d1_mat(nri,-n_next:n_next)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat_viscous(-n_next:n_next,jmax,nri)
!
      integer(kind = kint) :: inod, k, j
      real(kind = kreal) :: c_d1, c_d0
!
!
!$omp parallel do private(inod,k,j,c_d1,c_d0)
      do inod = ist, ied
        j = 1 + mod((inod-1), jmax)
        k = 1 + (inod - j) / jmax
!
        c_d0 = -(four / three) * (h_rho(k)*a1_radius(k) + h_drhodr(k))
        mat_viscous(0,j,k) = mat_viscous(0,j,k) + c_d0
!
        c_d1 = - h_rho(k) / three
        mat_viscous(-n_next:n_next,j,k)                                 &
     &                           = mat_viscous(-n_next:n_next,j,k)      &
     &                            + c_d1 * fdm_d1_mat(k,-n_next:n_next)
      end do
!$omp end parallel do
!
      end subroutine add_FDM_viscosity_rho_depend
!
! -----------------------------------------------------------------------
!
      subroutine add_FDM_viscosity_rho_nu_dep(n_next, ist, ied,         &
     &          nri, jmax, h_nu, h_rho, mat_viscous)
!
      integer(kind = kint), intent(in) :: n_next
      integer(kind = kint), intent(in) :: ist, ied
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: h_nu(nri)
      real(kind = kreal), intent(in) :: h_rho(nri)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat_viscous(-n_next:n_next,jmax,nri)
!
      integer(kind = kint) :: inod, k, j
      real(kind = kreal) :: c_d0
!
!
!$omp parallel do private(inod,k,j,c_d0)
      do inod = ist, ied
        j = 1 + mod((inod-1), jmax)
        k = 1 + (inod - j) / jmax
!
        c_d0 = - (four / three) * h_rho(k) * h_nu(k)
        mat_viscous(0,j,k) = mat_viscous(0,j,k) + c_d0
      end do
!$omp end parallel do
!
      end subroutine add_FDM_viscosity_rho_nu_dep
!
! -----------------------------------------------------------------------
!
      subroutine add_FDM_viscosity_nu_depend                            &
     &         (n_next, ist, ied, nri, jmax, a1_radius,                 &
     &          relative_d, h_nu, fdm_d1_mat, mat_viscous)
!
      integer(kind = kint), intent(in) :: n_next
      integer(kind = kint), intent(in) :: ist, ied
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: a1_radius(nri)
      real(kind = kreal), intent(in) :: relative_d(nri), h_nu(nri)
      real(kind = kreal), intent(in) :: fdm_d1_mat(nri,-n_next:n_next)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat_viscous(-n_next:n_next,jmax,nri)
!
      integer(kind = kint) :: inod, k, j
      real(kind = kreal) :: c_d1, c_d0
!
!
!$omp parallel do private(inod,k,j,c_d1,c_d0)
      do inod = ist, ied
        j = 1 + mod((inod-1), jmax)
        k = 1 + (inod - j) / jmax
!
        c_d0 = - four * h_nu(k) * a1_radius(k)
        mat_viscous(0,j,k) = mat_viscous(0,j,k) + c_d0
!
        c_d1 =   two * h_nu(k)
        mat_viscous(-n_next:n_next,j,k)                                 &
     &                           = mat_viscous(-n_next:n_next,j,k)      &
     &                            + c_d1 * fdm_d1_mat(k,-n_next:n_next)
!
        mat_viscous(-n_next:n_next,j,k)                                 &
     &                = relative_d(k) * mat_viscous(-n_next:n_next,j,k)
      end do
!$omp end parallel do
!
      end subroutine add_FDM_viscosity_nu_depend
!
! -----------------------------------------------------------------------
!
      end module cal_whole_sph_FDM_viscosity

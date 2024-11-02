!>@file   cal_each_sph_FDM_viscosity.f90
!!@brief  module cal_each_sph_FDM_viscosity
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2020
!
!>@brief  Forth order FDM on nodes
!!
!!@verbatim
!!      subroutine sph_FDM_each_viscosity                               &
!!     &         (flag_viscous_variation, flag_ref_density_valiation,   &
!!     &          n_next, jmax, a_radius, a2_radius, g_sph_rj,          &
!!     &          coef_d, relative_d, h_nu, h_rho, h_drhodr,            &
!!     &          fdm_d0_mat, fdm_d1_mat, fdm_d2_mat,                   &
!!     &          fdm_e2n_d1_mat, mat_viscous, mat_grad_p)
!!        real(kind = kreal), intent(in) :: a_radius
!!        real(kind = kreal), intent(in) :: a2_radius
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
!!        real(kind = kreal), intent(in) :: h_nu, h_rho, h_drhodr
!!        real(kind = kreal), intent(in) :: fdm_d0_mat(-n_next:n_next)
!!        real(kind = kreal), intent(in) :: fdm_d1_mat(-n_next:n_next)
!!        real(kind = kreal), intent(in) :: fdm_d2_mat(-n_next:n_next)
!!        real(kind = kreal), intent(in)                                &
!!       &                    :: fdm_e2n_d1_mat(-n_next+1:n_next)
!!        real(kind = kreal), intent(inout)                             &
!!       &           :: mat_viscous(-n_next:n_next,jmax)
!!        real(kind = kreal), intent(inout)                             &
!!       &             :: mat_grad_p(-n_next+1:n_next)
!!@endverbatim
!!
      module cal_each_sph_FDM_viscosity
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
      subroutine sph_FDM_each_viscosity                                 &
     &         (flag_viscous_variation, flag_ref_density_valiation,     &
     &          n_next, jmax, a_radius, a2_radius, g_sph_rj,            &
     &          coef_d, relative_d, h_nu, h_rho, h_drhodr,              &
     &          fdm_d0_mat, fdm_d1_mat, fdm_d2_mat,                     &
     &          fdm_e2n_d1_mat, mat_viscous, mat_grad_p)
!
      logical, intent(in) :: flag_viscous_variation
      logical, intent(in) :: flag_ref_density_valiation
      integer(kind = kint), intent(in) :: n_next, jmax
      real(kind = kreal), intent(in) :: a_radius, a2_radius
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: coef_d, relative_d
      real(kind = kreal), intent(in) :: h_nu, h_rho, h_drhodr
      real(kind = kreal), intent(in) :: fdm_d0_mat(-n_next:n_next)
      real(kind = kreal), intent(in) :: fdm_d1_mat(-n_next:n_next)
      real(kind = kreal), intent(in) :: fdm_d2_mat(-n_next:n_next)
      real(kind = kreal), intent(in)                                    &
     &                   :: fdm_e2n_d1_mat(-n_next+1:n_next)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat_viscous(-n_next:n_next,jmax)
      real(kind = kreal), intent(inout)                                 &
     &           :: mat_grad_p(-n_next+1:n_next)
!
!
      call set_FDM_each_viscosity(n_next, jmax, a2_radius,              &
     &    g_sph_rj, fdm_d0_mat, fdm_d2_mat, fdm_e2n_d1_mat,             &
     &    mat_viscous, mat_grad_p)
!
      if(flag_viscous_variation .and. flag_ref_density_valiation) then
        call add_FDM_each_viscous_depend(n_next, jmax, a_radius,        &
     &      h_nu, fdm_d0_mat, fdm_d1_mat, mat_viscous)
        call add_FDM_each_viscous_rho_dep(n_next, jmax, a_radius,       &
     &      h_nu, h_rho, h_drhodr, fdm_d0_mat, fdm_d1_mat,              &
     &      mat_viscous)
      else if(flag_viscous_variation) then
        call add_FDM_each_viscous_depend(n_next, jmax, a_radius,        &
     &      h_nu, fdm_d0_mat, fdm_d1_mat, mat_viscous)
      else if(flag_ref_density_valiation) then
        call add_FDM_each_viscous_rho_dep(n_next, jmax, a_radius,       &
     &      h_nu, h_rho, h_drhodr, fdm_d0_mat, fdm_d1_mat,              &
     &      mat_viscous)
      else
      end if
!
      if(flag_viscous_variation .or. flag_ref_density_valiation) then
!$omp parallel workshare
        mat_viscous(-n_next:n_next,1:jmax)                              &
     &       = coef_d * relative_d * mat_viscous(-n_next:n_next,1:jmax)
!$omp end parallel workshare
      else
!$omp parallel workshare
        mat_viscous(-n_next:n_next,1:jmax)                              &
     &       = coef_d * mat_viscous(-n_next:n_next,1:jmax)
!$omp end parallel workshare
      end if
!
      end subroutine sph_FDM_each_viscosity
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine set_FDM_each_viscosity(n_next, jmax, a2_radius,        &
     &          g_sph_rj, fdm_d0_mat, fdm_d2_mat, fdm_e2n_d1_mat,       &
     &          mat_viscous, mat_grad_p)
!
      integer(kind = kint), intent(in) :: n_next, jmax
      real(kind = kreal), intent(in) :: a2_radius
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: fdm_d0_mat(-n_next:n_next)
      real(kind = kreal), intent(in) :: fdm_d2_mat(-n_next:n_next)
      real(kind = kreal), intent(in)                                    &
     &                    :: fdm_e2n_d1_mat(-n_next+1:n_next)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat_viscous(-n_next:n_next,jmax)
      real(kind = kreal), intent(inout)                                 &
     &           :: mat_grad_p(-n_next+1:n_next)
!
      integer(kind = kint) :: j
      real(kind = kreal) :: c_d2, c_d0
!
!
      c_d2 =  one
!$omp parallel do private(j,c_d0)
      do j = 1, jmax
        c_d0 = - g_sph_rj(j,3) * a2_radius
        mat_viscous(-n_next:n_next,j)                                   &
     &                              = c_d2 * fdm_d2_mat(-n_next:n_next) &
     &                              + c_d0 * fdm_d0_mat(-n_next:n_next)
      end do
!$omp end parallel do
!
      mat_grad_p(-n_next+1:n_next) = fdm_e2n_d1_mat(-n_next+1:n_next)
!
      end subroutine set_FDM_each_viscosity
!
! -----------------------------------------------------------------------
!
      subroutine add_FDM_each_viscous_depend(n_next, jmax, a_radius,    &
     &          h_nu, fdm_d0_mat, fdm_d1_mat, mat_viscous)
!
      integer(kind = kint), intent(in) :: n_next, jmax
      real(kind = kreal), intent(in) :: a_radius
      real(kind = kreal), intent(in) :: h_nu
      real(kind = kreal), intent(in) :: fdm_d0_mat(-n_next:n_next)
      real(kind = kreal), intent(in) :: fdm_d1_mat(-n_next:n_next)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat_viscous(-n_next:n_next,jmax)
!
      integer(kind = kint) :: j
      real(kind = kreal) :: c_d1, c_d0
!
!
      c_d1 = two * h_nu
      c_d0 = - four * h_nu * a_radius
!$omp parallel do private(j)
      do j = 1, jmax
        mat_viscous(-n_next:n_next,j) = mat_viscous(-n_next:n_next,j)   &
     &                              + c_d1 * fdm_d1_mat(-n_next:n_next) &
     &                              + c_d0 * fdm_d0_mat(-n_next:n_next)
      end do
!$omp end parallel do
!
      end subroutine add_FDM_each_viscous_depend
!
! -----------------------------------------------------------------------
!
      subroutine add_FDM_each_viscous_rho_dep(n_next, jmax, a_radius,   &
     &          h_nu, h_rho, h_drhodr, fdm_d0_mat, fdm_d1_mat,          &
     &          mat_viscous)
!
      integer(kind = kint), intent(in) :: n_next, jmax
      real(kind = kreal), intent(in) :: a_radius
      real(kind = kreal), intent(in) :: h_nu, h_rho, h_drhodr
      real(kind = kreal), intent(in) :: fdm_d0_mat(-n_next:n_next)
      real(kind = kreal), intent(in) :: fdm_d1_mat(-n_next:n_next)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat_viscous(-n_next:n_next,jmax)
!
      integer(kind = kint) :: j
      real(kind = kreal) :: c_d1, c_d0
!
!
      c_d1 = - h_rho / three
      c_d0 = - (four / three) * (h_rho * a_radius                       &
     &                         + h_rho * h_nu + h_drhodr)
!$omp parallel do private(j)
      do j = 1, jmax
        mat_viscous(-n_next:n_next,j) = mat_viscous(-n_next:n_next,j)   &
     &                              + c_d1 * fdm_d1_mat(-n_next:n_next) &
     &                              + c_d0 * fdm_d0_mat(-n_next:n_next)
      end do
!$omp end parallel do
!
      end subroutine add_FDM_each_viscous_rho_dep
!
! -----------------------------------------------------------------------
!
      end module cal_each_sph_FDM_viscosity

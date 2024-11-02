!>@file   cal_whole_sph_FDM_viscosity.f90
!!@brief  module cal_whole_sph_FDM_viscosity
!!
!!@author H. Matsui
!!@date Programmed in Jan, 2020
!
!>@brief  Forth order FDM on nodes
!!
!!@verbatim
!!      subroutine sph_FDM_whole_viscosity                              &
!!     &         (flag_viscous_variation, flag_ref_density_valiation,   &
!!     &          n_next, kr_st, kr_ed, nri, jmax, a_radius, a2_radius, &
!!     &          g_sph_rj, coef_d, relative_d, h_nu, h_rho, h_drhodr,  &
!!     &          fdm_d0_mat, fdm_d1_mat, fdm_d2_mat,                   &
!!     &          fdm_e2n_d1_mat, mat_viscous, mat_grad_p)
!!        integer(kind = kint), intent(in) :: n_next
!!        integer(kind = kint), intent(in) :: kr_st, kr_ed
!!        integer(kind = kint), intent(in) :: nri, jmax
!!        real(kind = kreal), intent(in) :: a_radius(nri)
!!        real(kind = kreal), intent(in) :: a2_radius(nri)
!!        real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
!!        real(kind = kreal), intent(in) :: h_nu(nri)
!!        real(kind = kreal), intent(in) :: h_rho(nri), h_drhodr(nri)
!!        real(kind = kreal), intent(in):: fdm_d0_mat(-n_next:n_next,nri)
!!        real(kind = kreal), intent(in):: fdm_d1_mat(-n_next:n_next,nri)
!!        real(kind = kreal), intent(in):: fdm_d2_mat(-n_next:n_next,nri)
!!        real(kind = kreal), intent(in)                                &
!!     &                    :: fdm_e2n_d1_mat(-n_next+1:n_next,nri)
!!        real(kind = kreal), intent(inout)                             &
!!     &           :: mat_viscous(-n_next:n_next,jmax,nri)
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
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine sph_FDM_whole_viscosity                                &
     &         (flag_viscous_variation, flag_ref_density_valiation,     &
     &          n_next, kr_st, kr_ed, nri, jmax, a_radius, a2_radius,   &
     &          g_sph_rj, coef_d, relative_d, h_nu, h_rho, h_drhodr,    &
     &          fdm_d0_mat, fdm_d1_mat, fdm_d2_mat,                     &
     &          fdm_e2n_d1_mat, mat_viscous, mat_grad_p)
!
      logical, intent(in) :: flag_viscous_variation
      logical, intent(in) :: flag_ref_density_valiation
      integer(kind = kint), intent(in) :: n_next
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: a_radius(nri), a2_radius(nri)
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: coef_d
      real(kind = kreal), intent(in) :: relative_d(nri), h_nu(nri)
      real(kind = kreal), intent(in) :: h_rho(nri), h_drhodr(nri)
      real(kind = kreal), intent(in) :: fdm_d0_mat(-n_next:n_next, nri)
      real(kind = kreal), intent(in) :: fdm_d1_mat(-n_next:n_next, nri)
      real(kind = kreal), intent(in) :: fdm_d2_mat(-n_next:n_next, nri)
      real(kind = kreal), intent(in)                                    &
     &                   :: fdm_e2n_d1_mat(-n_next+1:n_next,nri)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat_viscous(-n_next:n_next,jmax,nri)
      real(kind = kreal), intent(inout)                                 &
     &           :: mat_grad_p(-n_next+1:n_next,nri)
!
      integer(kind = kint) :: k
!
!
      call set_FDM_whole_viscosity                                      &
     &   (n_next, kr_st, kr_ed, nri, jmax, a2_radius,                   &
     &    g_sph_rj, fdm_d0_mat, fdm_d2_mat, fdm_e2n_d1_mat,             &
     &    mat_viscous, mat_grad_p)
!
      if(flag_viscous_variation .and. flag_ref_density_valiation) then
        call add_FDM_whole_viscous_depend                               &
     &     (n_next, kr_st, kr_ed, nri, jmax, a_radius,                  &
     &      h_nu, fdm_d0_mat, fdm_d1_mat, mat_viscous)
        call add_FDM_whole_viscous_rho_dep                              &
     &     (n_next, kr_st, kr_ed, nri, jmax, a_radius,                  &
     &      h_nu, h_rho, h_drhodr, fdm_d0_mat, fdm_d1_mat,              &
     &      mat_viscous)
      else if(flag_viscous_variation) then
        call add_FDM_whole_viscous_depend                               &
     &     (n_next, kr_st, kr_ed, nri, jmax, a_radius,                  &
     &      h_nu, fdm_d0_mat, fdm_d1_mat, mat_viscous)
      else if(flag_ref_density_valiation) then
        call add_FDM_whole_viscous_rho_dep                              &
     &     (n_next, kr_st, kr_ed, nri, jmax, a_radius,                  &
     &      h_nu, h_rho, h_drhodr, fdm_d0_mat, fdm_d1_mat,              &
     &      mat_viscous)
      end if
!
      if(flag_viscous_variation .or. flag_ref_density_valiation) then
!$omp parallel do private(k)
        do k = kr_st, kr_ed
          mat_viscous(-n_next:n_next,1:jmax,k)                          &
     &       = relative_d(k) * mat_viscous(-n_next:n_next,1:jmax,k)
        end do
!$omp end parallel do
      end if
!
!$omp parallel do private(k)
      do k = kr_st, kr_ed
        mat_viscous(-n_next:n_next,1:jmax,k)                            &
     &              = coef_d * mat_viscous(-n_next:n_next,1:jmax,k)
      end do
!$omp end parallel do
!
      end subroutine sph_FDM_whole_viscosity
!
!  -------------------------------------------------------------------
!
      subroutine set_FDM_whole_viscosity(n_next, kr_st, kr_ed,          &
     &          nri, jmax, a2_radius, g_sph_rj,                         &
     &          fdm_d0_mat, fdm_d2_mat, fdm_e2n_d1_mat,                 &
     &          mat_viscous, mat_grad_p)
!
      integer(kind = kint), intent(in) :: n_next
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: a2_radius(nri)
      real(kind = kreal), intent(in) :: g_sph_rj(jmax,17)
      real(kind = kreal), intent(in) :: fdm_d0_mat(-n_next:n_next,nri)
      real(kind = kreal), intent(in) :: fdm_d2_mat(-n_next:n_next,nri)
      real(kind = kreal), intent(in)                                    &
     &                    :: fdm_e2n_d1_mat(-n_next+1:n_next,nri)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat_viscous(-n_next:n_next,jmax,nri)
      real(kind = kreal), intent(inout)                                 &
     &           :: mat_grad_p(-n_next+1:n_next,nri)
!
      integer(kind = kint) :: k, j
      real(kind = kreal) :: c_d2, c_d0
!
!
      c_d2 =  one
!$omp parallel do private(k,j,c_d0)
      do k = kr_st, kr_ed
        do j = 1, jmax
          c_d0 = - g_sph_rj(j,3) * a2_radius(k)
          mat_viscous(-n_next:n_next,j,k)                               &
     &                            = c_d2 * fdm_d2_mat(-n_next:n_next,k) &
     &                            + c_d0 * fdm_d0_mat(-n_next:n_next,k)
        end do
        mat_grad_p(-n_next+1:n_next,k)                                  &
     &                             = fdm_e2n_d1_mat(-n_next+1:n_next,k)
      end do
!$omp end parallel do
!
      end subroutine set_FDM_whole_viscosity
!
! -----------------------------------------------------------------------
!
      subroutine add_FDM_whole_viscous_depend(n_next, kr_st, kr_ed,     &
     &          nri, jmax, a_radius, h_nu, fdm_d0_mat, fdm_d1_mat,      &
     &          mat_viscous)
!
      integer(kind = kint), intent(in) :: n_next
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: a_radius(nri)
      real(kind = kreal), intent(in) :: h_nu(nri)
      real(kind = kreal), intent(in) :: fdm_d0_mat(-n_next:n_next,nri)
      real(kind = kreal), intent(in) :: fdm_d1_mat(-n_next:n_next,nri)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat_viscous(-n_next:n_next,jmax,nri)
!
      integer(kind = kint) :: k, j
      real(kind = kreal) :: c_d1, c_d0
!
!
!$omp parallel do private(k,j,c_d1,c_d0)
      do k = kr_st, kr_ed
        c_d1 =   two * h_nu(k)
        c_d0 = - four * h_nu(k) * a_radius(k)
        do j = 1, jmax
          mat_viscous(-n_next:n_next,j,k)                               &
     &                           = mat_viscous(-n_next:n_next,j,k)      &
     &                            + c_d1 * fdm_d1_mat(-n_next:n_next,k) &
     &                            + c_d0 * fdm_d0_mat(-n_next:n_next,k)
        end do
      end do
!$omp end parallel do
!
      end subroutine add_FDM_whole_viscous_depend
!
! -----------------------------------------------------------------------
!
      subroutine add_FDM_whole_viscous_rho_dep(n_next, kr_st, kr_ed,    &
     &          nri, jmax, a_radius, h_nu, h_rho, h_drhodr,             &
     &          fdm_d0_mat, fdm_d1_mat, mat_viscous)
!
      integer(kind = kint), intent(in) :: n_next
      integer(kind = kint), intent(in) :: kr_st, kr_ed
      integer(kind = kint), intent(in) :: nri, jmax
      real(kind = kreal), intent(in) :: a_radius(nri)
      real(kind = kreal), intent(in) :: h_nu(nri)
      real(kind = kreal), intent(in) :: h_rho(nri), h_drhodr(nri)
      real(kind = kreal), intent(in) :: fdm_d0_mat(-n_next:n_next,nri)
      real(kind = kreal), intent(in) :: fdm_d1_mat(-n_next:n_next,nri)
!
      real(kind = kreal), intent(inout)                                 &
     &           :: mat_viscous(-n_next:n_next,jmax,nri)
!
      integer(kind = kint) :: k, j
      real(kind = kreal) :: c_d1, c_d0
!
!
!$omp parallel do private(k,j,c_d1,c_d0)
      do k = kr_st, kr_ed
        c_d1 = - h_rho(k) / three
        c_d0 = - (four / three) * (h_rho(k) * a_radius(k)               &
     &                           + h_rho(k) * h_nu(k) + h_drhodr(k))
        do j = 1, jmax
          mat_viscous(-n_next:n_next,j,k)                               &
     &                           = mat_viscous(-n_next:n_next,j,k)      &
     &                            + c_d1 * fdm_d1_mat(-n_next:n_next,k) &
     &                            + c_d0 * fdm_d0_mat(-n_next:n_next,k)
        end do
      end do
!$omp end parallel do
!
      end subroutine add_FDM_whole_viscous_rho_dep
!
! -----------------------------------------------------------------------
!
      end module cal_whole_sph_FDM_viscosity

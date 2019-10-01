!>@file   set_scalar_boundary_sph.f90
!!@brief  module set_scalar_boundary_sph
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2009
!
!>@brief  Evaluate scalar fields at boundaries
!!@n     Adjust temperature and composition boundary conditions
!!       if perturbation is solved
!!
!!
!!@verbatim
!!      subroutine set_fixed_scalar_sph(n_point, jmax,                  &
!!     &          kr_bc_st, kr_bc_ed, is_fld, fixed_bc, S_CTR,          &
!!     &          ntot_phys_rj, d_rj)
!!
!!      subroutine adjust_in_fixed_flux_sph                             &
!!     &       (jmax, kr_in, r_ICB, fdm2_fix_dr_ICB, flux_ICB, coef_d,  &
!!     &        coef_imp, dt, is_fld, n_point, ntot_phys_rj, d_rj)
!!      subroutine adjust_out_fixed_flux_sph                            &
!!     &       (jmax, kr_out, r_CMB, fdm2_fix_dr_CMB, flux_CMB, coef_d, &
!!     &        coef_imp, dt, is_fld, n_point, ntot_phys_rj, d_rj)
!!
!!      subroutine poisson_in_fixed_flux_sph                            &
!!     &       (jmax, kr_in, r_ICB, fdm2_fix_dr_ICB, flux_ICB, is_fld,  &
!!     &        n_point, ntot_phys_rj, d_rj)
!!      subroutine poisson_out_fixed_flux_sph                           &
!!     &       (jmax, kr_out, r_CMB, fdm2_fix_dr_CMB, flux_CMB, is_fld, &
!!     &        n_point, ntot_phys_rj, d_rj)
!!@endverbatim
!!
!!@param  n_point  Number of points for spectrum data
!!@param  jmax        Number of modes for local spectrum
!!@param  kr_bc_st    Start radial address to set fixed field
!!@param  kr_bc_ed    End radial address to set fixed field
!!@param  fixed_bc(jmax)   Boundary condition spectrum
!!
!!@param kr_in        Radial ID for inner boundary
!!@param kr_out       Radial ID for outer boundary
!!@param r_ICB(0:2)   Radius at ICB
!!@param r_CMB(0:2)   Radius at CMB
!!@param fdm2_fix_dr_ICB(-1:1,3)
!!         Matrix to evaluate field at ICB with fixed radial derivative
!!@param fdm2_fix_dr_CMB(-1:1,3)
!!         Matrix to evaluate field at CMB with fixed radial derivative
!!
!!@param flux_ICB(jamx)  Spectrum of fixed flux at ICB
!!@param flux_CMB(jamx)  Spectrum of fixed flux at CMB
!!@param coef_imp   Coefficient for contribution of implicit term
!!@param coef_d     Coefficient for magnetic diffusion
!!@param is_fld     Input field address for d_rj
!!
!!@param ntot_phys_rj   Total number of components
!!@param d_rj           Spectrum data
!!
      module set_scalar_boundary_sph
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
      subroutine set_fixed_scalar_sph                                   &
     &         (jmax, inod_rj_center, idx_rj_degree_zero,               &
     &          kr_bc_st, kr_bc_ed, is_fld, fixed_bc, S_CTR,            &
     &          n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: is_fld
      integer(kind = kint), intent(in) :: inod_rj_center
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: jmax, kr_bc_st, kr_bc_ed
      real(kind = kreal), intent(in) :: fixed_bc(jmax)
      real(kind = kreal), intent(in) :: S_CTR
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: j, inod, k
!
!
!$omp parallel do private (k,j,inod)
      do k = kr_bc_st, kr_bc_ed
        do j = 1, jmax
          inod = j + (k-1) * jmax
          d_rj(inod,is_fld) = fixed_bc(j)
        end do
      end do
!$omp end parallel do
!
      if(inod_rj_center .eq. 0) return
      if(idx_rj_degree_zero .eq. 0) return
      if(kr_bc_st .ne. ione) return
!
      d_rj(inod_rj_center,is_fld) = S_CTR
!
      end subroutine set_fixed_scalar_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine adjust_in_fixed_flux_sph                               &
     &         (jmax, kr_in, r_ICB, fdm2_fix_dr_ICB, flux_ICB, coef_d,  &
     &          coef_imp, dt, is_fld, n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      integer(kind = kint), intent(in) :: is_fld
      real(kind = kreal), intent(in) :: coef_d, coef_imp, dt
      real(kind = kreal), intent(in) :: flux_ICB(jmax)
      real(kind = kreal), intent(in) :: r_ICB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
!
        d_rj(inod,is_fld) =  d_rj(inod,is_fld)                          &
     &                     + dt * coef_imp * coef_d                     &
     &                      * ( fdm2_fix_dr_ICB(-1,3)                   &
     &                       + two*r_ICB(1) ) * flux_ICB(j)
      end do
!$omp end parallel do
!
      end subroutine adjust_in_fixed_flux_sph
!
! -----------------------------------------------------------------------
!
      subroutine adjust_out_fixed_flux_sph                              &
     &         (jmax, kr_out, r_CMB, fdm2_fix_dr_CMB, flux_CMB, coef_d, &
     &          coef_imp, dt, is_fld, n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld
      real(kind = kreal), intent(in) :: coef_d, coef_imp, dt
      real(kind = kreal), intent(in) :: flux_CMB(jmax)
      real(kind = kreal), intent(in) :: r_CMB(0:2)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
!
        d_rj(inod,is_fld) = d_rj(inod,is_fld)                           &
     &                     + dt * coef_imp * coef_d                     &
     &                      * (fdm2_fix_dr_CMB( 1,3)                    &
     &                       + two*r_CMB(1) ) * flux_CMB(j)
      end do
!$omp end parallel do
!
      end subroutine adjust_out_fixed_flux_sph
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine poisson_in_fixed_flux_sph                              &
     &         (jmax, kr_in, r_ICB, fdm2_fix_dr_ICB, flux_ICB, is_fld,  &
     &          n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: jmax, kr_in
      integer(kind = kint), intent(in) :: is_fld
      real(kind = kreal), intent(in) :: flux_ICB(jmax)
      real(kind = kreal), intent(in) :: r_ICB(0:1)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_ICB(-1:1,3)
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, jmax
        inod = j + (kr_in-1) * jmax
!
        d_rj(inod,is_fld) = (fdm2_fix_dr_ICB(-1,3) + two*r_ICB(1))      &
     &                     * flux_ICB(j)
      end do
!$omp end parallel do
!
      end subroutine poisson_in_fixed_flux_sph
!
! -----------------------------------------------------------------------
!
      subroutine poisson_out_fixed_flux_sph                             &
     &         (jmax, kr_out, r_CMB, fdm2_fix_dr_CMB, flux_CMB, is_fld, &
     &          n_point, ntot_phys_rj, d_rj)
!
      integer(kind = kint), intent(in) :: jmax, kr_out
      integer(kind = kint), intent(in) :: is_fld
      real(kind = kreal), intent(in) :: flux_CMB(jmax)
      real(kind = kreal), intent(in) :: r_CMB(0:1)
      real(kind = kreal), intent(in) :: fdm2_fix_dr_CMB(-1:1,3)
!
      integer(kind = kint), intent(in) :: n_point, ntot_phys_rj
      real (kind=kreal), intent(inout) :: d_rj(n_point,ntot_phys_rj)
!
      integer(kind = kint) :: inod, j
!
!
!$omp parallel do private(inod)
      do j = 1, jmax
        inod = j + (kr_out-1) * jmax
!
        d_rj(inod,is_fld) = (fdm2_fix_dr_CMB( 1,3) + two*r_CMB(1))      &
     &                     * flux_CMB(j)
      end do
!$omp end parallel do
!
      end subroutine poisson_out_fixed_flux_sph
!
! -----------------------------------------------------------------------
!
      end module set_scalar_boundary_sph

!>@file   set_evoluved_boundaries.f90
!!@brief  module set_evoluved_boundaries
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief  Evaluate velocity with non-slip boundary at CMB
!!
!!@verbatim
!!      subroutine set_evo_scalar_boundaries                            &
!!     &         (time, sph_rj, sph_bc, ICB_Sevo, CMB_Sevo, bc_Sspec)
!!      subroutine set_evo_vector_boundaries                            &
!!     &         (time, sph_rj, sph_bc, ICB_Uevo, CMB_Uevo, bc_Vspec)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_boundary_type), intent(in) :: sph_bc
!!        type(sph_scalar_BC_evo), intent(in) :: ICB_Sevo, CMB_Sevo
!!        type(sph_scalar_BC_coef), intent(inout) :: bc_Sspec
!!        type(sph_vector_BC_evo), intent(in) :: ICB_Uevo, CMB_Uevo
!!        type(sph_vector_BC_coef), intent(inout) :: bc_Vspec
!!@endverbatim
!!
      module set_evoluved_boundaries
!
      use m_precision
      use m_constants
!
      implicit none
!
      private :: cal_sph_nod_evo_scalar_BC, cal_sph_nod_evo_vector_BC
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_evo_scalar_boundaries                              &
     &         (time, sph_rj, sph_bc, ICB_Sevo, CMB_Sevo, bc_Sspec)
!
      use t_spheric_rj_data
      use t_boundary_params_sph_MHD
      use t_boundary_sph_spectr
!
      real(kind = kreal), intent(in) :: time
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_scalar_BC_evo), intent(in) :: ICB_Sevo, CMB_Sevo
!
      type(sph_scalar_BC_coef), intent(inout) :: bc_Sspec
!
!
      if(sph_bc%iflag_icb .eq. iflag_evolve_field                       &
     &    .or. sph_bc%iflag_icb .eq. iflag_evolve_flux) then
        call cal_sph_nod_evo_scalar_BC                                  &
     &     (time, sph_rj%nidx_rj(2), sph_rj%idx_gl_1d_rj_j,             &
     &      ICB_Sevo%S_BC_mag, ICB_Sevo%S_BC_phase, ICB_Sevo%S_BC_freq, &
     &      bc_Sspec%s_ICB_bc)
      end if
!
      if(sph_bc%iflag_cmb .eq. iflag_evolve_field                       &
     &    .or. sph_bc%iflag_cmb .eq. iflag_evolve_flux) then
        call cal_sph_nod_evo_scalar_BC                                  &
     &     (time, sph_rj%nidx_rj(2), sph_rj%idx_gl_1d_rj_j,             &
     &      CMB_Sevo%S_BC_mag, CMB_Sevo%S_BC_phase, CMB_Sevo%S_BC_freq, &
     &      bc_Sspec%s_CMB_bc)
      end if
!
      end subroutine set_evo_scalar_boundaries
!
! -----------------------------------------------------------------------
!
      subroutine set_evo_vector_boundaries                              &
     &         (time, sph_rj, sph_bc, ICB_Uevo, CMB_Uevo, bc_Vspec)
!
      use t_spheric_rj_data
      use t_boundary_params_sph_MHD
      use t_boundary_sph_spectr
!
      real(kind = kreal), intent(in) :: time
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc
      type(sph_vector_BC_evo), intent(in) :: ICB_Uevo, CMB_Uevo
!
      type(sph_vector_BC_coef), intent(inout) :: bc_Vspec
!
!
      if(sph_bc%iflag_icb .eq. iflag_evolve_field                       &
     &    .or. sph_bc%iflag_icb .eq. iflag_evolve_flux) then
        call cal_sph_nod_evo_vector_BC                                  &
     &     (time, sph_rj%nidx_rj(2), sph_rj%idx_gl_1d_rj_j,             &
     &      ICB_Uevo%Vp_BC_mag, ICB_Uevo%Dp_BC_mag, ICB_Uevo%Vt_BC_mag, &
     &      ICB_Uevo%Vp_BC_phase, ICB_Uevo%Dp_BC_phase,                 &
     &      ICB_Uevo%Vt_BC_phase,                                       &
     &      ICB_Uevo%Vp_BC_freq, ICB_Uevo%Vt_BC_freq,                   &
     &      bc_Vspec%vp_ICB_bc, bc_Vspec%dp_ICB_bc, bc_Vspec%vt_ICB_bc)
      end if
!
      if(sph_bc%iflag_cmb .eq. iflag_evolve_field                       &
     &    .or. sph_bc%iflag_cmb .eq. iflag_evolve_flux) then
        call cal_sph_nod_evo_vector_BC                                  &
     &     (time, sph_rj%nidx_rj(2), sph_rj%idx_gl_1d_rj_j,             &
     &      CMB_Uevo%Vp_BC_mag, CMB_Uevo%Dp_BC_mag, CMB_Uevo%Vt_BC_mag, &
     &      CMB_Uevo%Vp_BC_phase, CMB_Uevo%Dp_BC_phase,                 &
     &      CMB_Uevo%Vt_BC_phase,                                       &
     &      CMB_Uevo%Vp_BC_freq, CMB_Uevo%Vt_BC_freq,                   &
     &      bc_Vspec%vp_CMB_bc, bc_Vspec%dp_CMB_bc, bc_Vspec%vt_CMB_bc)
      end if
!
      end subroutine set_evo_vector_boundaries
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_evo_scalar_BC(time, jmax, idx_rj,          &
     &         S_BC_mag, S_BC_phase, S_BC_freq, S_BC)
!
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: idx_rj(jmax,3)
      real(kind = kreal), intent(in) :: time
      real(kind = kreal), intent(in) :: S_BC_mag(jmax)
      real(kind = kreal), intent(in) :: S_BC_phase(jmax)
      real(kind = kreal), intent(in) :: S_BC_freq(jmax)
!
      real(kind = kreal), intent(inout) :: S_BC(jmax)
!
      integer(kind = kint) :: j, m
      real(kind = kreal) :: phase_sin
!
!
!$omp parallel do private(j,m,phase_sin)
      do j = 1, jmax
        m = abs(idx_rj(j,3))
        phase_sin = dble(sign(1, idx_rj(j,3))) * atan(one)              &
     &             - atan(one)
        S_BC(j) = S_BC_mag(j) * cos(dble(m) * S_BC_freq(j) * time       &
    &                               + S_BC_phase(j) + phase_sin)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_evo_scalar_BC
!
! -----------------------------------------------------------------------
!
      subroutine cal_sph_nod_evo_vector_BC                              &
     &         (time, jmax, idx_rj, Vp_BC_mag, Dp_BC_mag, Vt_BC_mag,    &
     &          Vp_BC_phase, Dp_BC_phase, Vt_BC_phase,                  &
     &          Vp_BC_freq, Vt_BC_freq, Vp_BC, Dp_BC, Vt_BC)
!
      integer(kind = kint), intent(in) :: jmax
      integer(kind = kint), intent(in) :: idx_rj(jmax,3)
      real(kind = kreal), intent(in) :: time
      real(kind = kreal), intent(in) :: Vp_BC_mag(jmax)
      real(kind = kreal), intent(in) :: Dp_BC_mag(jmax)
      real(kind = kreal), intent(in) :: Vt_BC_mag(jmax)
      real(kind = kreal), intent(in) :: Vp_BC_phase(jmax)
      real(kind = kreal), intent(in) :: Dp_BC_phase(jmax)
      real(kind = kreal), intent(in) :: Vt_BC_phase(jmax)
      real(kind = kreal), intent(in) :: Vp_BC_freq(jmax)
      real(kind = kreal), intent(in) :: Vt_BC_freq(jmax)
!
      real(kind = kreal), intent(inout) :: Vp_BC(jmax)
      real(kind = kreal), intent(inout) :: Dp_BC(jmax)
      real(kind = kreal), intent(inout) :: Vt_BC(jmax)
!
      integer(kind = kint) :: j, m
      real(kind = kreal) :: phase_sin
!
!
!$omp parallel do private(j,m,phase_sin)
      do j = 1, jmax
        m = abs(idx_rj(j,3))
        phase_sin = dble(sign(1, idx_rj(j,3))) * atan(one)              &
     &             - atan(one)
        Vp_BC(j) = Vp_BC_mag(j) * cos(dble(m) * Vp_BC_freq(j) * time    &
    &                                 + Vp_BC_phase(j) + phase_sin)
        Dp_BC(j) = Dp_BC_mag(j) * cos(dble(m) * Vp_BC_freq(j) * time    &
    &                                 + Dp_BC_phase(j) + phase_sin)
        Vt_BC(j) = Vt_BC_mag(j) * cos(dble(m) * Vt_BC_freq(j) * time    &
    &                                 + Vt_BC_phase(j) + phase_sin)
      end do
!$omp end parallel do
!
      end subroutine cal_sph_nod_evo_vector_BC
!
! -----------------------------------------------------------------------
!
      end module set_evoluved_boundaries

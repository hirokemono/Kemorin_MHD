!>@file   init_ref_temp_w_IC_source.f90
!!@brief  module init_ref_temp_w_IC_source
!!
!!@author H. Matsui
!!@date Programmed in June, 2013
!
!> @brief Obtain homogeneous sources from boundary flux
!!
!!@verbatim
!!     Tempareture at fluid core
!!       T = const_OC + coef_OC / r - (1/6) source_OC r^2
!!     Tempareture at inner core
!!       T = const_IC - (1/6) source_IC r^2
!!
!!      subroutine reftemp_full_sphere_fix_out(sph, sph_bc_T, bcs_T,    &
!!     &          source_OC, nri_1d, r_1d, reftemp, ref_src)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_boundary_type), intent(in) :: sph_bc_T
!!        type(sph_scalar_boundary_data), intent(in) :: bcs_T
!!        real(kind = kreal), intent(in) :: source_OC
!!        integer(kind = kint), intent(in) :: nri_1d
!!        real(kind = kreal), intent(in) :: r_1d(0:nri_1d)
!!        real(kind = kreal), intent(inout) :: reftemp(0:nri_1d)
!!        real(kind = kreal), intent(inout) :: ref_src(0:nri_1d)
!!      Constant for temperature in whole sphere
!!         Input::  Q_OC, T_CMB
!!         Output:: const_OC
!!           const_OC = T_CMB + 3 * Q_OC * r_CMB^2 / 6
!!
!!      subroutine reftemp_full_sphere_flux_out(sph, sph_bc_T, bcs_T,   &
!!     &          nri_1d, r_1d, reftemp, ref_src, source_OC)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_boundary_type), intent(in) :: sph_bc_T
!!        type(sph_scalar_boundary_data), intent(in) :: bcs_T
!!        integer(kind = kint), intent(in) :: nri_1d
!!        real(kind = kreal), intent(in) :: r_1d(0:nri_1d)
!!        real(kind = kreal), intent(inout) :: reftemp(0:nri_1d)
!!        real(kind = kreal), intent(inout) :: ref_src(0:nri_1d)
!!        real(kind = kreal), intent(inout) :: source_OC
!!      Constant for temperature in whole sphere
!!         Input::  Q_OC, T_CMB = 0.0
!!         Output:: const_OC
!!           const_OC = T_CMB + 3 * Q_OC * r_CMB^2 / 6
!!
!!      subroutine reftemp_w_IC_src_fix_out                             &
!!     &         (sph, sph_bc_T, bcs_T, source_OC, source_IC, kappa_IC, &
!!     &          nri_1d, r_1d, reftemp, ref_src)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_boundary_type), intent(in) :: sph_bc_T
!!        type(sph_scalar_boundary_data), intent(in) :: bcs_T
!!        real(kind = kreal), intent(in) :: source_OC
!!        real(kind = kreal), intent(in) :: source_IC
!!        real(kind = kreal), intent(in) :: kappa_IC
!!        integer(kind = kint), intent(in) :: nri_1d
!!        real(kind = kreal), intent(in) :: r_1d(0:nri_1d)
!!        real(kind = kreal), intent(inout) :: reftemp(0:nri_1d)
!!        real(kind = kreal), intent(inout) :: ref_src(0:nri_1d)
!!     Coefficients for fixed temperature at CMB 
!!                 with sources in inner and outer cores
!!       Input::  Q_IC, Q_OC, kappa_IC, T_CMB
!!       Output:: coef_OC, const_OC, const_IC
!!         coef_OC =  (kappa_IC * Q_IC - Q_OC) * r_ICB^3 / 3
!!         const_OC = T_CMB - coef_OC / r_CMB + source_OC * r_CMB^2 / 6
!!         const_IC = const_OC + coef_OC / r_ICB
!!       &          + (Q_IC - Q_OC) * r_ICB^2 / 6
!!
!!      subroutine reftemp_w_IC_src_flux_out                            &
!!     &         (sph, sph_bc_T, bcs_T, source_OC, kappa_IC,            &
!!     &          nri_1d, r_1d, reftemp, ref_src, source_IC)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_boundary_type), intent(in) :: sph_bc_T
!!        type(sph_scalar_boundary_data), intent(in) :: bcs_T
!!        real(kind = kreal), intent(in) :: source_OC
!!        real(kind = kreal), intent(in) :: kappa_IC
!!        integer(kind = kint), intent(in) :: nri_1d
!!        real(kind = kreal), intent(in) :: r_1d(0:nri_1d)
!!        real(kind = kreal), intent(inout) :: reftemp(0:nri_1d)
!!        real(kind = kreal), intent(inout) :: ref_src(0:nri_1d)
!!        real(kind = kreal), intent(inout) :: source_IC
!!     Coefficients for fixed flux at CMB
!!                 with sources in inner and outer cores
!!       Input::  f_CMB, Q_OC, kappa_IC, T_CMB = 0.0
!!       Output:: Q_IC, coef_OC, const_OC, const_IC
!!         Q_IC = -3 * f_CMB * r_CMB^2 / r_ICB^3
!!               - Q_OC * ((r_CMB/r_ICB)^3 - 1)
!!         coef_OC =  (kappa_IC * Q_IC - Q_OC) * r_ICB^3 / 3
!!         const_OC = T_CMB - coef_OC / r_CMB + source_OC * r_CMB^2 / 6
!!         const_IC = const_OC + coef_OC / r_ICB
!!       &          + (Q_IC - Q_OC) * r_ICB^2 / 6
!!@endverbatim
      module init_ref_temp_w_IC_source
!
      use m_precision
      use m_constants
!
      use t_spheric_parameter
      use t_boundary_params_sph_MHD
      use t_boundary_sph_spectr
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine reftemp_full_sphere_fix_out(sph, sph_bc_T, bcs_T,      &
     &          source_OC, nri_1d, r_1d, reftemp, ref_src)
!
      use spherical_indices_picker
      use initial_reference_setup
      use sources_from_boundary_flux
      use ref_temp_coefs_w_sources
      use set_sph_diffusive_scalar
!
!
      type(sph_grids), intent(in) :: sph
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(sph_scalar_boundary_data), intent(in) :: bcs_T
      real(kind = kreal), intent(in) :: source_OC
      integer(kind = kint), intent(in) :: nri_1d
      real(kind = kreal), intent(in) :: r_1d(0:nri_1d)
!
      real(kind = kreal), intent(inout) :: reftemp(0:nri_1d)
      real(kind = kreal), intent(inout) :: ref_src(0:nri_1d)
!
      integer(kind = kint) :: kr_in, kr_out
      real(kind = kreal) :: r_in, r_out
      real(kind = kreal) :: flux_ICB, temp_CMB
      real(kind = kreal) :: const_OC
!
!
      call boundary_data_for_reference(sph, sph_bc_T, bcs_T,            &
     &    kr_in, kr_out, r_in, r_out, flux_ICB, temp_CMB)
!
      const_OC = reftemp_const_whole_core(r_out, source_OC, temp_CMB)
!
      call init_sph_ref_temp_full_sphere(kr_out, nri_1d, r_1d,          &
     &                                   source_OC, const_OC, reftemp)
      call init_sph_ref_source_full_sphere(kr_out, nri_1d, source_OC,   &
     &                                     ref_src)
!
      end subroutine reftemp_full_sphere_fix_out
!
!-----------------------------------------------------------------------
!
      subroutine reftemp_full_sphere_flux_out(sph, sph_bc_T, bcs_T,     &
     &          nri_1d, r_1d, reftemp, ref_src, source_OC)
!
      use spherical_indices_picker
      use initial_reference_setup
      use sources_from_boundary_flux
      use ref_temp_coefs_w_sources
      use set_sph_diffusive_scalar
!
      type(sph_grids), intent(in) :: sph
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(sph_scalar_boundary_data), intent(in) :: bcs_T
      integer(kind = kint), intent(in) :: nri_1d
      real(kind = kreal), intent(in) :: r_1d(0:nri_1d)
!
      real(kind = kreal), intent(inout) :: reftemp(0:nri_1d)
      real(kind = kreal), intent(inout) :: ref_src(0:nri_1d)
      real(kind = kreal), intent(inout) :: source_OC
!
      integer(kind = kint) :: kr_in, kr_out
      real(kind = kreal) :: r_in, r_out
      real(kind = kreal) :: flux_ICB, flux_CMB
      real(kind = kreal) :: const_OC
!
      real(kind = kreal), parameter :: temp_CMB = zero
!
!
      call boundary_data_for_reference(sph, sph_bc_T, bcs_T,            &
     &    kr_in, kr_out, r_in, r_out, flux_ICB, flux_CMB)
!
      source_OC = source_from_inner_core(r_out, r_out, flux_CMB)
      const_OC = reftemp_const_whole_core(r_out, source_OC, temp_CMB)
!
      call init_sph_ref_temp_full_sphere(kr_out, nri_1d, r_1d,          &
     &                                   source_OC, const_OC, reftemp)
      call init_sph_ref_source_full_sphere(kr_out, nri_1d, source_OC,   &
     &                                     ref_src)
!
      end subroutine reftemp_full_sphere_flux_out
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine reftemp_w_IC_src_fix_out                               &
     &         (sph, sph_bc_T, bcs_T, source_OC, source_IC, kappa_IC,   &
     &          nri_1d, r_1d, reftemp, ref_src)
!
      use spherical_indices_picker
      use initial_reference_setup
      use ref_temp_coefs_w_sources
      use sources_from_boundary_flux
      use set_sph_diffusive_scalar
!
      type(sph_grids), intent(in) :: sph
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(sph_scalar_boundary_data), intent(in) :: bcs_T
      real(kind = kreal), intent(in) :: source_OC, source_IC
      real(kind = kreal), intent(in) :: kappa_IC
      integer(kind = kint), intent(in) :: nri_1d
      real(kind = kreal), intent(in) :: r_1d(0:nri_1d)
!
      real(kind = kreal), intent(inout) :: reftemp(0:nri_1d)
      real(kind = kreal), intent(inout) :: ref_src(0:nri_1d)
!
      integer(kind = kint) :: kr_in, kr_out, kr_ICB
      real(kind = kreal) :: r_in, r_out, rr_ICB
      real(kind = kreal) :: flux_ICB, temp_CMB
      real(kind = kreal) :: const_OC, coef_OC
      real(kind = kreal) :: const_IC
!
!
      call boundary_data_for_reference(sph, sph_bc_T, bcs_T,            &
     &    kr_in, kr_out, r_in, r_out, flux_ICB, temp_CMB)
      kr_ICB = nlayer_ICB(sph)
      rr_ICB = r_ICB(sph)
!
      call reftemp_coefs_w_IC_source(rr_ICB, r_out,                     &
     &    source_OC, source_IC, kappa_IC, temp_CMB,                     &
     &    const_OC, coef_OC, const_IC)
!
      call init_sph_ref_temp_whole_core(kr_ICB, kr_out, nri_1d, r_1d,   &
     &    source_IC, source_OC, const_OC, coef_OC, const_IC, reftemp)
      call init_sph_ref_source_whole_core(kr_ICB, kr_out, nri_1d,       &
     &    source_IC, source_OC, ref_src)
!
!
      end subroutine reftemp_w_IC_src_fix_out
!
!-----------------------------------------------------------------------
!
      subroutine reftemp_w_IC_src_flux_out                              &
     &         (sph, sph_bc_T, bcs_T, source_OC, kappa_IC,              &
     &          nri_1d, r_1d, reftemp, ref_src, source_IC)
!
      use spherical_indices_picker
      use initial_reference_setup
      use ref_temp_coefs_w_sources
      use sources_from_boundary_flux
      use set_sph_diffusive_scalar
!
      type(sph_grids), intent(in) :: sph
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(sph_scalar_boundary_data), intent(in) :: bcs_T
      real(kind = kreal), intent(in) :: source_OC
      real(kind = kreal), intent(in) :: kappa_IC
      integer(kind = kint), intent(in) :: nri_1d
      real(kind = kreal), intent(in) :: r_1d(0:nri_1d)
!
      real(kind = kreal), intent(inout) :: reftemp(0:nri_1d)
      real(kind = kreal), intent(inout) :: ref_src(0:nri_1d)
      real(kind = kreal), intent(inout) :: source_IC
!
      integer(kind = kint) :: kr_in, kr_out, kr_ICB
      real(kind = kreal) :: r_in, r_out, rr_ICB
      real(kind = kreal) :: flux_ICB, flux_CMB
      real(kind = kreal) :: const_OC, coef_OC
      real(kind = kreal) :: const_IC
!
      real(kind = kreal), parameter :: temp_CMB = zero
!
      call boundary_data_for_reference(sph, sph_bc_T, bcs_T,            &
     &    kr_in, kr_out, r_in, r_out, flux_ICB, flux_CMB)
      kr_ICB = nlayer_ICB(sph)
      rr_ICB = r_ICB(sph)
!
      source_IC = inner_core_source_w_OC_sorce(rr_ICB, r_out,           &
     &                                         flux_CMB, source_OC)
      call reftemp_coefs_w_IC_source(rr_ICB, r_out,                     &
     &    source_OC, source_IC, kappa_IC, temp_CMB,                     &
     &    const_OC, coef_OC, const_IC)
!
      call init_sph_ref_temp_whole_core(kr_ICB, kr_out, nri_1d, r_1d,   &
     &    source_IC, source_OC, const_OC, coef_OC, const_IC, reftemp)
      call init_sph_ref_source_whole_core(kr_ICB, kr_out, nri_1d,       &
     &    source_IC, source_OC, ref_src)
!
      end subroutine reftemp_w_IC_src_flux_out
!
!-----------------------------------------------------------------------
!
      end module init_ref_temp_w_IC_source

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
!!   Get homogeneous heat flux in INNER core from CMB heat flux
!!      real(kind = kreal) function source_from_inner_core(r_ICB, r_CMB,&
!!     &                                                   flux_CMB)
!!        real(kind = kreal), intent(in) :: r_ICB, r_CMB
!!        real(kind = kreal), intent(in) :: flux_CMB
!!           source_IC = - 3 * r_CMB^2 / r_ICB^3
!!      real(kind = kreal) function inner_core_source_w_OC_sorce        &
!!     &                          (r_ICB, r_CMB, flux_CMB, source_OC)
!!        real(kind = kreal), intent(in) :: r_ICB, r_CMB
!!        real(kind = kreal), intent(in) :: source_OC
!!        real(kind = kreal), intent(in) :: flux_CMB
!!           source_IC = - 3 * r_CMB^2 / r_ICB^3
!!                      - source_OC * ((r_CMB/r_ICB)^3 - one)
!!
!!      subroutine reftemp_coefs_IC_src_fix_out(idx_degree_zero,        &
!!     &          sph_bc_T, bcs_T, source_OC, source_IC, kappa_IC,      &
!!     &          const_OC, coef_OC, const_IC)
!!        integer(kind = kint), intent(in) :: idx_degree_zero
!!        type(sph_boundary_type), intent(in) :: sph_bc_T
!!        type(sph_scalar_boundary_data), intent(in) :: bcs_T
!!        real(kind = kreal), intent(in) :: source_OC
!!        real(kind = kreal), intent(in) :: source_IC
!!        real(kind = kreal), intent(in) :: kappa_IC
!!        real(kind = kreal), intent(inout) :: const_OC, coef_OC
!!        real(kind = kreal), intent(inout) :: const_IC
!!     Coefficients for fixed temperature at CMB 
!!                 with sources in inner and outer cores
!!       Input::  Q_IC, Q_OC, kappa_IC, T_CMB
!!       Output:: coef_OC, const_OC, const_IC
!!         coef_OC =  (kappa_IC * Q_IC - Q_OC) * r_ICB^3 / 3
!!         const_OC = T_CMB - coef_OC / r_CMB + source_OC * r_CMB^2 / 6
!!         const_IC = const_OC + coef_OC / r_ICB
!!       &          + (Q_IC - Q_OC) * r_ICB^2 / 6
!!
!!      subroutine reftemp_coefs_IC_src_flux_out(idx_degree_zero,       &
!!     &          sph_bc_T, bcs_T, temp_CMB, source_OC, kappa_IC,       &
!!     &          const_OC, coef_OC, const_IC, source_IC)
!!        integer(kind = kint), intent(in) :: idx_degree_zero
!!        type(sph_boundary_type), intent(in) :: sph_bc_T
!!        type(sph_scalar_boundary_data), intent(in) :: bcs_T
!!        type(sph_scalar_boundary_data), intent(in) :: temp_CMB
!!        real(kind = kreal), intent(in) :: source_OC
!!        real(kind = kreal), intent(in) :: kappa_IC
!!        real(kind = kreal), intent(inout) :: source_IC
!!        real(kind = kreal), intent(inout) :: const_OC, coef_OC
!!        real(kind = kreal), intent(inout) :: const_IC
!!     Coefficients for fixed flux at CMB
!!                 with sources in inner and outer cores
!!       Input::  f_CMB, Q_OC, kappa_IC, T_CMB
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
     &          source_OC, n_point, temp_rj)
!
      use spherical_indices_picker
      use sph_boundary_data_picker
      use sources_from_boundary_flux
      use ref_temp_coefs_w_sources
!
!
      type(sph_grids), intent(in) :: sph
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(sph_scalar_boundary_data), intent(in) :: bcs_T
      real(kind = kreal), intent(in) :: source_OC
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(inout) :: temp_rj(n_point)
!
      integer(kind = kint) :: jj
      real(kind = kreal) :: r_out
      real(kind = kreal) :: temp_CMB
      real(kind = kreal) :: const_OC
!
!
      jj = idx_rj_degree_zero(sph)
      if(jj .le. 0) return
      r_out =    sph_outer_boundary_radius(sph_bc_T)
      temp_CMB = sph_outer_boundary_scalar_coef(bcs_T, jj)
!
      const_OC = reftemp_const_whole_core(r_out, source_OC, temp_CMB)
!
      call init_sph_ref_temp_full_sphere(sph, r_out,                    &
     &    source_OC, const_OC, n_point, temp_rj)
!
      end subroutine reftemp_full_sphere_fix_out
!
!-----------------------------------------------------------------------
!
      subroutine reftemp_full_sphere_flux_out(sph, sph_bc_T, bcs_T,     &
     &          source_OC, n_point, temp_rj, source_IC)
!
      use spherical_indices_picker
      use sph_boundary_data_picker
      use sources_from_boundary_flux
      use ref_temp_coefs_w_sources
!
      type(sph_grids), intent(in) :: sph
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(sph_scalar_boundary_data), intent(in) :: bcs_T
      real(kind = kreal), intent(in) :: source_OC
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(inout) :: temp_rj(n_point)
      real(kind = kreal), intent(inout) :: source_IC
!
      integer(kind = kint) :: jj
      real(kind = kreal) :: r_out
      real(kind = kreal) :: flux_CMB
      real(kind = kreal) :: const_OC
!
      real(kind = kreal), parameter :: temp_CMB = zero
!
      jj = idx_rj_degree_zero(sph)
      if(jj .le. 0) return
      r_out =    sph_outer_boundary_radius(sph_bc_T)
      flux_CMB = sph_outer_boundary_scalar_coef(bcs_T, jj)
!
      const_OC = reftemp_const_whole_core(r_out, source_OC, temp_CMB)
!
      call init_sph_ref_temp_full_sphere(sph, r_out,                    &
     &    source_OC, const_OC, n_point, temp_rj)
!
      end subroutine reftemp_full_sphere_flux_out
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine reftemp_w_IC_src_fix_out(sph, sph_bc_T, bcs_T,         &
     &          source_OC, source_IC, kappa_IC, n_point, temp_rj)
!
      use spherical_indices_picker
      use sph_boundary_data_picker
      use sources_from_boundary_flux
!
      type(sph_grids), intent(in) :: sph
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(sph_scalar_boundary_data), intent(in) :: bcs_T
      real(kind = kreal), intent(in) :: source_OC
      real(kind = kreal), intent(in) :: source_IC
      real(kind = kreal), intent(in) :: kappa_IC
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(inout) :: temp_rj(n_point)
!
      integer(kind = kint) :: jj
      real(kind = kreal) :: r_in, r_out
      real(kind = kreal) :: flux_ICB, temp_CMB
      real(kind = kreal) :: const_OC, coef_OC
      real(kind = kreal) :: const_IC
!
!
      jj = idx_rj_degree_zero(sph)
      if(jj .le. 0) return
      r_in =     sph_inner_boundary_radius(sph_bc_T)
      r_out =    sph_outer_boundary_radius(sph_bc_T)
      flux_ICB = sph_inner_boundary_scalar_coef(bcs_T, jj)
      temp_CMB = sph_outer_boundary_scalar_coef(bcs_T, jj)
!
      call reftemp_coefs_w_IC_source(r_in, r_out,                       &
     &    source_OC, source_IC, kappa_IC, temp_CMB,                     &
     &    const_OC, coef_OC, const_IC)
!
      call init_sph_ref_temp_whole_core(sph, r_in, r_out,               &
     &    source_IC, source_OC, const_OC, coef_OC, const_IC,            &
     &    n_point, temp_rj)
!
      end subroutine reftemp_w_IC_src_fix_out
!
!-----------------------------------------------------------------------
!
      subroutine reftemp_w_IC_src_flux_out(sph, sph_bc_T, bcs_T,        &
     &          source_OC, kappa_IC, n_point, temp_rj, source_IC)
!
      use spherical_indices_picker
      use sph_boundary_data_picker
      use sources_from_boundary_flux
!
      type(sph_grids), intent(in) :: sph
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(sph_scalar_boundary_data), intent(in) :: bcs_T
      real(kind = kreal), intent(in) :: source_OC
      real(kind = kreal), intent(in) :: kappa_IC
!
      integer(kind = kint), intent(in) :: n_point
      real(kind = kreal), intent(inout) :: temp_rj(n_point)
      real(kind = kreal), intent(inout) :: source_IC
!
      integer(kind = kint) :: jj
      real(kind = kreal) :: r_in, r_out
      real(kind = kreal) :: flux_ICB, flux_CMB
      real(kind = kreal) :: const_OC, coef_OC
      real(kind = kreal) :: const_IC
!
      real(kind = kreal), parameter :: temp_CMB = zero
!
      jj = idx_rj_degree_zero(sph)
      if(jj .le. 0) return
      r_in =     sph_inner_boundary_radius(sph_bc_T)
      r_out =    sph_outer_boundary_radius(sph_bc_T)
      flux_ICB = sph_inner_boundary_scalar_coef(bcs_T, jj)
      flux_CMB = sph_outer_boundary_scalar_coef(bcs_T, jj)
!
      source_IC = inner_core_source_w_OC_sorce(r_in, r_out,             &
     &                                         flux_CMB, source_OC)
      call reftemp_coefs_w_IC_source(r_in, r_out,                       &
     &    source_OC, source_IC, kappa_IC, temp_CMB,                     &
     &    const_OC, coef_OC, const_IC)
!
      call init_sph_ref_temp_whole_core(sph, r_in, r_out,               &
     &    source_IC, source_OC, const_OC, coef_OC, const_IC,            &
     &    n_point, temp_rj)
!
      end subroutine reftemp_w_IC_src_flux_out
!
!-----------------------------------------------------------------------
!
      end module init_ref_temp_w_IC_source

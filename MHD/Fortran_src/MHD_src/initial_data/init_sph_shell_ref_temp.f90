!>@file   init_sph_shell_ref_temp.f90
!!@brief  module init_sph_shell_ref_temp
!!
!!@author H. Matsui
!!@date Programmed in June, 2013
!
!> @brief Obtain homogeneous sources from boundary flux
!!
!!@verbatim
!!     Tempareture at fluid core
!!       T = const_OC + coef_OC / r - (1/6) source_OC r^2
!!
!!      subroutine reftemp_OC_fix_in_fix_out(sph, sph_bc_T, bcs_T,      &
!!     &          source_OC, nri_1d, r_1d, reftemp, ref_src)
!!      subroutine reftemp_OC_fix_in_flux_out(sph, sph_bc_T, bcs_T,     &
!!     &          source_OC, nri_1d, r_1d, reftemp, ref_src)
!!      subroutine reftemp_OC_flux_in_fix_out(sph, sph_bc_T, bcs_T,     &
!!     &          source_OC, nri_1d, r_1d, reftemp, ref_src)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_boundary_type), intent(in) :: sph_bc_T
!!        type(sph_scalar_boundary_data), intent(in) :: bcs_T
!!        real(kind = kreal), intent(in) :: source_OC
!!        integer(kind = kint), intent(in) :: nri_1d
!!        real(kind = kreal), intent(in) :: r_1d(0:nri_1d)
!!        real(kind = kreal), intent(inout) :: reftemp(0:nri_1d)
!!        real(kind = kreal), intent(inout) :: ref_src(0:nri_1d)
!!
!!      subroutine reftemp_OC_flux_in_flux_out(sph, sph_bc_T, bcs_T,    &
!!     &          nri_1d, r_1d, reftemp, ref_src, source_OC)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_boundary_type), intent(in) :: sph_bc_T
!!        type(sph_scalar_boundary_data), intent(in) :: bcs_T
!!        integer(kind = kint), intent(in) :: nri_1d
!!        real(kind = kreal), intent(in) :: r_1d(0:nri_1d)
!!        real(kind = kreal), intent(inout) :: reftemp(0:nri_1d)
!!        real(kind = kreal), intent(inout) :: ref_src(0:nri_1d)
!!        real(kind = kreal), intent(inout) :: source_OC
!!@endverbatim
      module init_sph_shell_ref_temp
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
      subroutine reftemp_OC_fix_in_fix_out(sph, sph_bc_T, bcs_T,        &
     &          source_OC, nri_1d, r_1d, reftemp, ref_src)
!
      use spherical_indices_picker
      use initial_reference_setup
      use ref_temp_coefs_w_sources
      use set_sph_diffusive_scalar
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
      real(kind = kreal) :: temp_ICB, temp_CMB
      real(kind = kreal) :: const_OC, coef_OC
!
!
      call boundary_data_for_reference(sph, sph_bc_T, bcs_T,            &
     &          kr_in, kr_out, r_in, r_out, temp_ICB, temp_CMB)
!
      call reftemp_coefs_fix_in_fix_out(r_in, r_out, source_OC,         &
     &    temp_ICB, temp_CMB, const_OC, coef_OC)
!
      call init_sph_ref_temp_outer_core(kr_in, kr_out, nri_1d, r_1d,    &
     &    source_OC, const_OC, coef_OC, reftemp)
      call init_sph_ref_source_whole_core(kr_in, kr_out, nri_1d,        &
     &                                    zero, source_OC, ref_src)
!
      end subroutine reftemp_OC_fix_in_fix_out
!
!-----------------------------------------------------------------------
!
      subroutine reftemp_OC_fix_in_flux_out(sph, sph_bc_T, bcs_T,       &
     &          source_OC, nri_1d, r_1d, reftemp, ref_src)
!
      use spherical_indices_picker
      use initial_reference_setup
      use ref_temp_coefs_w_sources
      use set_sph_diffusive_scalar
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
      real(kind = kreal) :: temp_ICB, flux_CMB
      real(kind = kreal) :: const_OC, coef_OC
!
!
      call boundary_data_for_reference(sph, sph_bc_T, bcs_T,            &
     &          kr_in, kr_out, r_in, r_out, temp_ICB, flux_CMB)
!
      call reftemp_coefs_fix_in_flux_out(r_in, r_out, source_OC,        &
     &    temp_ICB, flux_CMB, const_OC, coef_OC)
!
      call init_sph_ref_temp_outer_core(kr_in, kr_out, nri_1d, r_1d,    &
     &    source_OC, const_OC, coef_OC, reftemp)
      call init_sph_ref_source_whole_core(kr_in, kr_out, nri_1d,        &
     &                                    zero, source_OC, ref_src)
!
      end subroutine reftemp_OC_fix_in_flux_out
!
!-----------------------------------------------------------------------
!
      subroutine reftemp_OC_flux_in_fix_out(sph, sph_bc_T, bcs_T,       &
     &          source_OC, nri_1d, r_1d, reftemp, ref_src)
!
      use spherical_indices_picker
      use initial_reference_setup
      use ref_temp_coefs_w_sources
      use set_sph_diffusive_scalar
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
      real(kind = kreal) :: const_OC, coef_OC
!
!
      call boundary_data_for_reference(sph, sph_bc_T, bcs_T,            &
     &          kr_in, kr_out, r_in, r_out, flux_ICB, temp_CMB)
!
      call reftemp_coefs_flux_in_fix_out(r_in, r_out, source_OC,        &
     &    flux_ICB, temp_CMB, const_OC, coef_OC)
!
      call init_sph_ref_temp_outer_core(kr_in, kr_out, nri_1d, r_1d,    &
     &    source_OC, const_OC, coef_OC, reftemp)
      call init_sph_ref_source_whole_core(kr_in, kr_out, nri_1d,        &
     &                                    zero, source_OC, ref_src)
!
      end subroutine reftemp_OC_flux_in_fix_out
!
!-----------------------------------------------------------------------
!
      subroutine reftemp_OC_flux_in_flux_out(sph, sph_bc_T, bcs_T,      &
     &          nri_1d, r_1d, reftemp, ref_src, source_OC)
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
      real(kind = kreal) :: const_OC, coef_OC
!
      real(kind = kreal), parameter :: temp_CMB = zero
!
!
      call boundary_data_for_reference(sph, sph_bc_T, bcs_T,            &
     &          kr_in, kr_out, r_in, r_out, flux_ICB, flux_CMB)
!
      source_OC = source_by_both_fluxes(r_in, r_out,                    &
     &                                  flux_ICB, flux_CMB)
      call reftemp_coefs_flux_in_flux_out(r_in, r_out, source_OC,       &
     &    flux_ICB, flux_CMB, temp_CMB, const_OC, coef_OC)
!
      call init_sph_ref_temp_outer_core(kr_in, kr_out, nri_1d, r_1d,    &
     &    source_OC, const_OC, coef_OC, reftemp)
      call init_sph_ref_source_whole_core(kr_in, kr_out, nri_1d,        &
     &                                    zero, source_OC, ref_src)
!
      end subroutine reftemp_OC_flux_in_flux_out
!
!-----------------------------------------------------------------------
!
      end module init_sph_shell_ref_temp

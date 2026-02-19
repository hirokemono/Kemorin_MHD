!>@file   sel_init_reftemp_sph_shell.f90
!!@brief  module sel_init_reftemp_sph_shell
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
!!      subroutine s_sel_init_reftemp_sph_shell                         &
!!     &         (sph, sph_bc_T, bcs_T, kappa_IC, n_point,              &
!!     &          temp_rj, source_rj, source_IC, source_OC)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_boundary_type), intent(in) :: sph_bc_T
!!        type(sph_scalar_boundary_data), intent(in) :: bcs_T
!!        real(kind = kreal), intent(in) :: kappa_IC
!!        integer(kind = kint), intent(in) :: n_point
!!        real(kind = kreal), intent(inout) :: temp_rj(n_point)
!!        real(kind = kreal), intent(inout) :: source_rj(n_point)
!!        real(kind = kreal), intent(inout) :: source_IC, source_OC
!!
!!      subroutine sel_reftemp_sph_shell_w_IC                           &
!!     &         (sph, sph_bc_T, bcs_T, kappa_IC, nri_1d, r_1d,         &
!!     &          reftemp, ref_src, source_IC, source_OC)
!!        type(sph_grids), intent(in) :: sph
!!        type(sph_boundary_type), intent(in) :: sph_bc_T
!!        type(sph_scalar_boundary_data), intent(in) :: bcs_T
!!        integer(kind = kint), intent(in) :: nri_1d
!!        real(kind = kreal), intent(in) :: r_1d(0:nri_1d)
!!        real(kind = kreal), intent(in) :: kappa_IC
!!        real(kind = kreal), intent(inout) :: reftemp(0:nri_1d)
!!        real(kind = kreal), intent(inout) :: ref_src(0:nri_1d)
!!        real(kind = kreal), intent(inout) :: source_IC, source_OC
!!@endverbatim
      module sel_init_reftemp_sph_shell
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
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_sel_init_reftemp_sph_shell                           &
     &         (sph, sph_bc_T, bcs_T, kappa_IC, n_point,                &
     &          temp_rj, source_rj, source_IC, source_OC)
!
      use spherical_indices_picker
      use initial_reference_setup
!
      type(sph_grids), intent(in) :: sph
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(sph_scalar_boundary_data), intent(in) :: bcs_T
      real(kind = kreal), intent(in) :: kappa_IC
      integer(kind = kint), intent(in) :: n_point
!
      real(kind = kreal), intent(inout) :: temp_rj(n_point)
      real(kind = kreal), intent(inout) :: source_rj(n_point)
      real(kind = kreal), intent(inout) :: source_IC, source_OC
!
      integer(kind = kint) :: nri_1d
      real(kind = kreal), allocatable :: r_1d(:)
      real(kind = kreal), allocatable :: reftemp(:)
      real(kind = kreal), allocatable :: ref_src(:)
!
!
      nri_1d = num_rj_radial_point(sph)
      allocate(r_1d(0:nri_1d))
      allocate(reftemp(0:nri_1d))
      allocate(ref_src(0:nri_1d))
      call copy_radius_rj(sph, nri_1d, r_1d)
!
      call sel_reftemp_sph_shell_w_IC(sph, sph_bc_T, bcs_T, kappa_IC,   &
     &    nri_1d, r_1d, reftemp, ref_src, source_IC, source_OC)
!
      call copy_from_reference(sph, nri_1d, reftemp, n_point, temp_rj)
      call copy_from_reference(sph, nri_1d, ref_src,                    &
     &                         n_point, source_rj)
      deallocate(ref_src, reftemp, r_1d)
!
      end subroutine s_sel_init_reftemp_sph_shell
!
! -----------------------------------------------------------------------
!
      subroutine sel_reftemp_sph_shell_w_IC                             &
     &         (sph, sph_bc_T, bcs_T, kappa_IC, nri_1d, r_1d,           &
     &          reftemp, ref_src, source_IC, source_OC)
!
      use spherical_indices_picker
      use init_ref_temp_w_IC_source
      use init_sph_shell_ref_temp
!
      type(sph_grids), intent(in) :: sph
      type(sph_boundary_type), intent(in) :: sph_bc_T
      type(sph_scalar_boundary_data), intent(in) :: bcs_T
      integer(kind = kint), intent(in) :: nri_1d
      real(kind = kreal), intent(in) :: r_1d(0:nri_1d)
      real(kind = kreal), intent(in) :: kappa_IC
!
      real(kind = kreal), intent(inout) :: reftemp(0:nri_1d)
      real(kind = kreal), intent(inout) :: ref_src(0:nri_1d)
      real(kind = kreal), intent(inout) :: source_IC, source_OC
!
!
      if(sph_bc_T%iflag_icb .eq. iflag_sph_fill_center) then
!-----------  Full sphere
        if(nlayer_ICB(sph) .le. 0) then
          if(     sph_bc_T%iflag_cmb .eq. iflag_fixed_field             &
     &       .or. sph_bc_T%iflag_cmb .eq. iflag_evolve_field) then
            call reftemp_full_sphere_fix_out(sph, sph_bc_T, bcs_T,      &
     &          source_OC, nri_1d, r_1d, reftemp, ref_src)
          else if(sph_bc_T%iflag_cmb .eq. iflag_fixed_flux              &
     &       .or. sph_bc_T%iflag_cmb .eq. iflag_evolve_flux) then
            call reftemp_full_sphere_flux_out(sph, sph_bc_T, bcs_T,     &
     &          nri_1d, r_1d, reftemp, ref_src, source_OC)
          end if
!
!-----------  fluid spherical shell with solid core
        else
          if(     sph_bc_T%iflag_cmb .eq. iflag_fixed_field             &
     &       .or. sph_bc_T%iflag_cmb .eq. iflag_evolve_field) then
            call reftemp_w_IC_src_fix_out                               &
     &         (sph, sph_bc_T, bcs_T, source_OC, source_IC, kappa_IC,   &
     &          nri_1d, r_1d, reftemp, ref_src)
          else if(sph_bc_T%iflag_cmb .eq. iflag_fixed_flux              &
     &       .or. sph_bc_T%iflag_cmb .eq. iflag_evolve_flux) then
            call reftemp_w_IC_src_flux_out                              &
     &         (sph, sph_bc_T, bcs_T, source_OC, kappa_IC,              &
     &          nri_1d, r_1d, reftemp, ref_src, source_IC)
          end if
        end if
!
!-----------  spherical shell
      else if(sph_bc_T%iflag_icb .eq. iflag_fixed_field                 &
     &   .or. sph_bc_T%iflag_icb .eq. iflag_evolve_field) then
        if(     sph_bc_T%iflag_cmb .eq. iflag_fixed_field               &
     &     .or. sph_bc_T%iflag_cmb .eq. iflag_evolve_field) then
          call reftemp_OC_fix_in_fix_out(sph, sph_bc_T, bcs_T,          &
     &        source_OC, nri_1d, r_1d, reftemp, ref_src)
        else if(sph_bc_T%iflag_cmb .eq. iflag_fixed_flux                &
     &     .or. sph_bc_T%iflag_cmb .eq. iflag_evolve_flux) then
          call reftemp_OC_fix_in_flux_out(sph, sph_bc_T, bcs_T,         &
     &        source_OC, nri_1d, r_1d, reftemp, ref_src)
        end if
      else if(sph_bc_T%iflag_icb .eq. iflag_fixed_flux                  &
     &   .or. sph_bc_T%iflag_icb .eq. iflag_evolve_flux) then
        if(     sph_bc_T%iflag_cmb .eq. iflag_fixed_field               &
     &     .or. sph_bc_T%iflag_cmb .eq. iflag_evolve_field) then
          call reftemp_OC_flux_in_fix_out(sph, sph_bc_T, bcs_T,         &
     &        source_OC, nri_1d, r_1d, reftemp, ref_src)
        else if(sph_bc_T%iflag_cmb .eq. iflag_fixed_flux                &
     &     .or. sph_bc_T%iflag_cmb .eq. iflag_evolve_flux) then
          call reftemp_OC_flux_in_flux_out(sph, sph_bc_T, bcs_T,        &
     &        nri_1d, r_1d, reftemp, ref_src, source_OC)
        end if
      end if
!
      end subroutine sel_reftemp_sph_shell_w_IC
!
!-----------------------------------------------------------------------
!
      end module sel_init_reftemp_sph_shell

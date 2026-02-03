!>@file   sph_initial_licv_control.f90
!!@brief  module sph_initial_licv_control
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2026
!
!>@brief Set initial field for liniear convection evolution
!!
!!@verbatim
!!      subroutine s_sph_initial_licv_control(MHD_files, MHD_step, sph, &
!!     &          ipol, MHD_prop, sph_MHD_bc, refs, rj_fld, sph_fst_IO)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(sph_grids), intent(in) :: sph
!!        type(phys_address), intent(in) :: ipol
!!        type(MHD_step_param), intent(in) :: MHD_step
!!        type(MHD_evolution_param), intent(in) :: MHD_prop
!!        type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
!!        type(radial_reference_field), intent(in) :: refs
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(field_IO), intent(inout) :: sph_fst_IO
!!@endverbatim
!
      module sph_initial_licv_control
!
      use m_precision
      use m_machine_parameter
!
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_phys_data
      use t_phys_address
      use t_boundary_data_sph_MHD
!
      implicit none
!
      private :: sph_initial_field_4_licv, sph_initial_scalar_licv
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine s_sph_initial_licv_control(MHD_files, MHD_step, sph,   &
     &          ipol, MHD_prop, sph_MHD_bc, refs, rj_fld, sph_fst_IO)
!
      use calypso_mpi
      use m_initial_field_control
!
      use t_MHD_step_parameter
      use t_control_parameter
      use t_radial_reference_field
      use t_field_data_IO
!
      use set_sph_restart_IO
      use sph_mhd_rst_IO_control
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(sph_grids), intent(in) :: sph
      type(phys_address), intent(in) :: ipol
      type(MHD_step_param), intent(in) :: MHD_step
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(radial_reference_field), intent(in) :: refs
!
      type(phys_data), intent(inout) :: rj_fld
      type(field_IO), intent(inout) :: sph_fst_IO
!
!
      call sph_initial_field_4_licv(sph%sph_rj, MHD_prop, sph_MHD_bc,   &
     &    refs%iref_base, refs%ref_field, ipol%base, rj_fld)
!
      if(iflag_debug .gt. 0) write(*,*) 'copy_time_step_data'
      call set_sph_restart_num_to_IO(rj_fld, sph_fst_IO)
!
      if(iflag_debug .gt. 0) write(*,*) 'output_sph_restart_control'
      call output_sph_restart_control(MHD_step%init_d%i_time_step,      &
     &    MHD_files%fst_file_IO, MHD_step%time_d, rj_fld,               &
     &    MHD_step%rst_step, sph_fst_IO)
      call calypso_mpi_barrier
!
      end subroutine s_sph_initial_licv_control
!
!-----------------------------------------------------------------------
!
      subroutine sph_initial_field_4_licv                               &
     &         (sph_rj, MHD_prop, sph_MHD_bc,                           &
     &          iref_base, ref_field, ipol_base, rj_fld)
!
      use t_base_field_labels
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(MHD_evolution_param), intent(in) :: MHD_prop
      type(sph_MHD_boundary_data), intent(in) :: sph_MHD_bc
      type(base_field_address), intent(in) :: iref_base
      type(phys_data), intent(in) :: ref_field
      type(base_field_address), intent(in) :: ipol_base
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      if((ipol_base%i_temp*iref_base%i_temp) .gt. 0) then
        call sph_initial_scalar_licv(sph_rj, sph_MHD_bc%sph_bc_T,       &
     &      iref_base%i_temp, ref_field, ipol_base%i_temp, rj_fld)
      end if
!
      if((ipol_base%i_light*iref_base%i_light) .gt. 0) then
        call sph_initial_scalar_licv(sph_rj, sph_MHD_bc%sph_bc_C,       &
     &      iref_base%i_light, ref_field, ipol_base%i_light, rj_fld)
      end if
!
      end subroutine sph_initial_field_4_licv
!
!-----------------------------------------------------------------------
!
      subroutine sph_initial_scalar_licv(sph_rj, sph_bc_S,              &
     &          iref_scalar, ref_field, ipol_scalar, rj_fld)
!
      use t_reference_scalar_param
      use t_boundary_params_sph_MHD
!
      use set_initial_sph_scalars
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_boundary_type), intent(in) :: sph_bc_S
      type(phys_data), intent(in) :: ref_field
      integer(kind = kint), intent(in) :: iref_scalar
      integer(kind = kint), intent(in) :: ipol_scalar
!
      type(phys_data), intent(inout) :: rj_fld
!
!
      call set_ini_reference_temp_sph(sph_rj,                           &
     &    ref_field%d_fld(1,iref_scalar), rj_fld%d_fld(1,ipol_scalar))
      call set_noize_scalar_sph                                         &
     &   (sph_rj, sph_bc_S%r_ICB(0), sph_bc_S%r_CMB(0),                 &
     &    sph_bc_S%kr_in, sph_bc_S%kr_out, rj_fld%d_fld(1,ipol_scalar))
!
      end subroutine sph_initial_scalar_licv
!
!-----------------------------------------------------------------------
!
      end module sph_initial_licv_control

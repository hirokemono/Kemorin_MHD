!>@file   input_control_sph_SGS_MHD.f90
!!@brief  module input_control_sph_SGS_MHD
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in March, 2015
!
!>@brief  Load mesh and filtering data for MHD simulation
!!
!!@verbatim
!!      subroutine input_control_SPH_dynamo(MHD_files, bc_IO, MHD_ctl,  &
!!     &          sph, comms_sph, sph_grps, rj_fld, nod_fld, pwr,       &
!!     &          SGS_par, dynamic_SPH, flex_p, MHD_step, MHD_prop,     &
!!     &          MHD_BC, WK, femmesh, ele_mesh)
!!        type(MHD_file_IO_params), intent(inout) :: MHD_files
!!        type(sph_sgs_mhd_control), intent(inout) :: MHD_ctl
!!        type(DNS_mhd_simulation_control), intent(inout) :: DMHD_ctl
!!        type(sph_grids), intent(inout) :: sph
!!        type(sph_comm_tables), intent(inout) :: comms_sph
!!        type(sph_group_data), intent(inout) ::  sph_grps
!!        type(construct_spherical_grid), intent(inout) :: gen_sph1
!!        type(phys_data), intent(inout) :: rj_fld
!!        type(phys_data), intent(inout) :: nod_fld
!!        type(sph_mean_squares), intent(inout) :: pwr
!!        type(SGS_paremeters), intent(inout) :: SGS_par
!!        type(sph_filters_type), intent(inout) :: sph_filters(1)
!!        type(mesh_data), intent(inout) :: femmesh
!!        type(element_geometry), intent(inout) :: ele_mesh
!!        type(flexible_stepping_parameter), intent(inout) :: flex_p
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(MHD_evolution_param), intent(inout) :: MHD_prop
!!        type(MHD_BC_lists), intent(inout) :: MHD_BC
!!@endverbatim
!
!
      module input_control_sph_SGS_MHD
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use t_control_parameter
      use t_const_spherical_grid
      use t_MHD_file_parameter
      use t_MHD_step_parameter
      use t_spheric_parameter
      use t_mesh_data
      use t_phys_data
      use t_spheric_mesh
      use t_group_data
      use t_rms_4_sph_spectr
      use t_file_IO_parameter
      use t_sph_trans_arrays_MHD
      use t_sph_boundary_input_data
      use t_bc_data_list
      use t_sph_filtering
      use t_select_make_SPH_mesh
      use t_flex_delta_t_data
!
      implicit none
!
!>      Structure to construct grid
      type(sph_grid_maker_in_sim), save :: sph_maker2
!
      private :: sph_maker2
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine input_control_SPH_dynamo(MHD_files, bc_IO, MHD_ctl,    &
     &          sph, comms_sph, sph_grps, rj_fld, nod_fld, pwr,         &
     &          SGS_par, dynamic_SPH, flex_p, MHD_step, MHD_prop,       &
     &          MHD_BC, WK, femmesh, ele_mesh)
!
      use m_error_IDs
!
      use t_SGS_control_parameter
      use t_ctl_data_SGS_MHD
      use set_control_sph_SGS_MHD
      use sph_file_IO_select
      use set_control_4_SPH_to_FEM
!
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(boundary_spectra), intent(inout) :: bc_IO
      type(sph_sgs_mhd_control), intent(inout) :: MHD_ctl
      type(sph_grids), intent(inout) :: sph
      type(sph_comm_tables), intent(inout) :: comms_sph
      type(sph_group_data), intent(inout) ::  sph_grps
!
      type(phys_data), intent(inout) :: rj_fld
      type(phys_data), intent(inout) :: nod_fld
      type(sph_mean_squares), intent(inout) :: pwr
      type(SGS_paremeters), intent(inout) :: SGS_par
      type(dynamic_SGS_data_4_sph), intent(inout) :: dynamic_SPH
      type(flexible_stepping_parameter), intent(inout) :: flex_p
      type(MHD_step_param), intent(inout) :: MHD_step
      type(MHD_evolution_param), intent(inout) :: MHD_prop
      type(MHD_BC_lists), intent(inout) :: MHD_BC
      type(works_4_sph_trans_MHD), intent(inout) :: WK
!
      type(mesh_data), intent(inout) :: femmesh
      type(element_geometry), intent(inout) :: ele_mesh
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_SGS_MHD'
      call set_control_4_SPH_SGS_MHD(MHD_ctl%plt, MHD_ctl%org_plt,      &
     &    MHD_ctl%model_ctl, MHD_ctl%smctl_ctl, MHD_ctl%smonitor_ctl,   &
     &    MHD_ctl%nmtr_ctl, MHD_ctl%psph_ctl, sph_maker2%sph_tmp,       &
     &    rj_fld, MHD_files, bc_IO, pwr,                                &
     &    SGS_par, dynamic_SPH%sph_filters, flex_p, MHD_step,           &
     &    MHD_prop, MHD_BC, WK%WK_sph, sph_maker2%gen_sph)
!
      call s_set_control_4_SPH_to_FEM                                   &
     &   (MHD_ctl%psph_ctl%spctl, sph%sph_params, rj_fld, nod_fld)
!
!
      call select_make_SPH_mesh(MHD_ctl%psph_ctl%iflag_sph_shell,       &
     &    sph, comms_sph, sph_grps, sph_maker2,                         &
     &    femmesh%mesh, femmesh%group, ele_mesh,                        &
     &    MHD_files%mesh_file_IO)
!
      call sph_boundary_IO_control(MHD_prop, MHD_BC, bc_IO)
!
      end subroutine input_control_SPH_dynamo
!
! ----------------------------------------------------------------------
!
      end module input_control_sph_SGS_MHD

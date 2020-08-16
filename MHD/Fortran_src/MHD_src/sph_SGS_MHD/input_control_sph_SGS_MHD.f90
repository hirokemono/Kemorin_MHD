!>@file   input_control_sph_SGS_MHD.f90
!!@brief  module input_control_sph_SGS_MHD
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in March, 2015
!
!>@brief  Load mesh and filtering data for MHD simulation
!!
!!@verbatim
!!      subroutine input_control_SPH_SGS_dynamo                         &
!!     &         (MHD_files, MHD_ctl, MHD_step, SPH_model,              &
!!     &          SPH_WK, SPH_SGS, SPH_MHD, FEM_dat)
!!        type(MHD_file_IO_params), intent(inout) :: MHD_files
!!        type(sph_sgs_mhd_control), intent(inout) :: MHD_ctl
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(SPH_MHD_model_data), intent(inout) :: SPH_model
!!        type(work_SPH_MHD), intent(inout) :: SPH_WK
!!        type(SPH_SGS_structure), intent(inout) :: SPH_SGS
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(FEM_mesh_field_data), intent(inout) :: FEM_dat
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
      use t_SPH_MHD_model_data
      use t_SPH_mesh_field_data
      use t_FEM_mesh_field_data
      use t_rms_4_sph_spectr
      use t_file_IO_parameter
      use t_sph_boundary_input_data
      use t_bc_data_list
      use t_SPH_SGS_structure
      use t_check_and_make_SPH_mesh
      use t_flex_delta_t_data
      use t_work_SPH_MHD
!
      implicit none
!
!>      Structure to construct grid
      type(sph_grid_maker_in_sim), save, private:: sph_maker2
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine input_control_SPH_SGS_dynamo                           &
     &         (MHD_files, MHD_ctl, MHD_step, SPH_model,                &
     &          SPH_WK, SPH_SGS, SPH_MHD, FEM_dat)
!
      use m_error_IDs
!
      use t_ctl_data_SGS_MHD
      use set_control_sph_SGS_MHD
      use sph_file_IO_select
      use set_control_4_SPH_to_FEM
      use sel_make_SPH_mesh_w_LIC
!
      type(MHD_file_IO_params), intent(inout) :: MHD_files
      type(sph_sgs_mhd_control), intent(inout) :: MHD_ctl
!
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(work_SPH_MHD), intent(inout) :: SPH_WK
!
      type(SPH_SGS_structure), intent(inout) :: SPH_SGS
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(FEM_mesh_field_data), intent(inout) :: FEM_dat
!
!
      if (iflag_debug.eq.1) write(*,*) 'set_control_4_SPH_SGS_MHD'
      call set_control_4_SPH_SGS_MHD                                    &
     &   (MHD_ctl%plt, MHD_ctl%org_plt, MHD_ctl%model_ctl,              &
     &    MHD_ctl%smctl_ctl, MHD_ctl%nmtr_ctl, MHD_ctl%psph_ctl,        &
     &    MHD_files, SPH_model%bc_IO, SPH_SGS%SGS_par, SPH_SGS%dynamic, &
     &    MHD_step, SPH_model%MHD_prop, SPH_model%MHD_BC,               &
     &    SPH_WK%trans_p, SPH_WK%trns_WK, sph_maker2)
!
      call set_control_SGS_SPH_MHD_field(MHD_ctl%model_ctl,             &
     &    MHD_ctl%psph_ctl, MHD_ctl%smonitor_ctl, MHD_ctl%zm_ctls,      &
     &    SPH_SGS%SGS_par, SPH_model%MHD_prop, SPH_MHD%sph,             &
     &    SPH_MHD%fld, FEM_dat%field, SPH_WK%monitor)
!
!
!  Check and construct spherical shell table
      call check_and_make_SPH_mesh(MHD_ctl%psph_ctl%iflag_sph_shell,    &
     &    MHD_files%sph_file_param, sph_maker2)
!
!  Load spherical shell table
      if (iflag_debug.eq.1) write(*,*) 'load_para_SPH_and_FEM_w_LIC'
      call load_para_SPH_and_FEM_w_LIC                                  &
     &   (MHD_files%FEM_mesh_flags, MHD_files%sph_file_param,           &
     &    SPH_MHD%sph, SPH_MHD%comms, SPH_MHD%groups, FEM_dat%geofem,   &
     &    MHD_files%mesh_file_IO, sph_maker2%gen_sph)
      call dealloc_sph_sgs_mhd_ctl_data(MHD_ctl)
!
      call sph_boundary_IO_control                                      &
     &   (SPH_model%MHD_prop, SPH_model%MHD_BC, SPH_model%bc_IO)
!
      end subroutine input_control_SPH_SGS_dynamo
!
! ----------------------------------------------------------------------
!
      end module input_control_sph_SGS_MHD

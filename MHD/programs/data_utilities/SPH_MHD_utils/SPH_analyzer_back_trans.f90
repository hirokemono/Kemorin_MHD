!>@file   SPH_analyzer_back_trans
!!@brief  module SPH_analyzer_back_trans
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evolution loop for spherical MHD
!!
!!@verbatim
!!      subroutine SPH_init_sph_back_trans(MHD_files, SPH_model,        &
!!     &          SPH_SGS, SPH_MHD, SPH_WK, m_SR)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(SPH_MHD_model_data), intent(inout) :: SPH_model
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(SPH_SGS_structure)< intent(inout) ::  SPH_SGS
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(work_SPH_MHD), intent(inout) :: SPH_WK
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine SPH_analyze_back_trans(MHD_files, SPH_SGS, MHD_step, &
!!     &                                  SPH_MHD, SPH_WK, m_SR)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(SPH_SGS_structure), intent(in) ::  SPH_SGS
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(work_SPH_MHD), intent(inout) :: SPH_WK
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module SPH_analyzer_back_trans
!
      use m_precision
      use calypso_mpi
      use m_work_time
      use m_elapsed_labels_4_MHD
      use t_phys_address
      use t_SGS_model_addresses
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_SPH_MHD_model_data
      use t_SPH_mesh_field_data
      use t_SPH_SGS_structure
      use t_control_parameter
      use t_boundary_data_sph_MHD
      use t_work_SPH_MHD
      use t_mesh_SR
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_init_sph_back_trans(MHD_files, SPH_model,          &
     &          MHD_step, SPH_SGS, SPH_MHD, SPH_WK, m_SR)
!
      use m_constants
      use calypso_mpi
      use m_machine_parameter
!
      use t_sph_boundary_input_data
!
      use set_control_sph_mhd
      use set_field_data_w_SGS
      use adjust_reference_fields
      use set_bc_sph_mhd
      use adjust_reference_fields
      use material_property
      use init_sphrical_transform_MHD
      use init_radial_infos_sph_mhd
      use const_radial_mat_4_sph
      use r_interpolate_sph_data
      use sph_mhd_rst_IO_control
      use sph_filtering
      use cal_rms_fields_by_sph
      use input_control_sph_MHD
      use back_sph_trans_4_all_field
      use sph_SGS_mhd_monitor_data_IO
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
!
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_SGS_structure), intent(inout) ::  SPH_SGS
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(work_SPH_MHD), intent(inout) :: SPH_WK
      type(mesh_SR), intent(inout) :: m_SR
!
!   Allocate spectr field data
!
      call init_field_data_w_SGS(SPH_MHD%sph%sph_rj%nnod_rj,            &
     &    SPH_MHD%fld, SPH_MHD%ipol, SPH_SGS%ipol_LES)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_r_infos_sph_mhd_evo'
      call init_r_infos_sph_mhd_evo(SPH_model%bc_IO, SPH_MHD%groups,    &
     &   SPH_model%MHD_BC, SPH_MHD%ipol, SPH_MHD%sph, SPH_WK%r_2nd,     &
     &   SPH_model%omega_sph, SPH_model%MHD_prop, SPH_model%sph_MHD_bc)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_sph_back_transform'
      call init_sph_back_transform(SPH_model, SPH_WK%trans_p,           &
     &    SPH_WK%trns_WK, SPH_MHD, m_SR%SR_sig, m_SR%SR_r)
!
! ---------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' read_alloc_sph_restart_data'
      call read_alloc_sph_restart_data(MHD_files%fst_file_IO,           &
     &    MHD_step%init_d, MHD_step%time_d, SPH_MHD%fld,                &
     &    MHD_step%rst_step)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_reference_fields '
      call init_reference_fields                                        &
     &   (SPH_MHD%sph, SPH_MHD%ipol, SPH_WK%r_2nd, SPH_model%refs,      &
     &    SPH_MHD%fld, SPH_model%MHD_prop, SPH_model%sph_MHD_bc)
!
      call init_radial_sph_interpolation(MHD_files%org_rj_file_IO,      &
     &    SPH_MHD%sph%sph_params, SPH_MHD%sph%sph_rj, SPH_WK%rj_itp)
!
      end subroutine SPH_init_sph_back_trans
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_back_trans(MHD_files, MHD_step, SPH_MHD,   &
     &                                  SPH_WK, m_SR)
!
      use t_sph_mhd_monitor_data_IO
!
      use cal_nonlinear
      use cal_sol_sph_MHD_crank
      use adjust_reference_fields
      use lead_fields_4_sph_mhd
      use sph_mhd_rst_IO_control
      use input_control_sph_MHD
!
      use back_sph_trans_4_all_field
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(work_SPH_MHD), intent(inout) :: SPH_WK
      type(mesh_SR), intent(inout) :: m_SR
!
!
      call read_alloc_sph_spectr                                        &
     &   (MHD_step%time_d%i_time_step, MHD_step%ucd_step,               &
     &    MHD_files%org_rj_file_IO, MHD_files%sph_file_IO,              &
     &    SPH_MHD%sph%sph_rj, SPH_MHD%ipol, SPH_MHD%fld,                &
     &    MHD_step%init_d, SPH_WK%rj_itp)
      call copy_time_data(MHD_step%init_d, MHD_step%time_d)
!
!* ----  Update fields after time evolution ------------------------=
!*
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+5)
      if (iflag_debug.eq.1) write(*,*) 'sph_all_back_transform'
      call sph_all_back_transform                                       &
     &   (SPH_MHD%sph, SPH_MHD%comms, SPH_WK%trans_p,                   &
     &    SPH_MHD%fld, SPH_WK%trns_WK%trns_MHD, SPH_WK%trns_WK%WK_leg,  &
     &    SPH_WK%trns_WK%WK_FFTs, m_SR%SR_sig, m_SR%SR_r)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+5)
!
      end subroutine SPH_analyze_back_trans
!
! ----------------------------------------------------------------------
!
!      subroutine SPH_finalize_snap
!
!      end subroutine SPH_finalize_snap
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_back_trans

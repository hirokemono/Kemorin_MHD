!>@file   SPH_analyzer_SGS_snap
!!@brief  module SPH_analyzer_SGS_snap
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evolution loop for spherical MHD
!!
!!@verbatim
!!      subroutine SPH_init_SGS_snap(MHD_files, FEM_dat, SPH_model,     &
!!     &          MHD_step, SPH_SGS, SPH_MHD, SPH_WK, m_SR)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(FEM_mesh_field_data), intent(inout) :: FEM_dat
!!        type(SPH_MHD_model_data), intent(inout) :: SPH_model
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(SPH_SGS_structure), intent(inout) :: SPH_SGS
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(work_SPH_MHD), intent(inout) :: SPH_WK
!!        type(mesh_SR), intent(inout) :: m_SR
!!      subroutine SPH_analyze_SGS_snap(MHD_files, SPH_model, MHD_step, &
!!     &                                SPH_SGS, SPH_MHD, SPH_WK, m_SR)
!!        type(phys_address), intent(in) :: iphys
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(SPH_MHD_model_data), intent(inout) :: SPH_model
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(SPH_SGS_structure), intent(inout) :: SPH_SGS
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(work_SPH_MHD), intent(inout) :: SPH_WK
!!        type(mesh_SR), intent(inout) :: m_SR
!!@endverbatim
!
      module SPH_analyzer_SGS_snap
!
      use m_precision
      use m_work_time
      use m_elapsed_labels_4_MHD
      use t_SPH_MHD_model_data
      use t_control_parameter
      use t_phys_address
      use t_MHD_file_parameter
      use t_SPH_SGS_structure
      use t_SPH_mesh_field_data
      use t_FEM_mesh_field_data
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
      subroutine SPH_init_SGS_snap(MHD_files, FEM_dat, SPH_model,       &
     &          MHD_step, SPH_SGS, SPH_MHD, SPH_WK, m_SR)
!
      use m_constants
      use calypso_mpi
      use m_machine_parameter
!
      use t_sph_boundary_input_data
!
      use set_control_sph_mhd
      use adjust_reference_fields
      use set_bc_sph_mhd
      use adjust_reference_fields
      use material_property
      use cal_SGS_nonlinear
      use init_sph_trans_SGS_MHD
      use init_radial_infos_sph_mhd
      use const_radial_mat_4_sph
      use r_interpolate_sph_data
      use sph_mhd_rst_IO_control
      use sph_filtering
      use check_dependency_SGS_MHD
      use input_control_sph_MHD
      use sph_SGS_mhd_monitor_data_IO
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(FEM_mesh_field_data), intent(in) :: FEM_dat
!
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(SPH_SGS_structure), intent(inout) :: SPH_SGS
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(work_SPH_MHD), intent(inout) :: SPH_WK
      type(mesh_SR), intent(inout) :: m_SR
!
!   Allocate spectr field data
!
      call set_sph_SGS_MHD_spectr_data                                  &
     &   (SPH_SGS%SGS_par, SPH_model%MHD_prop,                          &
     &    SPH_MHD%sph, SPH_MHD%fld, SPH_MHD%ipol, SPH_SGS%ipol_LES)
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
      if (iflag_debug.gt.0) write(*,*) 'init_sph_transform_SGS_MHD'
      call init_sph_transform_SGS_MHD(SPH_model, SPH_SGS%SGS_par,       &
     &    SPH_SGS%ipol_LES, SPH_SGS%iphys_LES, FEM_dat%iphys,           &
     &    SPH_WK%trans_p, SPH_WK%trns_WK, SPH_SGS%trns_WK_LES,          &
     &    SPH_MHD, m_SR%SR_sig, m_SR%SR_r)
!
      call init_SGS_model_sph_mhd                                       &
     &   (SPH_SGS%SGS_par, SPH_MHD%sph, SPH_MHD%groups,                 &
     &    SPH_model%MHD_prop, SPH_WK%trans_p, SPH_SGS%dynamic)
!
!  -------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' read_alloc_sph_restart_data'
      call read_alloc_sph_restart_data(MHD_files%fst_file_IO,           &
     &    MHD_step%init_d, SPH_MHD%fld, MHD_step%rst_step)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_reference_fields '
      call init_reference_fields                                        &
     &   (SPH_MHD%sph, SPH_MHD%ipol, SPH_WK%r_2nd, SPH_model%refs,      &
     &    SPH_MHD%fld, SPH_model%MHD_prop, SPH_model%sph_MHD_bc)
!
      if (iflag_debug.eq.1) write(*,*) 'const_radial_mat_sph_snap'
      call const_radial_mat_sph_snap                                    &
     &   (SPH_model%MHD_prop, SPH_model%sph_MHD_bc,                     &
     &    SPH_MHD%sph%sph_rj, SPH_WK%r_2nd, SPH_WK%trans_p%leg,         &
     &    SPH_WK%MHD_mats)
!
!     --------------------- 
!  set original spectr mesh data for extension of B
!
      call init_radial_sph_interpolation(MHD_files%org_rj_file_IO,      &
     &    SPH_MHD%sph%sph_params, SPH_MHD%sph%sph_rj, SPH_WK%rj_itp)
!*
      if(iflag_debug .gt. 0) write(*,*) 'init_rms_sph_SGS_mhd_control'
      call init_rms_sph_SGS_mhd_control(SPH_model%MHD_prop,             &
     &    SPH_model%sph_MHD_bc, SPH_WK%r_2nd, SPH_WK%trans_p,           &
     &    FEM_dat%field, SPH_SGS, SPH_MHD, SPH_WK%MHD_mats,             &
     &    SPH_WK%monitor, m_SR%SR_sig, m_SR%SR_r)
!
      end subroutine SPH_init_SGS_snap
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_SGS_snap(MHD_files, SPH_model, MHD_step,   &
     &                                SPH_SGS, SPH_MHD, SPH_WK, m_SR)
!
      use cal_SGS_nonlinear
      use cal_sol_sph_MHD_crank
      use adjust_reference_fields
      use lead_fields_SPH_SGS_MHD
      use sph_SGS_MHD_rst_IO_control
      use input_control_sph_MHD
      use output_viz_file_control
      use sph_SGS_mhd_monitor_data_IO
      use sph_radial_grad_4_magne
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_SGS_structure), intent(inout) :: SPH_SGS
      type(work_SPH_MHD), intent(inout) :: SPH_WK
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(mesh_SR), intent(inout) :: m_SR
!
!
      call read_alloc_sph_rst_SGS_snap                                  &
     &   (MHD_step%time_d%i_time_step, MHD_files%org_rj_file_IO,        &
     &    MHD_files, MHD_step%rst_step, MHD_step%init_d,                &
     &    SPH_MHD, SPH_SGS, SPH_WK%rj_itp)
      call extend_by_potential_with_j                                   &
     &   (SPH_MHD%sph%sph_rj, SPH_model%sph_MHD_bc%sph_bc_B,            &
     &    SPH_MHD%ipol%base%i_magne, SPH_MHD%ipol%base%i_current,       &
     &    SPH_MHD%fld)

      call copy_time_data(MHD_step%init_d, MHD_step%time_d)
!
      if (iflag_debug.eq.1) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph                                    &
     &   (SPH_model%MHD_prop, SPH_model%refs,                           &
     &    SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%fld)
!
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_MHD_evolved_boundaries'
      call set_MHD_evolved_boundaries(MHD_step%time_d, SPH_MHD%sph,     &
     &    SPH_model%MHD_prop, SPH_model%sph_MHD_bc)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start(SPH_MHD%sph%sph_rj, SPH_WK%r_2nd,     &
     &    SPH_model%MHD_prop, SPH_model%sph_MHD_bc, SPH_WK%trans_p%leg, &
     &    SPH_MHD%ipol, SPH_MHD%fld)
!
!*  ----------------lead nonlinear term ... ----------
!*
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+4)
      call nonlinear_with_SGS(MHD_step%time_d%i_time_step,              &
     &    SPH_WK%r_2nd, SPH_model, SPH_WK%trans_p, SPH_WK%trns_WK,      &
     &    SPH_SGS, SPH_MHD, m_SR%SR_sig, m_SR%SR_r)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+4)
!
!* ----  Update fields after time evolution ------------------------=
!*
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+5)
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph                                   &
     &   (SPH_model%MHD_prop, SPH_model%refs,                           &
     &    SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%fld)
!*
      if(lead_field_data_flag(MHD_step%time_d%i_time_step,              &
     &                        MHD_step)) then
        if(iflag_debug.gt.0) write(*,*) 'lead_fields_4_SPH_SGS_MHD'
        call lead_fields_4_SPH_SGS_MHD                                  &
     &     (SPH_SGS%SGS_par, SPH_WK%monitor, SPH_WK%r_2nd,              &
     &      SPH_model%MHD_prop, SPH_model%sph_MHD_bc, SPH_WK%trans_p,   &
     &      SPH_SGS%ipol_LES, SPH_WK%MHD_mats, SPH_WK%trns_WK,          &
     &      SPH_SGS%trns_WK_LES, SPH_SGS%dynamic, SPH_MHD,              &
     &      m_SR%SR_sig, m_SR%SR_r)
      end if
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+5)
!
!*  -----------  lead energy data --------------
!*
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+7)
      if(output_IO_flag(MHD_step%time_d%i_time_step,                    &
     &                  MHD_step%rms_step)) then
        if(iflag_debug .gt. 0)                                          &
     &                write(*,*) 'output_rms_sph_SGS_mhd_control'
        call output_rms_sph_SGS_mhd_control(MHD_step%time_d, SPH_SGS,   &
     &      SPH_MHD, SPH_model%MHD_prop, SPH_model%sph_MHD_bc,          &
     &      SPH_WK%r_2nd, SPH_WK%trans_p, SPH_WK%MHD_mats,              &
     &      SPH_WK%monitor, m_SR%SR_sig)
      end if
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+7)
!
!*  -----------  Output spectr data --------------
!*
      if(iflag_debug.gt.0)  write(*,*) 'output_spectr_4_snap'
      call output_spectr_4_snap                                         &
     &   (MHD_step%time_d%i_time_step, MHD_step%time_d,                 &
     &    MHD_files%sph_file_IO, SPH_MHD%fld, MHD_step%ucd_step)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
      end subroutine SPH_analyze_SGS_snap
!
! ----------------------------------------------------------------------
!
!      subroutine SPH_finalize_SGS_snap
!
!      end subroutine SPH_finalize_SGS_snap
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_SGS_snap

!>@file   SPH_analyzer_snap
!!@brief  module SPH_analyzer_snap
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evolution loop for spherical MHD
!!
!!@verbatim
!!      subroutine SPH_init_sph_snap(MHD_files, iphys, SPH_model,       &
!!     &          SPH_SGS, SPH_WK)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(phys_address), intent(in) :: iphys
!!        type(SPH_MHD_model_data), intent(inout) :: SPH_model
!!        type(SPH_SGS_structure), intent(inout) :: SPH_SGS
!!        type(work_SPH_MHD), intent(inout) :: SPH_WK
!!      subroutine SPH_analyze_snap(i_step, MHD_files, SPH_model,       &
!!     &          MHD_step, SPH_SGS, SPH_WK)
!!        type(phys_address), intent(in) :: iphys
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(SPH_MHD_model_data), intent(inout) :: SPH_model
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(SPH_SGS_structure), intent(inout) :: SPH_SGS
!!        type(work_SPH_MHD), intent(inout) :: SPH_WK
!!@endverbatim
!
      module SPH_analyzer_snap
!
      use m_precision
      use m_MHD_step_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
      use t_SPH_MHD_model_data
      use t_control_parameter
      use t_phys_address
      use t_MHD_file_parameter
      use t_SPH_SGS_structure
      use t_boundary_data_sph_MHD
      use t_work_SPH_MHD
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_init_sph_snap(MHD_files, iphys, SPH_model,         &
     &          SPH_SGS, SPH_WK)
!
      use m_constants
      use calypso_mpi
      use m_machine_parameter
!
      use t_sph_boundary_input_data
!
      use set_control_sph_mhd
      use const_fdm_coefs
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
      type(phys_address), intent(in) :: iphys
!
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(SPH_SGS_structure), intent(inout) :: SPH_SGS
      type(work_SPH_MHD), intent(inout) :: SPH_WK
!
!   Allocate spectr field data
!
      call set_sph_SGS_MHD_spectr_data                                  &
     &   (SPH_SGS%SGS_par, SPH_model%MHD_prop,                          &
     &    SPH_SGS%sph, SPH_SGS%ipol, SPH_SGS%fld)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_r_infos_sph_mhd_evo'
      call init_r_infos_sph_mhd_evo(SPH_WK%r_2nd, SPH_model%bc_IO,      &
     &    SPH_SGS%groups, SPH_model%MHD_BC, SPH_SGS%ipol, SPH_SGS%sph,  &
     &    SPH_model%omega_sph, SPH_model%ref_temp, SPH_model%ref_comp,  &
     &    SPH_SGS%fld, SPH_model%MHD_prop, SPH_model%sph_MHD_bc)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_sph_transform_SGS_MHD'
      call init_sph_transform_SGS_MHD                                   &
     &   (SPH_model, iphys, SPH_WK%trans_p, SPH_WK%trns_WK, SPH_SGS)
!
! ---------------------------------
!
      call init_SGS_model_sph_mhd                                       &
     &   (SPH_SGS%SGS_par, SPH_SGS%sph, SPH_SGS%groups,                 &
     &    SPH_model%MHD_prop, SPH_WK%trans_p, SPH_SGS%dynamic)
!
!  -------------------------------
!
      if (iflag_debug.eq.1) write(*,*) 'const_radial_mat_sph_snap'
      call const_radial_mat_sph_snap                                    &
     &   (SPH_model%MHD_prop, SPH_model%sph_MHD_bc,                     &
     &    SPH_SGS%sph%sph_rj, SPH_WK%r_2nd, SPH_WK%trans_p%leg,         &
     &    SPH_WK%MHD_mats)
!
!     --------------------- 
!  set original spectr mesh data for extension of B
!
      call init_radial_sph_interpolation(MHD_files%org_rj_file_IO,      &
     &    SPH_SGS%sph%sph_params, SPH_SGS%sph%sph_rj)
!*
      if(iflag_debug .gt. 0) write(*,*) 'open_sph_vol_rms_file_SGS_mhd'
      call open_sph_vol_rms_file_SGS_mhd                                &
     &   (SPH_SGS%sph, SPH_SGS%ipol, SPH_SGS%fld, SPH_WK%monitor)
!
      end subroutine SPH_init_sph_snap
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_snap(i_step, MHD_files, SPH_model,         &
     &          MHD_step, SPH_SGS, SPH_WK)
!
      use cal_SGS_nonlinear
      use cal_sol_sph_MHD_crank
      use adjust_reference_fields
      use lead_fields_SPH_SGS_MHD
      use sph_SGS_MHD_rst_IO_control
      use input_control_sph_MHD
      use output_viz_file_control
      use sph_SGS_mhd_monitor_data_IO
      use self_buoyancy_w_filter_sph
!
      integer(kind = kint), intent(in) :: i_step
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_SGS_structure), intent(inout) :: SPH_SGS
      type(work_SPH_MHD), intent(inout) :: SPH_WK
!
      integer(kind = kint) :: iflag
!
!
      call read_alloc_sph_rst_SGS_snap                                  &
     &   (i_step, MHD_files%org_rj_file_IO, MHD_files,                  &
     &    MHD_step%rst_step, MHD_step%init_d, SPH_SGS)

      call copy_time_data(MHD_step%init_d, MHD_step%time_d)
!
      if (iflag_debug.eq.1) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph(SPH_model,                         &
     &    SPH_SGS%sph%sph_rj, SPH_SGS%ipol, SPH_SGS%fld)
!
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_MHD_evolved_boundaries'
      call set_MHD_evolved_boundaries(MHD_step%time_d, SPH_SGS%sph,     &
     &    SPH_model%MHD_prop, SPH_model%sph_MHD_bc)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start(SPH_SGS%sph%sph_rj, SPH_WK%r_2nd,     &
     &    SPH_model%MHD_prop, SPH_model%sph_MHD_bc, SPH_WK%trans_p%leg, &
     &    SPH_SGS%ipol, SPH_SGS%fld)
      if(iflag_debug .gt. 0) write(*,*) 'rot_self_filter_buoyancy_sph'
      call rot_self_filter_buoyancy_sph                                 &
     &   (SPH_SGS%sph%sph_rj, SPH_SGS%ipol, SPH_model%MHD_prop,         &
     &    SPH_model%sph_MHD_bc%sph_bc_U, SPH_SGS%fld)
!
!*  ----------------lead nonlinear term ... ----------
!*
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+4)
      call nonlinear_with_SGS(i_step, SPH_WK%r_2nd, SPH_model,          &
     &    SPH_WK%trans_p, SPH_WK%trns_WK, SPH_SGS)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+4)
!
!* ----  Update fields after time evolution ------------------------=
!*
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+5)
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph(SPH_model,                        &
     &    SPH_SGS%sph%sph_rj, SPH_SGS%ipol, SPH_SGS%fld)
!*
      if(lead_field_data_flag(i_step, MHD_step) .eq. 0) then
        if(iflag_debug.gt.0) write(*,*) 'lead_fields_4_SPH_SGS_MHD'
        call lead_fields_4_SPH_SGS_MHD                                  &
     &     (SPH_WK%monitor, SPH_WK%r_2nd, SPH_model%MHD_prop,           &
     &      SPH_model%sph_MHD_bc, SPH_WK%trans_p, SPH_WK%MHD_mats,      &
     &      SPH_WK%trns_WK, SPH_SGS%dynamic, SPH_SGS)
      end if
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+5)
!
!*  -----------  lead energy data --------------
!*
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+7)
      if(output_IO_flag(i_step, MHD_step%rms_step) .eq. 0) then
        if(iflag_debug .gt. 0)                                          &
     &                write(*,*) 'output_rms_sph_SGS_mhd_control'
        call output_rms_sph_SGS_mhd_control(MHD_step%time_d, SPH_SGS,   &
     &      SPH_model%sph_MHD_bc, SPH_WK%trans_p%leg, SPH_WK%monitor)
      end if
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+7)
!
!*  -----------  Output spectr data --------------
!*
      if(iflag_debug.gt.0)  write(*,*) 'output_spectr_4_snap'
      call output_spectr_4_snap(i_step, MHD_step%time_d,                &
     &    MHD_files%sph_file_IO, SPH_SGS%fld, MHD_step%ucd_step)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
      end subroutine SPH_analyze_snap
!
! ----------------------------------------------------------------------
!
!      subroutine SPH_finalize_snap
!
!      end subroutine SPH_finalize_snap
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_snap

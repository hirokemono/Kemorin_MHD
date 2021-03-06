!>@file   SPH_analyzer_snap_w_psf
!!@brief  module SPH_analyzer_snap_w_psf
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evolution loop for spherical MHD
!!
!!@verbatim
!!      subroutine SPH_init_sph_snap_psf(MHD_files, iphys,              &
!!     &          SPH_model, SPH_MHD, SPH_WK)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(phys_address), intent(in) :: iphys
!!        type(SPH_MHD_model_data), intent(inout) :: SPH_model
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(work_SPH_MHD), intent(inout) :: SPH_WK
!!      subroutine SPH_analyze_snap_psf(i_step, MHD_files, SPH_model,   &
!!     &          MHD_step, SPH_MHD, SPH_WK)
!!        type(phys_address), intent(in) :: iphys
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(SPH_MHD_model_data), intent(inout) :: SPH_model
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(work_SPH_MHD), intent(inout) :: SPH_WK
!!@endverbatim
!
      module SPH_analyzer_snap_w_psf
!
      use m_precision
      use m_MHD_step_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
      use t_SPH_MHD_model_data
      use t_control_parameter
      use t_phys_address
      use t_MHD_file_parameter
      use t_SPH_mesh_field_data
      use t_boundary_data_sph_MHD
      use t_work_SPH_MHD
      use t_sph_mhd_monitor_data_IO
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_init_sph_snap_psf(MHD_files, iphys,                &
     &          SPH_model, SPH_MHD, SPH_WK)
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
      use init_sphrical_transform_MHD
      use init_radial_infos_sph_mhd
      use const_radial_mat_4_sph
      use r_interpolate_sph_data
      use sph_mhd_rst_IO_control
      use check_dependency_for_MHD
      use input_control_sph_MHD
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(phys_address), intent(in) :: iphys
!
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(work_SPH_MHD), intent(inout) :: SPH_WK
!
!   Allocate spectr field data
!
      call set_sph_MHD_sprctr_data(SPH_model%MHD_prop, SPH_MHD)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_r_infos_sph_mhd_evo'
      call init_r_infos_sph_mhd_evo(SPH_WK%r_2nd, SPH_model%bc_IO,      &
     &    SPH_MHD%groups, SPH_model%MHD_BC, SPH_MHD%ipol, SPH_MHD%sph,  &
     &    SPH_model%omega_sph, SPH_model%ref_temp, SPH_model%ref_comp,  &
     &    SPH_MHD%fld, SPH_model%MHD_prop, SPH_model%sph_MHD_bc)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_sph_transform_MHD'
      call init_sph_transform_MHD                                       &
     &   (SPH_model, iphys, SPH_WK%trans_p, SPH_WK%trns_WK, SPH_MHD)
!
! ---------------------------------
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
     &    SPH_MHD%sph%sph_params, SPH_MHD%sph%sph_rj)
!*
      if(iflag_debug .gt. 0) write(*,*) 'open_sph_vol_rms_file_mhd'
      call open_sph_vol_rms_file_mhd                                    &
     &   (SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%fld, SPH_WK%monitor)
!
      end subroutine SPH_init_sph_snap_psf
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_snap_psf(i_step, MHD_files, SPH_model,     &
     &          MHD_step, SPH_MHD, SPH_WK)
!
      use cal_nonlinear
      use cal_sol_sph_MHD_crank
      use adjust_reference_fields
      use lead_fields_4_sph_mhd
      use sph_mhd_rst_IO_control
      use input_control_sph_MHD
      use output_viz_file_control
!
      integer(kind = kint), intent(in) :: i_step
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(MHD_step_param), intent(inout) :: MHD_step
      type(work_SPH_MHD), intent(inout) :: SPH_WK
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!
!
      call read_alloc_sph_rst_4_snap                                    &
     &   (i_step, MHD_files%org_rj_file_IO, MHD_files%fst_file_IO,      &
     &    MHD_step%rst_step, SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%fld,    &
     &    MHD_step%init_d)

      call copy_time_data(MHD_step%init_d, MHD_step%time_d)
!
      if (iflag_debug.eq.1) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph(SPH_model,                         &
     &    SPH_MHD%sph%sph_rj, SPH_MHD%ipol, SPH_MHD%fld)
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
      call nonlinear(SPH_WK%r_2nd, SPH_model,                           &
     &    SPH_WK%trans_p, SPH_WK%trns_WK, SPH_MHD)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+4)
!
!* ----  Update fields after time evolution ------------------------=
!*
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+5)
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph(SPH_model,                        &
     &    SPH_MHD%sph%sph_rj, SPH_MHD%ipol, SPH_MHD%fld)
!*
      if(lead_field_data_flag(i_step, MHD_step)) then
        if(iflag_debug.gt.0) write(*,*) 's_lead_fields_4_sph_mhd'
        call s_lead_fields_4_sph_mhd(SPH_WK%monitor, SPH_WK%r_2nd,      &
     &      SPH_model%MHD_prop, SPH_model%sph_MHD_bc, SPH_WK%trans_p,   &
     &      SPH_WK%MHD_mats, SPH_WK%trns_WK, SPH_MHD)
      end if
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+5)
!
!*  -----------  lead energy data --------------
!*
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+7)
      if(output_IO_flag(i_step, MHD_step%rms_step)) then
        if(iflag_debug .gt. 0)                                          &
     &                write(*,*) 'output_rms_sph_mhd_control'
        call output_rms_sph_mhd_control(MHD_step%time_d, SPH_MHD,       &
     &      SPH_model%sph_MHD_bc, SPH_WK%trans_p%leg, SPH_WK%monitor)
      end if
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+7)
!
!*  -----------  Output spectr data --------------
!*
      if(iflag_debug.gt.0)  write(*,*) 'output_spectr_4_snap'
      call output_spectr_4_snap(i_step, MHD_step%time_d,                &
     &    MHD_files%sph_file_IO, SPH_MHD%fld, MHD_step%ucd_step)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
      end subroutine SPH_analyze_snap_psf
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_snap_w_psf

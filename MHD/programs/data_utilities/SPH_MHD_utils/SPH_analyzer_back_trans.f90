!>@file   SPH_analyzer_back_trans
!!@brief  module SPH_analyzer_back_trans
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evolution loop for spherical MHD
!!
!!@verbatim
!!      subroutine SPH_init_sph_back_trans(MHD_files, bc_IO, iphys,     &
!!     &          SPH_model, SPH_MHD, SPH_WK)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(boundary_spectra), intent(in) :: bc_IO
!!        type(phys_address), intent(in) :: iphys
!!        type(SPH_MHD_model_data), intent(inout) :: SPH_model
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(work_SPH_MHD), intent(inout) :: SPH_WK
!!      subroutine SPH_analyze_back_trans                               &
!!     &         (i_step, MHD_files, MHD_step, SPH_MHD, SPH_WK)
!!        type(boundary_spectra), intent(in) :: bc_IO
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(work_SPH_MHD), intent(inout) :: SPH_WK
!!@endverbatim
!
      module SPH_analyzer_back_trans
!
      use m_precision
      use calypso_mpi
      use t_phys_address
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_SPH_MHD_model_data
      use t_SPH_mesh_field_data
      use t_control_parameter
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
      subroutine SPH_init_sph_back_trans(MHD_files, bc_IO, iphys,       &
     &          SPH_model, SPH_MHD, SPH_WK)
!
      use m_constants
      use calypso_mpi
      use m_machine_parameter
!
      use m_bc_data_list
!
      use t_sph_boundary_input_data
!
      use set_control_sph_mhd
      use set_sph_phys_address
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
      use sph_filtering
      use cal_rms_fields_by_sph
      use input_control_sph_MHD
      use back_sph_trans_4_all_field
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(boundary_spectra), intent(in) :: bc_IO
      type(phys_address), intent(in) :: iphys
!
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(work_SPH_MHD), intent(inout) :: SPH_WK
!
!
!   Allocate spectr field data
!
      call set_sph_sprctr_data_address(SPH_MHD%sph%sph_rj,              &
     &    SPH_MHD%ipol, SPH_MHD%idpdr, SPH_MHD%itor, SPH_MHD%fld)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_r_infos_sph_mhd_evo'
      call init_r_infos_sph_mhd_evo                                     &
     &   (bc_IO, SPH_MHD%groups, MHD_BC1, SPH_MHD%ipol, SPH_MHD%sph,    &
     &    SPH_model, SPH_WK%r_2nd, SPH_MHD%fld)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_sph_back_transform'
      call init_sph_back_transform                                      &
     &   (SPH_model, iphys, SPH_WK%trans_p, SPH_WK%trns_WK, SPH_MHD)
!
! ---------------------------------
!
      call init_radial_sph_interpolation(MHD_files%org_rj_file_IO,      &
     &    SPH_MHD%sph%sph_params, SPH_MHD%sph%sph_rj)
!
! ---------------------------------
!
      call init_rms_4_sph_spectr_4_mhd                                  &
     &   (SPH_MHD%sph, SPH_MHD%fld, SPH_WK%monitor)
!
      end subroutine SPH_init_sph_back_trans
!
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_back_trans                                 &
     &         (i_step, MHD_files, MHD_step, SPH_MHD, SPH_WK)
!
      use m_work_time
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
      integer(kind = kint), intent(in) :: i_step
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(work_SPH_MHD), intent(inout) :: SPH_WK
!
!
      call read_alloc_sph_spectr                                        &
     &   (i_step, MHD_files%org_rj_file_IO, MHD_files%sph_file_IO,      &
     &    SPH_MHD%sph%sph_rj, SPH_MHD%ipol, SPH_MHD%fld,                &
     &    MHD_step%ucd_step, MHD_step%init_d)
      call copy_time_data(MHD_step%init_d, MHD_step%time_d)
!
!* ----  Update fields after time evolution ------------------------=
!*
      call start_elapsed_time(9)
      if (iflag_debug.eq.1) write(*,*) 'sph_all_back_transform'
      call sph_all_back_transform                                       &
     &   (SPH_MHD%sph, SPH_MHD%comms, SPH_WK%trans_p,                   &
     &    SPH_MHD%fld, SPH_WK%trns_WK%trns_MHD, SPH_WK%trns_WK%WK_sph)
       call end_elapsed_time(9)
!
!*  -----------  lead energy data --------------
!*
      call start_elapsed_time(11)
      if(iflag_debug.gt.0)  write(*,*) 'output_rms_sph_back_trans'
      call output_rms_sph_back_trans                                    &
     &   (MHD_step, SPH_MHD%sph%sph_params, SPH_MHD%sph%sph_rj,         &
     &    SPH_WK%trans_p%leg, SPH_MHD%ipol, SPH_MHD%fld,                &
     &    SPH_WK%monitor)
      call end_elapsed_time(11)
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
      subroutine output_rms_sph_back_trans(MHD_step,                    &
     &          sph_params, sph_rj, leg, ipol, rj_fld, monitor)
!
      use m_machine_parameter
      use t_MHD_step_parameter
      use t_schmidt_poly_on_rtm
      use t_sph_mhd_monitor_data_IO
!
      use cal_rms_fields_by_sph
      use volume_average_4_sph
      use output_sph_m_square_file
!
      type(MHD_step_param), intent(in) :: MHD_step
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(legendre_4_sph_trans), intent(in) :: leg
      type(phys_address), intent(in) :: ipol
      type(phys_data), intent(in) :: rj_fld
!
      type(sph_mhd_monitor_data), intent(inout) :: monitor
!
      integer(kind = kint) :: iflag
!
!
      iflag = output_IO_flag(MHD_step%time_d%i_time_step,               &
     &                       MHD_step%rms_step)
      if(iflag .ne. 0) return
!
      if(iflag_debug.gt.0)  write(*,*) 'cal_rms_sph_outer_core'
      call cal_mean_squre_in_shell                                      &
     &   (sph_params%l_truncation, sph_rj, ipol, rj_fld, leg%g_sph_rj,  &
     &    monitor%pwr, monitor%WK_pwr)
!
      call write_sph_vol_ave_file                                       &
     &   (MHD_step%time_d, sph_params, sph_rj, monitor%pwr)
      call write_sph_vol_ms_file                                        &
     &   (my_rank, MHD_step%time_d, sph_params, sph_rj, monitor%pwr)
      call write_sph_vol_ms_spectr_file                                 &
     &   (my_rank, MHD_step%time_d, sph_params, sph_rj, monitor%pwr)
      call write_sph_layer_ms_file                                      &
     &   (my_rank, MHD_step%time_d, sph_params, monitor%pwr)
      call write_sph_layer_spectr_file                                  &
     &   (my_rank, MHD_step%time_d, sph_params, monitor%pwr)
!
      end subroutine output_rms_sph_back_trans
!
!  --------------------------------------------------------------------
!
      end module SPH_analyzer_back_trans

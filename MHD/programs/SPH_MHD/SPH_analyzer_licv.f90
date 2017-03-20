!
!     module SPH_analyzer_licv
!
!!      subroutine SPH_initialize_linear_conv(iphys, MHD_step)
!!        type(phys_address), intent(in) :: iphys
!!      subroutine SPH_analyze_linear_conv                              &
!!     &         (i_step, iflag_finish, MHD_step)
!!        type(MHD_IO_step_param), intent(inout) :: MHD_step
!
!      Written by H. Matsui
!
      module SPH_analyzer_licv
!
      use m_precision
      use m_MHD_step_parameter
      use m_SGS_control_parameter
      use m_t_step_parameter
      use t_phys_address
      use t_MHD_step_parameter
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_initialize_linear_conv(iphys, MHD_step)
!
      use calypso_mpi
      use m_constants
      use m_array_for_send_recv
      use m_machine_parameter
!
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_fdm_coefs
      use m_rms_4_sph_spectr
      use m_sph_trans_arrays_MHD
      use m_boundary_params_sph_MHD
      use m_rms_4_sph_spectr
!
      use set_control_sph_mhd
      use set_sph_phys_address
      use const_fdm_coefs
      use set_initial_sph_dynamo
      use adjust_reference_fields
      use set_bc_sph_mhd
      use adjust_reference_fields
      use material_property
      use init_sphrical_transform_MHD
      use init_radial_infos_sph_mhd
      use const_radial_mat_4_sph
      use sph_mhd_rms_IO
      use cal_sol_sph_MHD_crank
      use cal_nonlinear
      use check_dependency_for_MHD
      use output_viz_file_control
!
      use m_work_time
!
      type(phys_address), intent(in) :: iphys
      type(MHD_IO_step_param), intent(inout) :: MHD_step
!
!   Allocate spectr field data
!
      call set_sph_MHD_sprctr_data                                      &
     &   (SGS_par1%model_p, sph1%sph_rj, ipol, idpdr, itor, rj_fld1)
!
!
      if (iflag_debug.gt.0 ) write(*,*) 'allocate_vector_for_solver'
      call allocate_vector_for_solver(isix, sph1%sph_rtp%nnod_rtp)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_r_infos_sph_mhd_evo'
      call init_r_infos_sph_mhd_evo(sph_grps1, ipol, sph1,              &
     &    omega_sph1, ref_temp1, ref_comp1, r_2nd, rj_fld1)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_sph_transform_MHD'
      call init_sph_transform_MHD                                       &
     &   (SGS_par1%model_p, ipol, idpdr, itor, iphys,                   &
     &    sph1, comms_sph1, omega_sph1, trans_p1, trns_WK1, rj_fld1)
!
! ---------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' sph_initial_data_control'
      call sph_initial_data_control                                     &
     &   (sph1%sph_params, sph1%sph_rj, ref_temp1%t_rj,                 &
     &    ipol, idpdr, itor, rj_fld1, MHD_step%rst_step, time_d1)
!
      if(iflag_debug.gt.0) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph                                    &
     &   (ref_param_T1, ref_param_C1, ref_temp1, ref_comp1,             &
     &    sph1%sph_rj, ipol, idpdr, rj_fld1)
!
!  -------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' const_radial_mat_sph_mhd'
      call const_radial_mat_sph_mhd                                     &
     &   (time_d1%dt, fl_prop1, cd_prop1, ht_prop1, cp_prop1,           &
     &    sph1%sph_rj, r_2nd, trans_p1%leg)
!*
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start                                       &
     &   (sph1%sph_rj, r_2nd, trans_p1%leg, ipol, itor, rj_fld1)
!
!*  ----------------lead nonlinear term ... ----------
!*
      if(iflag_debug .gt. 0) write(*,*) 'first licv_exp'
      call licv_exp                                                     &
     &   (ref_temp1, ref_comp1, sph1%sph_rlm, sph1%sph_rj,              &
     &    comms_sph1%comm_rlm, comms_sph1%comm_rj, omega_sph1,          &
     &    trans_p1%leg, trns_WK1%trns_MHD, ipol, itor, rj_fld1)
!
!* -----  Open Volume integration data files -----------------
!*
      if(iflag_debug .gt. 0) write(*,*) 'open_sph_vol_rms_file_mhd'
      call open_sph_vol_rms_file_mhd                                    &
     &   (sph1%sph_params, sph1%sph_rj, ipol, rj_fld1, pwr1, WK_pwr)
!
      end subroutine SPH_initialize_linear_conv
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_linear_conv                                &
     &         (i_step, iflag_finish, MHD_step)
!
      use m_work_time
      use m_t_step_parameter
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_fdm_coefs
      use m_sph_trans_arrays_MHD
      use m_rms_4_sph_spectr
!
      use cal_momentum_eq_explicit
      use cal_sol_sph_MHD_crank
      use adjust_reference_fields
      use lead_fields_4_sph_mhd
      use sph_mhd_rst_IO_control
      use sph_mhd_rms_IO
      use output_viz_file_control
      use cal_nonlinear
!
      integer(kind = kint), intent(in) :: i_step
      integer(kind = kint), intent(inout) :: iflag_finish
      type(MHD_IO_step_param), intent(inout) :: MHD_step
!
      integer(kind = kint) :: iflag
!
!*  ----------  add time evolution -----------------
!*
      if(i_step .eq. 1) then
        if(iflag_debug.gt.0) write(*,*) 'cal_expricit_sph_euler'
        call cal_expricit_sph_euler(i_step, time_d1%dt, sph1%sph_rj,    &
     &      fl_prop1, cd_prop1, ht_prop1, cp_prop1,                     &
     &      ipol, itor, rj_fld1)
      else
        if(iflag_debug.gt.0) write(*,*) 'cal_expricit_sph_adams'
        call cal_expricit_sph_adams(time_d1%dt, sph1%sph_rj,            &
     &     fl_prop1, cd_prop1, ht_prop1, cp_prop1, ipol, itor, rj_fld1)
      end if
!*
!*  ----------  time evolution by inplicit method ----------
!*
      call s_cal_sol_sph_MHD_crank                                      &
     &   (time_d1%dt, sph1%sph_rj, r_2nd, cd_prop1, ht_prop1, cp_prop1, &
     &    trans_p1%leg, ipol, idpdr, itor, rj_fld1)
!*
!* ----  Update fields after time evolution ------------------------
!*
!
      call start_eleps_time(9)
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph                                   &
     &   (ref_param_T1, ref_param_C1, ref_temp1, ref_comp1,             &
     &    sph1%sph_rj, ipol, idpdr, rj_fld1)
!*
      iflag = lead_field_data_flag(i_step, MHD_step,                    &
     &                             SGS_par1%sgs_step)
      if(iflag .eq. 0) then
        if(iflag_debug.gt.0) write(*,*) 's_lead_fields_4_sph_mhd'
        call s_lead_fields_4_sph_mhd(SGS_par1%model_p, sph1,            &
     &      comms_sph1, r_2nd, fl_prop1, trans_p1,                      &
     &      ipol, rj_fld1, trns_WK1)
      end if
      call end_eleps_time(9)
!
!*  ----------------lead nonlinear term ... ----------
!*
        call licv_exp                                                   &
     &     (ref_temp1, ref_comp1, sph1%sph_rlm, sph1%sph_rj,            &
     &      comms_sph1%comm_rlm, comms_sph1%comm_rj, omega_sph1,        &
     &      trans_p1%leg, trns_WK1%trns_MHD, ipol, itor, rj_fld1)
!
!*  -----------  output restart data --------------
!*
      call start_eleps_time(4)
      call start_eleps_time(10)
      if(iflag_debug.gt.0) write(*,*) 'output_sph_restart_control'
      call output_sph_restart_control                                   &
     &   (time_d1, rj_fld1, MHD_step%rst_step)
!
      total_time = MPI_WTIME() - total_start
      if(finish_d1%i_end_step .eq. -1                                   &
     &   .and. total_time .gt. finish_d1%elapsed_time) then
        call output_sph_rst_by_elaps(time_d1, rj_fld1)
        iflag_finish = 1
      end if
      call end_eleps_time(10)
!
!*  -----------  lead energy data --------------
!*
      call start_eleps_time(11)
      iflag = output_IO_flag(i_step, MHD_step%rms_step)
      if(iflag .eq. 0) then
        if(iflag_debug.gt.0)  write(*,*) 'output_rms_sph_mhd_control'
        call output_rms_sph_mhd_control                                 &
     &     (time_d1%i_time_step, time_d1%time,                          &
     &      sph1%sph_params, sph1%sph_rj, trans_p1%leg,                 &
     &      ipol, rj_fld1, pwr1, WK_pwr)
      end if
      call end_eleps_time(11)
!
      if(iflag_debug.gt.0) write(*,*) 'sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph                                    &
     &   (ref_param_T1, ref_param_C1, ref_temp1, ref_comp1,             &
     &    sph1%sph_rj, ipol, idpdr, rj_fld1)
      call end_eleps_time(4)
!
      if(i_step .ge. finish_d1%i_end_step                               &
     &     .and. finish_d1%i_end_step .gt. 0) then
        iflag_finish = 1
      end if
!
      end subroutine SPH_analyze_linear_conv
!
! ----------------------------------------------------------------------
!
!      subroutine SPH_finalize_licv
!
!
!      end subroutine SPH_finalize_licv
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_licv

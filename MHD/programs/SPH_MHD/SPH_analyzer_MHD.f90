!>@file   SPH_analyzer_MHD
!!@brief  module SPH_analyzer_MHD
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evolution loop for spherical MHD
!!
!!@verbatim
!!      subroutine SPH_initialize_MHD(iphys)
!!        type(phys_address), intent(in) :: iphys
!!      subroutine SPH_analyze_MHD(i_step, iflag_finish)
!!@endverbatim
!
      module SPH_analyzer_MHD
!
      use m_precision
      use t_phys_address
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_initialize_MHD(iphys)
!
      use m_constants
      use calypso_mpi
      use m_machine_parameter
      use m_control_parameter
!
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_fdm_coefs
      use m_rms_4_sph_spectr
      use m_sph_trans_arrays_MHD
      use m_boundary_params_sph_MHD
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
      use sph_filtering
      use check_dependency_for_MHD
!
      use m_work_time
!
      type(phys_address), intent(in) :: iphys
!
!   Allocate spectr field data
!
      call set_sph_MHD_sprctr_data                                      &
     &   (sph1%sph_rj, ipol, idpdr, itor, rj_fld1)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_r_infos_sph_mhd_evo'
      call init_r_infos_sph_mhd_evo(sph_grps1, ipol, sph1,              &
     &    omega_sph1, ref_temp1, r_2nd, rj_fld1)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_sph_transform_MHD'
      call init_sph_transform_MHD(ipol, idpdr, itor, iphys,             &
     &    sph1, comms_sph1, omega_sph1, trans_p1, trns_WK1, rj_fld1)
!
!  -------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' sph_initial_data_control'
      call sph_initial_data_control                                     &
     &   (sph1%sph_params, sph1%sph_rj, ref_temp1%t_rj,                 &
     &    ipol, idpdr, itor, rj_fld1)
!
      if(iflag_debug.gt.0) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph                                    &
     &   (ref_temp1%t_rj, sph1%sph_rj, ipol, idpdr, rj_fld1)
!
!  -------------------------------
!
      if(iflag_SGS_model .gt. 0) then
        if(iflag_debug.gt.0) write(*,*)' init_SGS_model_sph_mhd'
        call init_SGS_model_sph_mhd                                     &
     &     (sph1, sph_grps1, trns_WK1%dynamic_SPH)
      end if
!
!  -------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' const_radial_mat_sph_mhd'
      call const_radial_mat_sph_mhd(sph1%sph_rj, r_2nd, trans_p1%leg)
!*
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start                                       &
     &   (sph1%sph_rj, r_2nd, trans_p1%leg, ipol, itor, rj_fld1)
!
!* obtain nonlinear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'first nonlinear'
      call nonlinear(sph1, comms_sph1, omega_sph1, r_2nd, trans_p1,     &
     &    ref_temp1%t_rj, ipol, itor, trns_WK1, rj_fld1)
!
!* -----  Open Volume integration data files -----------------
!*
      if(iflag_debug .gt. 0) write(*,*) 'open_sph_vol_rms_file_mhd'
      call start_eleps_time(4)
      call open_sph_vol_rms_file_mhd                                    &
     &   (sph1%sph_params, sph1%sph_rj, ipol, rj_fld1, pwr1, WK_pwr)
      call end_eleps_time(4)
!
      end subroutine SPH_initialize_MHD
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_MHD(i_step, iflag_finish)
!
      use m_work_time
      use m_spheric_parameter
      use m_sph_spectr_data
      use m_fdm_coefs
      use m_t_step_parameter
      use m_sph_trans_arrays_MHD
      use m_rms_4_sph_spectr
!
      use cal_momentum_eq_explicit
      use cal_sol_sph_MHD_crank
      use cal_nonlinear
      use adjust_reference_fields
      use lead_fields_4_sph_mhd
      use sph_mhd_rst_IO_control
      use sph_mhd_rms_IO
!
      integer(kind = kint), intent(in) :: i_step
!
      integer(kind = kint), intent(inout) :: iflag_finish
!
      real(kind = kreal) :: total_max
!
!*  ----------  add time evolution -----------------
!*
!
      call start_eleps_time(5)
      call start_eleps_time(6)
      if(i_step .eq. 1) then
        if(iflag_debug.gt.0) write(*,*) 'cal_expricit_sph_euler'
        call cal_expricit_sph_euler                                     &
     &     (i_step, sph1%sph_rj, ipol, itor, rj_fld1)
      else
        if(iflag_debug.gt.0) write(*,*) 'cal_expricit_sph_adams'
        call cal_expricit_sph_adams(sph1%sph_rj, ipol, itor, rj_fld1)
      end if
!*
!*  ----------  time evolution by inplicit method ----------
!*
      call start_eleps_time(7)
      call s_cal_sol_sph_MHD_crank                                      &
     &   (sph1%sph_rj, r_2nd, trans_p1%leg, ipol, idpdr, itor, rj_fld1)
      call end_eleps_time(7)
      call end_eleps_time(6)
!*
!*  ----------------lead nonlinear term ... ----------
!*
      call start_eleps_time(8)
      call nonlinear(sph1, comms_sph1, omega_sph1, r_2nd, trans_p1,     &
     &    ref_temp1%t_rj, ipol, itor, trns_WK1, rj_fld1)
      call end_eleps_time(8)
      call end_eleps_time(5)
!
!* ----  Update fields after time evolution ------------------------=
!*
      call start_eleps_time(9)
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph                                   &
     &   (ref_temp1%t_rj, sph1%sph_rj, ipol, idpdr, rj_fld1)
!*
      if(iflag_debug.gt.0) write(*,*) 's_lead_fields_4_sph_mhd'
      call s_lead_fields_4_sph_mhd                                      &
     &   (sph1, comms_sph1, r_2nd, trans_p1, ipol, rj_fld1, trns_WK1)
      call end_eleps_time(9)
!
!*  -----------  output restart data --------------
!*
      call start_eleps_time(4)
      call start_eleps_time(10)
      if(iflag_debug.gt.0) write(*,*) 'output_sph_restart_control'
      call output_sph_restart_control(rj_fld1)
!
      total_time = MPI_WTIME() - total_start
      call MPI_allREDUCE (total_time, total_max, ione, CALYPSO_REAL,    &
     &    MPI_MAX, CALYPSO_COMM, ierr_MPI)
      if      (istep_rst_end .eq. -1                                    &
     &   .and. total_max.gt.elapsed_time) then
        call output_sph_rst_by_elaps(rj_fld1)
        iflag_finish = 1
      end if
      call end_eleps_time(10)
!
!*  -----------  lead energy data --------------
!*
      call start_eleps_time(11)
      if(iflag_debug.gt.0)  write(*,*) 'output_rms_sph_mhd_control'
      call output_rms_sph_mhd_control(sph1%sph_params, sph1%sph_rj,     &
     &    trans_p1%leg, ipol, rj_fld1, pwr1, WK_pwr)
      call end_eleps_time(11)
!
      if(iflag_debug.gt.0) write(*,*) 'sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph                                    &
     &   (ref_temp1%t_rj, sph1%sph_rj, ipol, idpdr, rj_fld1)
!
      if(i_step .ge. i_step_number .and. i_step_number.gt.0) then
        iflag_finish = 1
      end if
      call end_eleps_time(4)
!
      end subroutine SPH_analyze_MHD
!
! ----------------------------------------------------------------------
!
!      subroutine SPH_finalize_MHD
!
!      end subroutine SPH_finalize_MHD
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_MHD

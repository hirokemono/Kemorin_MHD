!>@file   SPH_analyzer_SGS_MHD
!!@brief  module SPH_analyzer_SGS_MHD
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Evolution loop for spherical MHD
!!
!!@verbatim
!!      subroutine SPH_initialize_SGS_MHD                               &
!!     &         (MHD_files, iphys, MHD_step, sph_fst_IO, SPH_model,    &
!!     &          SPH_SGS, SPH_MHD, SPH_WK)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(phys_address), intent(in) :: iphys
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(SPH_MHD_model_data), intent(inout) :: SPH_model
!!        type(SPH_SGS_structure), intent(inout) :: SPH_SGS
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(work_SPH_MHD), intent(inout) :: SPH_WK
!!        type(field_IO), intent(inout) :: sph_fst_IO
!!      subroutine SPH_analyze_SGS_MHD                                  &
!!     &         (i_step, MHD_files, iflag_finish, SPH_model,           &
!!     &          MHD_step, sph_fst_IO, SPH_SGS, SPH_MHD, SPH_WK)
!!        type(MHD_file_IO_params), intent(in) :: MHD_files
!!        type(SGS_paremeters), intent(in) :: SGS_par
!!        type(SPH_MHD_model_data), intent(inout) :: SPH_model
!!        type(MHD_step_param), intent(inout) :: MHD_step
!!        type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
!!        type(work_SPH_MHD), intent(inout) :: SPH_WK
!!        type(field_IO), intent(inout) :: sph_fst_IO
!!@endverbatim
!
      module SPH_analyzer_SGS_MHD
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use m_MHD_step_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
!
      use calypso_mpi
!
      use t_MHD_step_parameter
      use t_MHD_file_parameter
      use t_SPH_MHD_model_data
      use t_SPH_mesh_field_data
      use t_SPH_SGS_structure
      use t_boundary_data_sph_MHD
      use t_work_SPH_MHD
      use t_field_data_IO
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine SPH_initialize_SGS_MHD                                 &
     &         (MHD_files, iphys, MHD_step, sph_fst_IO, SPH_model,      &
     &          SPH_SGS, SPH_MHD, SPH_WK)
!
      use t_sph_boundary_input_data
!
      use set_control_sph_mhd
      use set_initial_sph_dynamo
      use adjust_reference_fields
      use set_bc_sph_mhd
      use adjust_reference_fields
      use material_property
      use cal_SGS_nonlinear
      use init_sph_trans_SGS_MHD
      use init_radial_infos_sph_mhd
      use const_radial_mat_4_sph
      use sph_SGS_MHD_rst_IO_control
      use SPH_SGS_ini_model_coefs_IO
      use cal_sol_sph_MHD_crank
      use cal_SGS_nonlinear
      use sph_filtering
      use check_dependency_SGS_MHD
      use input_control_sph_MHD
      use sph_SGS_mhd_monitor_data_IO
      use self_buoyancy_w_filter_sph
!
      type(MHD_file_IO_params), intent(in) :: MHD_files
      type(phys_address), intent(in) :: iphys
!
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(SPH_SGS_structure), intent(inout) :: SPH_SGS
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(work_SPH_MHD), intent(inout) :: SPH_WK
      type(field_IO), intent(inout) :: sph_fst_IO
!
!
!   Allocate spectr field data
!
      call set_sph_SGS_MHD_spectr_data                                  &
     &   (SPH_SGS%SGS_par, SPH_model%MHD_prop,                          &
     &    SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%fld)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_r_infos_sph_mhd_evo'
      call init_r_infos_sph_mhd_evo(SPH_model, SPH_WK%r_2nd, SPH_MHD)
!
! ---------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_sph_transform_SGS_MHD'
      call init_sph_transform_SGS_MHD(SPH_SGS%SGS_par%model_p,          &
     &    SPH_model, iphys, SPH_WK%trans_p, SPH_WK%trns_WK, SPH_MHD)
!
!  -------------------------------
!
      if (iflag_debug.gt.0) write(*,*) 'init_SGS_model_sph_mhd'
      call init_SGS_model_sph_mhd                                       &
     &   (SPH_SGS%SGS_par, SPH_MHD%sph, SPH_MHD%groups,                 &
     &    SPH_model%MHD_prop, SPH_WK%trans_p, SPH_SGS%dynamic)
      call calypso_mpi_barrier
!
!  -------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' sph_initial_data_control'
      call sph_initial_data_control                                     &
     &   (MHD_files, SPH_model, SPH_MHD, MHD_step, sph_fst_IO)
      call set_initial_Csim_control                                     &
     &   (MHD_files, MHD_step, SPH_SGS%SGS_par, SPH_SGS%dynamic)
      MHD_step%iflag_initial_step = 0
!
      if(iflag_debug.gt.0) write(*,*)' sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph(SPH_model,                         &
     &    SPH_MHD%sph%sph_rj, SPH_MHD%ipol, SPH_MHD%fld)
!
!  -------------------------------
!
      if(iflag_debug.gt.0) write(*,*)' const_radial_mat_sph_mhd'
      call const_radial_mat_sph_mhd                                     &
     &   (MHD_step%time_d%dt, SPH_model%MHD_prop,                       &
     &    SPH_model%sph_MHD_bc, SPH_MHD%sph%sph_rj, SPH_WK%r_2nd,       &
     &    SPH_WK%trans_p%leg, SPH_WK%MHD_mats)
!*
!* obtain linear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_MHD_evolved_boundaries'
      call set_MHD_evolved_boundaries(MHD_step%time_d, SPH_MHD%sph,     &
     &    SPH_model%MHD_prop, SPH_model%sph_MHD_bc)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_field_to_start'
      call set_sph_field_to_start                                       &
     &   (SPH_MHD%sph%sph_rj, SPH_WK%r_2nd, SPH_model%MHD_prop,         &
     &    SPH_model%sph_MHD_bc, SPH_WK%trans_p%leg,                     &
     &    SPH_MHD%ipol, SPH_MHD%fld)
      if(iflag_debug .gt. 0) write(*,*) 'rot_self_filter_buoyancy_sph'
      call rot_self_filter_buoyancy_sph                                 &
     &   (SPH_MHD%sph%sph_rj, SPH_MHD%ipol, SPH_model%MHD_prop,         &
     &    SPH_model%sph_MHD_bc%sph_bc_U, SPH_MHD%fld)
!
!* obtain nonlinear terms for starting
!*
      if(iflag_debug .gt. 0) write(*,*) 'first nonlinear'
      call nonlinear_SGS_first                                          &
     &   (MHD_step%init_d%i_time_step, SPH_WK%r_2nd, SPH_model,         &
     &    SPH_WK%trans_p, SPH_WK%trns_WK, SPH_SGS%SGS_par,              &
     &    SPH_SGS%dynamic, SPH_MHD)
!
!* -----  Open Volume integration data files -----------------
!*
      if(iflag_debug .gt. 0) write(*,*) 'open_sph_vol_rms_file_SGS_mhd'
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      call open_sph_vol_rms_file_SGS_mhd                                &
     &   (SPH_MHD%sph, SPH_MHD%ipol, SPH_MHD%fld, SPH_WK%monitor)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
      call calypso_mpi_barrier
!
      end subroutine SPH_initialize_SGS_MHD
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine SPH_analyze_SGS_MHD                                    &
     &         (i_step, MHD_files, iflag_finish, SPH_model,             &
     &          MHD_step, sph_fst_IO, SPH_SGS, SPH_MHD, SPH_WK)
!
      use momentum_w_SGS_explicit
      use cal_sol_sph_MHD_crank
      use cal_SGS_nonlinear
      use adjust_reference_fields
      use lead_fields_SPH_SGS_MHD
      use sph_SGS_MHD_rst_IO_control
      use output_viz_file_control
      use sph_SGS_mhd_monitor_data_IO
      use self_buoyancy_w_filter_sph
!
      integer(kind = kint), intent(in) :: i_step
      type(MHD_file_IO_params), intent(in) :: MHD_files
!
      integer(kind = kint), intent(inout) :: iflag_finish
      type(SPH_MHD_model_data), intent(inout) :: SPH_model
      type(MHD_step_param), intent(inout) :: MHD_step
      type(SPH_SGS_structure), intent(inout) :: SPH_SGS
      type(SPH_mesh_field_data), intent(inout) :: SPH_MHD
      type(work_SPH_MHD), intent(inout) :: SPH_WK
      type(field_IO), intent(inout) :: sph_fst_IO
!
      integer(kind = kint) :: iflag
!
!*  ----------  add time evolution -----------------
!*
!
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+1)
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+2)
      if(iflag_debug.gt.0) write(*,*) 'sel_explicit_sph_SGS_MHD'
      call sel_explicit_sph_SGS_MHD                                     &
     &   (i_step, MHD_step%time_d%dt, SPH_SGS%SGS_par%model_p,          &
     &    SPH_model%MHD_prop, SPH_model%sph_MHD_bc, SPH_MHD)
!*
!*  ----------  time evolution by inplicit method ----------
!*
      if(iflag_debug .gt. 0) write(*,*) 'set_MHD_evolved_boundaries'
      call set_MHD_evolved_boundaries(MHD_step%time_d, SPH_MHD%sph,     &
     &    SPH_model%MHD_prop, SPH_model%sph_MHD_bc)
!
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+3)
      call s_cal_sol_sph_MHD_crank                                      &
     &   (MHD_step%time_d%dt, SPH_MHD%sph%sph_rj, SPH_WK%r_2nd,         &
     &    SPH_model%MHD_prop, SPH_model%sph_MHD_bc, SPH_WK%trans_p%leg, &
     &    SPH_MHD%ipol, SPH_WK%MHD_mats, SPH_MHD%fld)
      if(iflag_debug .gt. 0) write(*,*) 'rot_self_filter_buoyancy_sph'
      call rot_self_filter_buoyancy_sph                                 &
     &   (SPH_MHD%sph%sph_rj, SPH_MHD%ipol, SPH_model%MHD_prop,         &
     &    SPH_model%sph_MHD_bc%sph_bc_U, SPH_MHD%fld)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+3)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+2)
!*
!*  ----------------lead nonlinear term ... ----------
!*
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+4)
      call nonlinear_with_SGS                                           &
     &   (i_step, SPH_SGS%SGS_par, SPH_WK%r_2nd, SPH_model,             &
     &    SPH_WK%trans_p, SPH_WK%trns_WK, SPH_SGS%dynamic, SPH_MHD)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+4)
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+1)
!
!* ----  Update fields after time evolution ------------------------=
!*
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+5)
      if(iflag_debug.gt.0) write(*,*) 'trans_per_temp_to_temp_sph'
      call trans_per_temp_to_temp_sph(SPH_model,                        &
     &    SPH_MHD%sph%sph_rj, SPH_MHD%ipol, SPH_MHD%fld)
!*
      if(lead_field_data_flag(i_step, MHD_step) .eq. 0) then
        if(iflag_debug.gt.0) write(*,*) 'lead_fields_4_SPH_SGS_MHD'
        call lead_fields_4_SPH_SGS_MHD(SPH_SGS%SGS_par,                 &
     &      SPH_WK%monitor, SPH_WK%r_2nd, SPH_model%MHD_prop,           &
     &      SPH_model%sph_MHD_bc, SPH_WK%trans_p, SPH_WK%MHD_mats,      &
     &      SPH_WK%trns_WK, SPH_SGS%dynamic, SPH_MHD)
      end if
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+5)
!
!*  -----------  output restart data --------------
!*
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+6)
      iflag = output_IO_flag(MHD_step%time_d%i_time_step,               &
     &                         MHD_step%rst_step)
      if(iflag .eq. 0) then
        if(iflag_debug.gt.0) write(*,*)                                 &
     &     'output_sph_SGS_MHD_rst_control'
        call output_sph_SGS_MHD_rst_control                             &
     &     (MHD_step%time_d%i_time_step, MHD_files,                     &
     &      MHD_step%time_d, SPH_MHD%fld, MHD_step%rst_step,            &
     &      SPH_SGS%SGS_par%i_step_sgs_coefs, SPH_SGS%SGS_par%model_p,  &
     &      SPH_SGS%dynamic, sph_fst_IO)
      end if
!
      MHD_step%finish_d%elapsed_local                                   &
     &    = MPI_WTIME() - MHD_step%finish_d%started_time
      call MPI_allREDUCE(MHD_step%finish_d%elapsed_local,               &
     &    MHD_step%finish_d%elapsed_max, 1, CALYPSO_REAL,               &
     &    MPI_MAX, CALYPSO_COMM, ierr_MPI)
      if      (MHD_step%finish_d%i_end_step .eq. -1                     &
     &   .and. MHD_step%finish_d%elapsed_max                            &
     &        .gt. MHD_step%finish_d%elapsed_time) then
        MHD_step%rst_step%istep_file = MHD_step%finish_d%i_end_step
        iflag_finish = 1
        call output_sph_SGS_MHD_rst_control                             &
     &     (MHD_step%finish_d%i_end_step, MHD_files,                    &
     &      MHD_step%time_d, SPH_MHD%fld, MHD_step%rst_step,            &
     &      SPH_SGS%SGS_par%i_step_sgs_coefs, SPH_SGS%SGS_par%model_p,  &
     &      SPH_SGS%dynamic, sph_fst_IO)
      end if
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+6)
!
!*  -----------  lead energy data --------------
!*
      if(iflag_SMHD_time) call start_elapsed_time(ist_elapsed_SMHD+7)
      iflag = output_IO_flag(i_step, MHD_step%rms_step)
      if(iflag .eq. 0) then
        if(iflag_debug .gt. 0)                                          &
     &                write(*,*) 'output_rms_sph_SGS_mhd_control'
        call output_rms_sph_SGS_mhd_control(MHD_step%time_d, SPH_MHD,   &
     &      SPH_model%sph_MHD_bc, SPH_WK%trans_p%leg, SPH_WK%monitor)
      end if
      if(iflag_SMHD_time) call end_elapsed_time(ist_elapsed_SMHD+7)
!
      if(iflag_debug.gt.0) write(*,*) 'sync_temp_by_per_temp_sph'
      call sync_temp_by_per_temp_sph(SPH_model,                         &
     &    SPH_MHD%sph%sph_rj, SPH_MHD%ipol, SPH_MHD%fld)
!
      if(i_step .ge. MHD_step%finish_d%i_end_step                       &
     &    .and. MHD_step%finish_d%i_end_step .gt. 0) then
        iflag_finish = 1
      end if
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
      end subroutine SPH_analyze_SGS_MHD
!
! ----------------------------------------------------------------------
!
!      subroutine SPH_finalize_MHD
!
!      end subroutine SPH_finalize_MHD
!
! ----------------------------------------------------------------------
!
      end module SPH_analyzer_SGS_MHD

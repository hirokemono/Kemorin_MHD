!>@file   analyzer_sph_all_rms_ratio.f90
!!@brief  module analyzer_sph_all_rms_ratio
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to evaluate snapshots from spectr data
!!
!!@verbatim
!!      subroutine initialize_sph_all_rms_ratio
!!      subroutine evolution_sph_all_rms_ratio
!!@endverbatim
!
      module analyzer_sph_all_rms_ratio
!
      use m_precision
      use calypso_mpi
!
      use m_machine_parameter
      use m_work_time
      use m_elapsed_labels_4_MHD
      use m_elapsed_labels_SEND_RECV
      use t_spherical_MHD
      use t_sph_SGS_MHD
      use t_ctl_data_MHD
      use t_ctl_data_SGS_MHD
      use t_control_data_tracers
      use t_control_data_dynamo_vizs
!
      implicit none
!
!>      Structure of the all data of program
      type(spherical_MHD), save, private :: SNAPs
!>      Structure for visualization in spherical MHD
      type(sph_SGS_MHD), save, private :: SVIZ_m
!
!>      Control struture for MHD simulation
      type(mhd_simulation_control), save, private :: MHD_ctl_m
!>        Control structures for tracer modules
      type(tracers_control), save, private :: tracer_ctls_m
!>        Control structures for visualization modules
      type(visualization_controls), save, private ::  viz_ctls_m
!>        Control structures for zonal mean and trancated magnetic field
      type(sph_dynamo_viz_controls), save, private :: zm_ctls_m
!
       private :: set_ctl_4_second_spectr_data
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_all_rms_ratio(control_file_name)
!
      use init_sph_MHD_elapsed_label
      use input_control_sph_SGS_MHD
      use SPH_analyzer_back_trans
      use SPH_analyzer_rms_ratio_all
      use FEM_analyzer_sph_SGS_MHD
      use FEM_to_VIZ_bridge
!
      character(len=kchara), intent(in) :: control_file_name
!
      write(*,*) 'Simulation start: PE. ', my_rank
      call init_elapse_time_by_TOTAL
      call set_sph_MHD_elapsed_label
      call elpsed_label_field_send_recv
!
!   Load parameter file
!
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_SGS_dynamo'
      call input_control_SPH_SGS_dynamo                                 &
     &   (control_file_name, SNAPs%MHD_files, MHD_ctl_m, tracer_ctls_m, &
     &    viz_ctls_m, zm_ctls_m, SNAPs%MHD_step, SNAPs%SPH_model,       &
     &    SNAPs%SPH_WK, SVIZ_m%SPH_SGS, SNAPs%SPH_MHD, SVIZ_m%FEM_DAT)
      call dealloc_tracer_controls(tracer_ctls_m)
      call dealloc_dynamo_viz_control(zm_ctls_m)
      call set_ctl_4_second_spectr_data                                 &
     &   (MHD_ctl_m%new_plt, sph_file_param2)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!     --------------------- 
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_sph_SGS_MHD'
      call FEM_initialize_sph_SGS_MHD(SNAPs%MHD_files, SNAPs%MHD_step,  &
     &    SVIZ_m%SPH_SGS%iphys_LES, SNAPs%MHD_IO, SVIZ_m%FEM_DAT,       &
     &    SNAPs%m_SR)
!
!        Initialize spherical transform dynamo
      if(iflag_debug .gt. 0) write(*,*) 'SPH_init_sph_back_trans'
      call SPH_init_sph_back_trans                                      &
     &   (SNAPs%MHD_files, SNAPs%SPH_model, SNAPs%MHD_step,             &
     &    SVIZ_m%SPH_SGS, SNAPs%SPH_MHD, SNAPs%SPH_WK, SNAPs%m_SR)
!
!  -------------------------------------------
!  ----   Mesh setting for visualization -----
!  -------------------------------------------
      if(iflag_debug .gt. 0) write(*,*) 'init_FEM_to_VIZ_bridge'
      call init_FEM_to_VIZ_bridge(SNAPs%MHD_step%viz_step,              &
     &    SVIZ_m%FEM_DAT%geofem, SVIZ_m%VIZ_FEM, SNAPs%m_SR)
!
!        Initialize visualization
      if(iflag_debug .gt. 0) write(*,*) 'init_visualize'
      call init_visualize                                               &
     &   (SNAPs%MHD_step%viz_step, SVIZ_m%FEM_DAT%geofem,               &
     &    SVIZ_m%FEM_DAT%field, SVIZ_m%VIZ_FEM,                         &
     &    viz_ctls_m, SVIZ_m%VIZs, SNAPs%m_SR)
      call dealloc_viz_controls(viz_ctls_m)
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+1)
      call calypso_MPI_barrier
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_all_rms_ratio
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_all_rms_ratio
!
      use copy_all_fields_4_sph_trans
!
      use SPH_analyzer_back_trans
      use SPH_analyzer_rms_ratio_all
      use FEM_analyzer_sph_SGS_MHD
      use FEM_to_VIZ_bridge
      use set_time_step_params
!
!*  -----------  set initial step data --------------
!*
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+2)
      call set_from_initial_step(SNAPs%MHD_step%init_d,                 &
     &                           SNAPs%MHD_step%time_d)
!*
!*  -------  time evelution loop start -----------
!*
      do
        call add_one_step(SNAPs%MHD_step%time_d)
        if(output_IO_flag(SNAPs%MHD_step%time_d%i_time_step,            &
     &                    SNAPs%MHD_step%rst_step) .eqv. .FALSE.) cycle
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_rms_ratio_all'
        call SPH_analyze_rms_ratio_all                                  &
     &     (SNAPs%MHD_step%time_d, SNAPs%MHD_files, SVIZ_m%SPH_SGS,     &
     &      SNAPs%MHD_step, SNAPs%SPH_MHD, SNAPs%SPH_WK, SNAPs%m_SR)
!*
!*  -----------  output field data --------------
!*
        if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
!
        if (iflag_debug.gt.0) write(*,*) 'copy_all_field_from_trans'
        call copy_all_field_from_trans                                  &
     &     (SNAPs%SPH_MHD%sph%sph_params%m_folding,                     &
     &      SNAPs%SPH_MHD%sph%sph_rtp,                                  &
     &      SNAPs%SPH_WK%trns_WK%trns_MHD%backward,                     &
     &      SVIZ_m%FEM_DAT%geofem%mesh, SVIZ_m%FEM_DAT%field)
!
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_SGS_MHD'
        call FEM_analyze_sph_SGS_MHD(SNAPs%MHD_files, SNAPs%MHD_step,   &
     &      SNAPs%MHD_IO, SVIZ_m%FEM_DAT, SNAPs%m_SR)
!
        if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!*  ----------- Visualization --------------
!*
        if(iflag_vizs_w_fix_step(SNAPs%MHD_step%time_d%i_time_step,     &
     &                           SNAPs%MHD_step%viz_step)) then
          if (iflag_debug.eq.1) write(*,*) 'visualize_all'
          if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+4)
          call istep_viz_w_fix_dt(SNAPs%MHD_step%time_d%i_time_step,    &
     &                          SNAPs%MHD_step%viz_step)
          call visualize_all                                            &
     &       (SNAPs%MHD_step%viz_step, SNAPs%MHD_step%time_d,           &
     &        SVIZ_m%FEM_DAT%geofem, SVIZ_m%FEM_DAT%field,              &
     &        SVIZ_m%VIZ_FEM, SVIZ_m%VIZs, SNAPs%m_SR)
          if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+4)
        end if
!
!*  -----------  exit loop --------------
!*
        if(SNAPs%MHD_step%time_d%i_time_step                            &
     &        .ge. SNAPs%MHD_step%finish_d%i_end_step) exit
      end do
!
!  time evolution end
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+2)
!
      if (iflag_debug.eq.1) write(*,*) 'visualize_fin'
      call visualize_fin                                                &
     &   (SNAPs%MHD_step%viz_step, SNAPs%MHD_step%time_d, SVIZ_m%VIZs)
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize_sph_SGS_MHD'
      call FEM_finalize_sph_SGS_MHD(SNAPs%MHD_files, SNAPs%MHD_step,    &
     &                              SNAPs%MHD_IO)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_snap'
!      call SPH_finalize_snap
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_all_rms_ratio
!
! ----------------------------------------------------------------------
!
      subroutine set_ctl_4_second_spectr_data(new_plt, sph_file_param2)
!
      use t_file_IO_parameter
      use t_ctl_data_4_platforms
      use m_file_format_switch
      use mpi_abort_by_missing_zlib
!
      type(platform_data_control), intent(in) :: new_plt
      type(field_IO_params), intent(inout) :: sph_file_param2
!
!
      sph_file_param2%iflag_format                                      &
     &   = choose_para_file_format(new_plt%spectr_field_fmt_ctl)
!
      sph_file_param2%iflag_IO = new_plt%spectr_field_file_prefix%iflag
      if(sph_file_param2%iflag_IO .gt. 0) then
        sph_file_param2%file_prefix                                     &
     &           = new_plt%spectr_field_file_prefix%charavalue
      end if
      call s_mpi_abort_by_missing_zlib(sph_file_param2%file_prefix,     &
     &                                 sph_file_param2%iflag_format)
!
      end subroutine set_ctl_4_second_spectr_data
!
!  --------------------------------------------------------------------
!
      end module analyzer_sph_all_rms_ratio

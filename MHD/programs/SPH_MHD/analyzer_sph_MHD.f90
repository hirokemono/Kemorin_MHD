!>@file   analyzer_sph_MHD.f90
!!@brief  module analyzer_sph_MHD
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop for MHD dynamo simulation
!!
!!@verbatim
!!      subroutine initialize_sph_mhd(control_file_name)
!!      subroutine evolution_sph_mhd
!!@endverbatim
!
      module analyzer_sph_MHD
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
      use t_particle_trace
      use t_ctl_data_MHD
      use t_ctl_data_SGS_MHD
      use t_control_data_tracers
      use t_control_data_vizs
      use t_control_data_dynamo_vizs
!
      implicit none
!
!>      Control struture for MHD simulation
      type(spherical_MHD), save, private :: SSMHDs
!>      Structure for visualization in spherical MHD
      type(sph_SGS_MHD), save, private :: SVIZ_m
!
!>      Control struture for MHD simulation
      type(mhd_simulation_control), save, private :: MHD_ctl1
!
!>        Control structures for tracer modules
      type(tracers_control), save, private :: tracer_ctls1
!>        Control structures for visualization modules
      type(visualization_controls), save, private ::  viz_ctls1
!>        Control structures for zonal mean and trancated magnetic field
      type(sph_dynamo_viz_controls), save, private :: zm_ctls1
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_mhd(control_file_name)
!
      use t_visualizer
      use t_SPH_MHD_zonal_mean_viz
      use m_elapsed_labels_4_REPART
      use FEM_analyzer_sph_SGS_MHD
      use SPH_analyzer_SGS_MHD
      use FEM_to_VIZ_bridge
      use input_control_sph_SGS_MHD
      use init_sph_MHD_elapsed_label
!
      character(len=kchara), intent(in) :: control_file_name
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
      SSMHDs%MHD_step%finish_d%started_time = MPI_WTIME()
      call init_elapse_time_by_TOTAL
      call set_sph_MHD_elapsed_label
      call elpsed_label_4_repartition
!
      call elpsed_label_field_send_recv
!
!   Load parameter file
!
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      if (iflag_debug.eq.1) write(*,*) 'input_control_SPH_SGS_dynamo'
      call input_control_SPH_SGS_dynamo(control_file_name,              &
     &   SSMHDs%MHD_files, MHD_ctl1, tracer_ctls1, viz_ctls1, zm_ctls1, &
     &   SSMHDs%MHD_step, SSMHDs%SPH_model, SSMHDs%SPH_WK,              &
     &   SVIZ_m%SPH_SGS, SSMHDs%SPH_MHD, SVIZ_m%FEM_DAT)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!    IO elapsed end
!    precondition elaps start
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
!
!        Initialize FEM mesh data for field data IO
!
      if(iflag_debug .gt. 0) write(*,*) 'FEM_initialize_sph_SGS_MHD'
      call FEM_initialize_sph_SGS_MHD                                   &
     &   (SSMHDs%MHD_files, SSMHDs%MHD_step, SVIZ_m%SPH_SGS%iphys_LES,  &
     &    SSMHDs%MHD_IO, SVIZ_m%FEM_DAT, SSMHDs%m_SR)
!
!        Initialize spherical transform dynamo
!
      if(iflag_debug .gt. 0) write(*,*) 'SPH_initialize_SGS_MHD'
      call SPH_initialize_SGS_MHD                                       &
     &   (SSMHDs%MHD_files, SVIZ_m%FEM_DAT, SSMHDs%MHD_step,            &
     &    SSMHDs%MHD_IO%rst_IO, SSMHDs%SPH_model, SVIZ_m%SPH_SGS,       &
     &    SSMHDs%SPH_MHD, SSMHDs%SPH_WK, SSMHDs%m_SR)
!
!  -------------------------------------------
!  ----   Mesh setting for visualization -----
!  -------------------------------------------
      if(iflag_debug .gt. 0) write(*,*) 'init_FEM_to_VIZ_bridge'
      call init_FEM_to_VIZ_bridge                                       &
      &  (SSMHDs%MHD_step%viz_step, SVIZ_m%FEM_DAT%geofem,              &
      &   SVIZ_m%VIZ_FEM, SSMHDs%m_SR)
!
!  -----   Initialize tracer
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+11)
      call TRACER_initialize                                            &
     &   (SSMHDs%MHD_step%init_d,  SSMHDs%MHD_step%finish_d,            &
     &    SSMHDs%MHD_step%rst_step, SVIZ_m%FEM_DAT%geofem,              &
     &    SVIZ_m%VIZ_FEM%para_surf, SVIZ_m%FEM_DAT%field,               &
     &    tracer_ctls1%tracer_controls, SVIZ_m%tracers)
      call dealloc_tracer_controls(tracer_ctls1)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+11)
!
!  -----   Initialize visualization
      if(iflag_debug .gt. 0) write(*,*) 'init_visualize'
      call init_visualize(SSMHDs%MHD_step%viz_step,                     &
     &    SVIZ_m%FEM_DAT%geofem, SVIZ_m%FEM_DAT%field, SVIZ_m%VIZ_FEM,  &
     &    viz_ctls1, SVIZ_m%VIZs, SSMHDs%m_SR)
      call dealloc_viz_controls(viz_ctls1)

      call init_zonal_mean_vizs                                         &
     &   (SSMHDs%MHD_step%viz_step, SVIZ_m%FEM_DAT%geofem,              &
     &    SVIZ_m%VIZ_FEM%edge_comm, SVIZ_m%FEM_DAT%field,               &
     &    zm_ctls1, SVIZ_m%zmeans, SSMHDs%m_SR)
      call dealloc_dynamo_viz_control(zm_ctls1)
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+1)
      call reset_elapse_4_init_sph_mhd
!
      end subroutine initialize_sph_mhd
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_mhd
!
      use t_time_data
      use t_VIZ_step_parameter
      use t_sph_trans_arrays_MHD
      use t_sph_trans_arrays_SGS_MHD
      use t_visualizer
      use t_SPH_MHD_zonal_mean_viz
      use SPH_analyzer_SGS_MHD
      use SGS_MHD_zonal_mean_viz
      use FEM_analyzer_sph_SGS_MHD
      use output_viz_file_control
      use init_sph_MHD_elapsed_label
!
      integer(kind = kint) :: iflag_finish
!
!     ---------------------
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+2)
!
!*  -----------  set initial step data --------------
!*
      call copy_time_step_data(SSMHDs%MHD_step%init_d,                  &
     &                         SSMHDs%MHD_step%time_d)
      iflag_finish = 0
!*
!*  -------  time evelution loop start -----------
!*
      do
        call evolve_time_data(SSMHDs%MHD_step%time_d)
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if(lead_field_data_flag(SSMHDs%MHD_step%time_d%i_time_step,     &
     &                          SSMHDs%MHD_step)                        &
     &    .or. lead_field_data_flag(SSMHDs%MHD_step%time_d%i_time_step, &
     &                              SSMHDs%MHD_step)                    &
     &    .or. SVIZ_m%tracers%num_trace .gt. 0) then
          if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+6)
          call alloc_sph_trans_area_snap                                &
     &       (SSMHDs%SPH_MHD%sph, SSMHDs%SPH_WK%trns_WK)
          call alloc_SGS_sph_trns_area_snap                             &
     &       (SSMHDs%SPH_MHD%sph, SVIZ_m%SPH_SGS%trns_WK_LES)
          if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+6)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_SGS_MHD'
        call SPH_analyze_SGS_MHD(SSMHDs%MHD_files, iflag_finish,        &
     &      SSMHDs%SPH_model, SSMHDs%MHD_step, SSMHDs%MHD_IO%rst_IO,    &
     &      SVIZ_m%SPH_SGS, SSMHDs%SPH_MHD,                             &
     &      SSMHDs%SPH_WK, SSMHDs%m_SR)
!*
!*  -----------  Send field data to FEM mesh --------------
!*
        if(SVIZ_m%tracers%num_trace .gt. 0) then
          if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+5)
          call SPH_to_TRACER_bridge_SGS_MHD(SSMHDs%SPH_MHD%sph,         &
     &        SSMHDs%SPH_MHD%comms, SSMHDs%SPH_MHD%fld,                 &
     &        SSMHDs%SPH_WK%trans_p, SSMHDs%SPH_WK%trns_WK%trns_MHD,    &
     &        SVIZ_m%FEM_DAT%geofem, SVIZ_m%FEM_DAT%field, SSMHDs%m_SR)
          if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+5)
        end if
        if(lead_field_data_flag(SSMHDs%MHD_step%time_d%i_time_step,     &
     &                          SSMHDs%MHD_step)                        &
     &    .or. lead_field_data_flag(SSMHDs%MHD_step%time_d%i_time_step, &
     &                              SSMHDs%MHD_step)) then
          if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
          if (iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge_SGS_MHD'
          call SPH_to_FEM_bridge_SGS_MHD                                &
     &       (SVIZ_m%SPH_SGS%SGS_par, SSMHDs%SPH_MHD%sph,               &
     &        SSMHDs%SPH_WK%trns_WK, SVIZ_m%SPH_SGS%trns_WK_LES,        &
     &        SVIZ_m%FEM_DAT%geofem, SVIZ_m%FEM_DAT%field)
!
          if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_SGS_MHD'
          call FEM_analyze_sph_SGS_MHD                                  &
     &       (SSMHDs%MHD_files, SSMHDs%MHD_step, SSMHDs%MHD_IO,         &
     &        SVIZ_m%FEM_DAT, SSMHDs%m_SR)
          if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
        end if
!*
!*  ----------- Move tracer --------------
!*
        if(SVIZ_m%tracers%num_trace .gt. 0) then
          if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+5)
           call TRACER_evolution(SSMHDs%MHD_step%time_d,                &
     &        SSMHDs%MHD_step%finish_d, SSMHDs%MHD_step%rst_step,       &
     &        SSMHDs%MHD_step%viz_step%istep_tracer,                    &
     &        SVIZ_m%FEM_DAT%geofem, SVIZ_m%VIZ_FEM%para_surf,          &
     &        SVIZ_m%FEM_DAT%field, SVIZ_m%tracers, SSMHDs%m_SR)
          if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+5)
        end if
!*
!*  ----------- Visualization --------------
!*
        if(iflag_vizs_w_fix_step(SSMHDs%MHD_step%time_d%i_time_step,    &
     &                           SSMHDs%MHD_step%viz_step)) then
          if (iflag_debug.eq.1) write(*,*) 'visualize_all', my_rank
          if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+4)
          call istep_viz_w_fix_dt(SSMHDs%MHD_step%time_d%i_time_step,   &
     &                          SSMHDs%MHD_step%viz_step)
          call visualize_all                                            &
     &       (SSMHDs%MHD_step%viz_step, SSMHDs%MHD_step%time_d,         &
     &        SVIZ_m%FEM_DAT%geofem, SVIZ_m%FEM_DAT%field,              &
     &        SVIZ_m%VIZ_FEM, SVIZ_m%VIZs, SSMHDs%m_SR)
!*
!*  ----------- Zonal means --------------
!*
          if(SSMHDs%MHD_step%viz_step%istep_psf .ge. 0                  &
     &        .or. SSMHDs%MHD_step%viz_step%istep_map .ge. 0) then
            call SGS_MHD_zmean_sections(SSMHDs%MHD_step%viz_step,       &
     &          SSMHDs%MHD_step%time_d, SSMHDs%SPH_MHD%sph,             &
     &          SVIZ_m%FEM_DAT%geofem, SSMHDs%SPH_WK%trns_WK,           &
     &          SVIZ_m%SPH_SGS, SVIZ_m%FEM_DAT%field,                   &
     &          SVIZ_m%zmeans, SSMHDs%m_SR)
          end if
          if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+4)
        end if
!
        if(lead_field_data_flag(SSMHDs%MHD_step%time_d%i_time_step,     &
     &                          SSMHDs%MHD_step)                        &
     &    .or. lead_field_data_flag(SSMHDs%MHD_step%time_d%i_time_step, &
     &                              SSMHDs%MHD_step)                    &
     &    .or. SVIZ_m%tracers%num_trace .gt. 0) then
          if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+6)
          call dealloc_sph_trans_area_snap(SSMHDs%SPH_WK%trns_WK)
          call dealloc_SGS_sph_trns_area_snap                           &
     &       (SVIZ_m%SPH_SGS%trns_WK_LES)
          if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+6)
        end if
!*  -----------  exit loop --------------
!*
        if(iflag_finish .gt. 0) exit
      end do
!
!  time evolution end
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+2)
!
      if (iflag_debug.eq.1) write(*,*) 'visualize_fin'
      call visualize_fin(SSMHDs%MHD_step%viz_step,                      &
     &                   SSMHDs%MHD_step%time_d, SVIZ_m%VIZs)
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize_sph_SGS_MHD'
      call FEM_finalize_sph_SGS_MHD(SSMHDs%MHD_files, SSMHDs%MHD_step,  &
     &                              SSMHDs%MHD_IO)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_MHD'
!      call SPH_finalize_MHD
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
      if (iflag_debug.eq.1) write(*,*) 'write_resolution_data'
      call write_resolution_data(SSMHDs%SPH_MHD%sph)
      if (iflag_debug.eq.1) write(*,*) 'output_elapsed_times '
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_mhd
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_MHD

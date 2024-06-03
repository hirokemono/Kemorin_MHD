!>@file   analyzer_sph_snap.f90
!!@brief  module analyzer_sph_snap
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to evaluate snapshots from spectr data
!!
!!@verbatim
!!      subroutine initialize_sph_snap(control_file_name)
!!      subroutine evolution_sph_snap
!!        character(len=kchara), intent(in) :: control_file_name
!!        type(sph_SGS_SNAP), intent(inout) :: SSNAPs
!!@endverbatim
!
      module analyzer_sph_snap
!
      use m_precision
      use calypso_mpi
!
      use m_work_time
      use m_elapsed_labels_4_MHD
      use m_elapsed_labels_SEND_RECV
      use m_machine_parameter
      use t_spherical_MHD
      use t_sph_SGS_MHD
!
      implicit none
!
      integer(kind = kint), parameter :: id_ctl_file = 11
!
!>      Control struture for MHD simulation
      type(spherical_MHD), save, private :: SSNAPs
!>      Structure for visualization in spherical MHD
      type(sph_SGS_MHD), save, private :: SVIZ_m
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialize_sph_snap(control_file_name)
!
      use m_elapsed_labels_4_REPART
      use init_sph_MHD_elapsed_label
      use initialization_sph_SGS_snap
!
      character(len=kchara), intent(in) :: control_file_name
!
      write(*,*) 'Simulation start: PE. ', my_rank
      SSNAPs%MHD_step%finish_d%started_time = MPI_WTIME()
      call init_elapse_time_by_TOTAL
      call set_sph_MHD_elapsed_label
!
      call elpsed_label_4_repartition
      call elpsed_label_field_send_recv
!
      call initialize_sph_SGS_snap(control_file_name, SSNAPs, SVIZ_m)
!
      end subroutine initialize_sph_snap
!
! ----------------------------------------------------------------------
!
      subroutine evolution_sph_snap
!
      use t_time_data
      use t_VIZ_step_parameter
      use t_sph_trans_arrays_MHD
      use t_sph_trans_arrays_SGS_MHD
      use t_visualizer
      use t_SPH_MHD_zonal_mean_viz
      use SPH_analyzer_SGS_snap
      use FEM_analyzer_sph_SGS_MHD
      use output_viz_file_control
      use SGS_MHD_zonal_mean_viz
      use set_time_step_params
      use init_sph_MHD_elapsed_label
!
!*  -----------  set initial step data --------------
!*
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+2)
      call set_from_initial_step(SSNAPs%MHD_step%init_d,                &
     &                           SSNAPs%MHD_step%time_d)
!*
!*  -------  time evelution loop start -----------
!*
      do
        call add_one_step(SSNAPs%MHD_step%time_d)
        if(output_IO_flag(SSNAPs%MHD_step%time_d%i_time_step,           &
     &                   SSNAPs%MHD_step%rst_step) .eqv. .FALSE.) cycle
!
!*  ----------  time evolution by spectral methood -----------------
!*
        if(lead_field_data_flag(SSNAPs%MHD_step%time_d%i_time_step,     &
     &                          SSNAPs%MHD_step)) then
          call alloc_sph_trans_area_snap(SSNAPs%SPH_MHD%sph,            &
     &                          SSNAPs%SPH_WK%trns_WK)
          call alloc_SGS_sph_trns_area_snap(SSNAPs%SPH_MHD%sph,         &
     &                                      SVIZ_m%SPH_SGS%trns_WK_LES)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'SPH_analyze_SGS_snap'
        call SPH_analyze_SGS_snap                                       &
     &     (SSNAPs%MHD_files, SSNAPs%SPH_model, SSNAPs%MHD_step,        &
     &      SVIZ_m%SPH_SGS, SSNAPs%SPH_MHD, SSNAPs%SPH_WK, SSNAPs%m_SR)
!*
!*  -----------  Send field data to FEM mesh --------------
!*
        if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
!
        if(lead_field_data_flag(SSNAPs%MHD_step%time_d%i_time_step,     &
     &                          SSNAPs%MHD_step)) then
          if (iflag_debug.eq.1) write(*,*) 'SPH_to_FEM_bridge_SGS_MHD'
          call SPH_to_FEM_bridge_SGS_MHD                                &
     &       (SVIZ_m%SPH_SGS%SGS_par, SSNAPs%SPH_MHD%sph,               &
     &        SSNAPs%SPH_WK%trns_WK, SVIZ_m%SPH_SGS%trns_WK_LES,        &
     &        SVIZ_m%FEM_DAT%geofem, SVIZ_m%FEM_DAT%field)
        end if
!
        if (iflag_debug.eq.1) write(*,*) 'FEM_analyze_sph_SGS_MHD'
        call FEM_analyze_sph_SGS_MHD(SSNAPs%MHD_files, SSNAPs%MHD_step, &
     &      SSNAPs%MHD_IO, SVIZ_m%FEM_DAT, SSNAPs%m_SR)
        if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
!*  ----------- Visualization --------------
!*
        if(iflag_vizs_w_fix_step(SSNAPs%MHD_step%time_d%i_time_step,    &
     &                           SSNAPs%MHD_step%viz_step)) then
          if (iflag_debug.eq.1) write(*,*) 'visualize_all'
          if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+4)
          call istep_viz_w_fix_dt(SSNAPs%MHD_step%time_d%i_time_step,   &
     &                          SSNAPs%MHD_step%viz_step)
          call visualize_all                                            &
     &       (SSNAPs%MHD_step%viz_step, SSNAPs%MHD_step%time_d,         &
     &        SVIZ_m%FEM_DAT%geofem, SVIZ_m%FEM_DAT%field,              &
     &        SVIZ_m%VIZ_FEM, SVIZ_m%VIZs, SSNAPs%m_SR)
!*
!*  ----------- Zonal means --------------
!*
          if(SSNAPs%MHD_step%viz_step%istep_psf .ge. 0                  &
     &        .or. SSNAPs%MHD_step%viz_step%istep_map .ge. 0) then
            call SGS_MHD_zmean_sections                                 &
     &         (SSNAPs%MHD_step%viz_step, SSNAPs%MHD_step%time_d,       &
     &          SSNAPs%SPH_MHD%sph, SVIZ_m%FEM_DAT%geofem,              &
     &          SSNAPs%SPH_WK%trns_WK, SVIZ_m%SPH_SGS,                  &
     &          SVIZ_m%FEM_DAT%field, SVIZ_m%zmeans, SSNAPs%m_SR)
          end if
          if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+4)
        end if
!
        if(lead_field_data_flag(SSNAPs%MHD_step%time_d%i_time_step,     &
     &                          SSNAPs%MHD_step)) then
           call dealloc_sph_trans_area_snap(SSNAPs%SPH_WK%trns_WK)
           call dealloc_SGS_sph_trns_area_snap                          &
     &        (SVIZ_m%SPH_SGS%trns_WK_LES)
        end if
!
!*  -----------  exit loop --------------
!*
        if(SSNAPs%MHD_step%time_d%i_time_step                           &
     &        .ge. SSNAPs%MHD_step%finish_d%i_end_step) exit
      end do
!
!  time evolution end
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+2)
!
      if (iflag_debug.eq.1) write(*,*) 'visualize_fin'
      call visualize_fin(SSNAPs%MHD_step%viz_step,                      &
     &                   SSNAPs%MHD_step%time_d, SVIZ_m%VIZs)
      if (iflag_debug.eq.1) write(*,*) 'FEM_finalize_sph_SGS_MHD'
      call FEM_finalize_sph_SGS_MHD(SSNAPs%MHD_files, SSNAPs%MHD_step,  &
     &                              SSNAPs%MHD_IO)
!
!      if (iflag_debug.eq.1) write(*,*) 'SPH_finalize_SGS_snap'
!      call SPH_finalize_SGS_snap
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
      call output_elapsed_times
!
      call calypso_MPI_barrier
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_sph_snap
!
! ----------------------------------------------------------------------
!
      end module analyzer_sph_snap

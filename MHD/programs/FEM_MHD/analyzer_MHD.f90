!
!      module analyzer_MHD
!
!      Written by H. Matsui and H. Okuda
!      modified by H. Matsui on June, 2005 
!
!      subroutine initialization_MHD
!      subroutine evolution_MHD
!
      module analyzer_MHD
!
      use m_precision
      use calypso_mpi
      use m_work_time
      use m_elapsed_labels_4_MHD
      use m_elapsed_labels_SEND_RECV
!
      use FEM_analyzer_MHD
      use t_visualizer
      use t_VIZ_mesh_field
      use t_FEM_SGS_MHD
!
      implicit none
!
!
      type(FEM_MHD), save, private :: FMHDs
      type(FEM_SGS_MHD), save, private ::  FSGSs
      type(FEM_SGS_vizs), save, private :: FMVIZs
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine initialization_MHD
!
      use input_control
      use m_elapsed_labels_4_VIZ
      use FEM_to_VIZ_bridge
!
!
      FMHDs%MHD_step%finish_d%started_time = MPI_WTIME()
      write(*,*) 'Simulation start: PE. ', my_rank
!
      call init_elapse_time_by_TOTAL
      call elapsed_label_4_MHD
      call elapsed_label_4_FEM_MHD
      call elpsed_label_4_VIZ
      call elpsed_label_field_send_recv
!
!     --------------------- 
!
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
      call input_control_4_FEM_MHD                                      &
     &   (FMHDs%MHD_files, FMHDs%FEM_model%FEM_prm,                     &
     &    FSGSs%FEM_SGS%SGS_par, FMHDs%MHD_step,                        &
     &    FMHDs%FEM_model%MHD_prop, FMHDs%FEM_model%MHD_BC,             &
     &    FMHDs%FEM_MHD%geofem, FMHDs%FEM_MHD%field,                    &
     &    FSGSs%SGS_MHD_wk%ele_fld, FMHDs%FEM_MHD%nod_mntr,             &
     &    FMHDs%FEM_model%bc_FEM_IO, FSGSs%FEM_SGS%FEM_filters,         &
     &    FSGSs%SGS_MHD_wk%FEM_SGS_wk, FMHDs%MHD_CG,                    &
     &    FMVIZs%tracer_ctls, FMVIZs%vizs_ctl)
      call copy_delta_t(FMHDs%MHD_step%init_d, FMHDs%MHD_step%time_d)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
!
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+1)
      call FEM_initialize_MHD                                           &
     &   (FMHDs%MHD_files, FMHDs%flex_MHD, FMHDs%MHD_step,              &
     &    FMHDs%FEM_model, FMHDs%FEM_MHD, FMHDs%MHD_CG,                 &
     &    FSGSs%FEM_SGS, FSGSs%SGS_MHD_wk, FMHDs%MHD_IO,                &
     &    FMHDs%fem_sq, FMHDs%m_SR)
!
      call init_FEM_MHD_to_VIZ_bridge(FMHDs%MHD_step%viz_step,          &
     &    FSGSs%SGS_MHD_wk%fem_int%next_tbl,                            &
     &    FSGSs%SGS_MHD_wk%fem_int%jcs, FMHDs%FEM_MHD%geofem,           &
     &    FMVIZs%VIZ_DAT, FMHDs%m_SR)
!
      if(iflag_VIZ_time) call start_elapsed_time(ist_elapsed_VIZ+11)
      call FLINE_initialize(FMHDs%MHD_step%viz_step%FLINE_t%increment,  &
     &    FMHDs%FEM_MHD%geofem, FMHDs%FEM_MHD%field,                    &
     &    FMVIZs%tracer_ctls%tracer_controls, FMVIZs%tracers)
      if(iflag_VIZ_time) call end_elapsed_time(ist_elapsed_VIZ+11)
!
      call init_visualize(FMHDs%MHD_step%viz_step,                      &
     &    FMHDs%FEM_MHD%geofem, FMHDs%FEM_MHD%field,                    &
     &    FMVIZs%VIZ_DAT, FMVIZs%vizs_ctl, FMVIZs%VIZs, FMHDs%m_SR)
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+1)
!
      end subroutine initialization_MHD
!
! ----------------------------------------------------------------------
!
      subroutine evolution_MHD
!
      use output_viz_file_control
      use FEM_to_VIZ_bridge
!
      integer(kind = kint) :: retval
      logical :: visval
!
!
      retval = 1
      if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+2)
!
      do
!  Time evolution
        call FEM_analyze_MHD(FMHDs%MHD_files, FMHDs%FEM_model,          &
     &      FMHDs%flex_MHD, FMHDs%MHD_step, retval, FMHDs%MHD_CG,       &
     &      FSGSs%FEM_SGS, FSGSs%SGS_MHD_wk, FMHDs%FEM_MHD,             &
     &      FMHDs%MHD_IO, FMHDs%fem_sq, FMHDs%m_SR)
!
!  Visualization
        visval = MHD_viz_routine_flag(FMHDs%MHD_step%flex_p,            &
     &                                FMHDs%MHD_step%time_d,            &
     &                                FMHDs%MHD_step%viz_step)
        if (visval) then
          if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+3)
          call MHD_viz_routine_step(FMHDs%MHD_step%flex_p,              &
     &        FMHDs%MHD_step%time_d, FMHDs%MHD_step%viz_step)
          call visualize_all                                            &
     &       (FMHDs%MHD_step%viz_step, FMHDs%MHD_step%time_d,           &
     &        FMHDs%FEM_MHD%geofem, FMHDs%FEM_MHD%field,                &
     &        FMVIZs%VIZ_DAT, FMVIZs%VIZs, FMHDs%m_SR)
          if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+3)
        end if
!
        if (retval .eq. 0) exit
      end do
!
      if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+2)
!
!  time evolution end
!
      call visualize_fin                                                &
     &   (FMHDs%MHD_step%viz_step, FMHDs%MHD_step%time_d, FMVIZs%VIZs)
      call FEM_finalize_MHD                                             &
     &   (FMHDs%MHD_files, FMHDs%MHD_step, FMHDs%MHD_IO)
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
!
      call output_elapsed_times
!
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine evolution_MHD
!
! ----------------------------------------------------------------------
!
      end module analyzer_MHD

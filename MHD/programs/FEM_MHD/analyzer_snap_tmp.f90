!analyzer_snap_tmp.f90
!      module analyzer_snap_tmp
!
!..................................................
!
!      Written by H. Matsui & H. Okuda
!      Modified by H. Matsui
!
      module analyzer_snap_tmp
!
      use m_precision
      use calypso_mpi
!
      use m_work_time
      use m_elapsed_labels_4_MHD
      use m_elapsed_labels_SEND_RECV
      use m_MHD_step_parameter
      use m_FEM_MHD_model_data
      use FEM_analyzer_snap_tmp
      use t_visualizer
      use t_VIZ_mesh_field
      use t_mesh_SR
      use m_solver_SR
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_analyzer
!
      use input_control
      use m_elapsed_labels_4_VIZ
      use FEM_to_VIZ_bridge
!
!
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
      call input_control_4_FEM_snap                                     &
     &   (MHD_files1, FEM_model1%FEM_prm, FEM_SGS1%SGS_par,             &
     &    MHD_step1, FEM_model1%MHD_prop, FEM_model1%MHD_BC,            &
     &    FEM_MHD1%geofem, FEM_MHD1%field,                              &
     &    SGS_MHD_wk1%ele_fld, FEM_model1%bc_FEM_IO,                    &
     &    FEM_SGS1%FEM_filters, SGS_MHD_wk1%FEM_SGS_wk, MHD_CG1,        &
     &    vizs_ctl_F)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
!
!     --------------------- 
!
      call FEM_initialize_snap_tmp                                      &
     &   (MHD_files1, MHD_step1, FEM_model1, MHD_CG1%ak_MHD,            &
     &    FEM_MHD1, FEM_SGS1, SGS_MHD_wk1, MHD_IO1, fem_sq1,            &
     &    v_sol2, SR_sig1, SR_r1, SR_i1, SR_il1)
!
      call init_FEM_MHD_to_VIZ_bridge(MHD_step1%viz_step,               &
     &    SGS_MHD_wk1%fem_int%next_tbl, SGS_MHD_wk1%fem_int%jcs,        &
     &    FEM_MHD1%geofem, VIZ_DAT2, SR_sig1, SR_r1, SR_i1, SR_il1)
      call init_visualize(MHD_step1%viz_step, FEM_MHD1%geofem,          &
     &    FEM_MHD1%field, VIZ_DAT2, vizs_ctl_F, vizs_F,                 &
     &    SR_sig1, SR_r1, SR_i1, SR_il1)
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use output_viz_file_control
      use FEM_to_VIZ_bridge
!
      integer(kind = kint) :: i_step
      logical :: visval
!
!
      do i_step = MHD_step1%init_d%i_time_step,                         &
     &            MHD_step1%finish_d%i_end_step
!
!  Read and generate fields
        call FEM_analyze_snap_tmp(i_step, MHD_files1, FEM_model1,       &
     &      MHD_CG1%ak_MHD, MHD_step1, FEM_SGS1, SGS_MHD_wk1,           &
     &      FEM_MHD1, MHD_IO1, fem_sq1, v_sol2, SR_sig1, SR_r1)
!
!  Visualization
        visval = MHD_viz_routine_flag                                   &
     &       (MHD_step1%flex_p, MHD_step1%time_d, MHD_step1%viz_step)
        if(visval) then
          if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+4)
          call MHD_viz_routine_step                                     &
     &       (MHD_step1%flex_p, MHD_step1%time_d, MHD_step1%viz_step)
          call visualize_all(MHD_step1%viz_step, MHD_step1%time_d,      &
     &        FEM_MHD1%geofem, FEM_MHD1%field, VIZ_DAT2, vizs_F,        &
     &        v_sol2, SR_sig1, SR_r1, SR_i1, SR_il1)
          if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+4)
        end if
      end do
!
      call FEM_finalize_snap_tmp(MHD_files1, MHD_step1, MHD_IO1)
!
      call output_elapsed_times
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_snap_tmp

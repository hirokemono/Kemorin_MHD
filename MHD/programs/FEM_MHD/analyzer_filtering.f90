!analyzer_filtering.f90
!      module analyzer_filtering
!
!..................................................
!
!      Written by H. Matsui & H. Okuda
!      Modified by H. Matsui
!
      module analyzer_filtering
!
      use m_precision
      use calypso_mpi
!
      use m_MHD_step_parameter
      use m_FEM_MHD_model_data
      use m_work_time
      use m_elapsed_labels_SEND_RECV
      use FEM_analyzer_filtered
      use t_visualizer
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
      use FEM_analyzer_snapshot
      use input_control
      use m_elapsed_labels_4_VIZ
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
!
      call init_elapse_time_by_TOTAL
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
     &    viz_ctls_F)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
!
!     --------------------- 
!
      call FEM_initialize_snapshot                                      &
     &   (MHD_files1, MHD_step1, FEM_MHD1%geofem, FEM_MHD1%iphys,       &
     &    FEM_MHD1%field, FEM_model1, MHD_CG1%ak_MHD, FEM_SGS1,         &
     &    SGS_MHD_wk1, MHD_IO1, fem_sq1, FEM_MHD1%label_sim)
!
      call init_visualize                                               &
     &   (FEM_MHD1%geofem, FEM_MHD1%field, viz_ctls_F, vizs_F)
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use FEM_analyzer_snapshot
      use output_viz_file_control
!
      integer(kind=kint ) :: i_step
      logical :: visval
!
!
      do i_step = MHD_step1%init_d%i_time_step,                         &
     &           MHD_step1%finish_d%i_end_step
!
!  Read and generate fields
        call FEM_analyze_filtered                                       &
     &     (i_step, MHD_files1, FEM_MHD1%geofem, FEM_MHD1%iphys,        &
     &      FEM_model1, MHD_CG1%ak_MHD, MHD_step1, FEM_SGS1,            &
     &      SGS_MHD_wk1, FEM_MHD1%field, MHD_IO1%ucd, MHD_IO1, fem_sq1)
!
!  Visualization
        visval = MHD_viz_routine_flag                                   &
     &       (MHD_step1%flex_p, MHD_step1%time_d, MHD_step1%viz_step)
        if (visval) then
          call MHD_viz_routine_step                                     &
     &     (MHD_step1%flex_p, MHD_step1%time_d, MHD_step1%viz_step)
          call visualize_all(MHD_step1%viz_step, MHD_step1%time_d,      &
     &        FEM_MHD1%geofem, FEM_MHD1%field,                          &
     &        SGS_MHD_wk1%fem_int%next_tbl%neib_ele,                    &
     &        SGS_MHD_wk1%fem_int%jcs, vizs_F)
        end if
      end do
!
      call FEM_finalize_snapshot                                        &
     &   (MHD_files1, MHD_step1, MHD_IO1)
!
      call output_elapsed_times
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_filtering

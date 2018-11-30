!analyzer_snap.f90
!      module analyzer_snap
!
!..................................................
!
!      Written by H. Matsui & H. Okuda
!      Modified by H. Matsui
!
      module analyzer_snap
!
      use m_precision
      use calypso_mpi
!
      use m_MHD_step_parameter
      use m_FEM_MHD_model_data
      use m_work_time
      use m_elapsed_labels_4_MHD
      use FEM_analyzer_snapshot
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
      use input_control
      use m_elapsed_labels_4_VIZ
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
!
      num_elapsed = 0
      call allocate_elapsed_times
!
      call elapsed_label_4_MHD
      call elapsed_label_4_FEM_MHD
      call elpsed_label_4_VIZ
      call append_COMM_TIME_to_elapsed
!
!     --------------------- 
!
      call input_control_4_FEM_snap                                     &
     &   (MHD_files1, FEM_model1%FEM_prm, FEM_SGS1%SGS_par,             &
     &    MHD_step1, FEM_model1%MHD_prop, FEM_model1%MHD_BC,            &
     &    FEM_MHD1%geofem, FEM_MHD1%ele_mesh, FEM_MHD1%field,           &
     &    SGS_MHD_wk1%ele_fld, FEM_model1%bc_FEM_IO,                    &
     &    FEM_SGS1%FEM_filters, SGS_MHD_wk1%FEM_SGS_wk, MHD_CG1,        &
     &    viz_ctls_F)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
!
!     --------------------- 
!
      call FEM_initialize_snapshot                                      &
     &   (MHD_files1, MHD_step1, FEM_MHD1%geofem, FEM_MHD1%ele_mesh,    &
     &    FEM_MHD1%iphys, FEM_MHD1%field, FEM_model1, MHD_CG1%ak_MHD,   &
     &    FEM_SGS1, SGS_MHD_wk1, MHD_IO1, fem_sq1, FEM_MHD1%label_sim)
!
      call init_visualize                                               &
     &   (FEM_MHD1%geofem, FEM_MHD1%ele_mesh, FEM_MHD1%field,           &
     &    viz_ctls_F, vizs_F)
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      integer(kind=kint ) :: i_step, visval
!
!
      do i_step = MHD_step1%init_d%i_time_step,                         &
     &           MHD_step1%finish_d%i_end_step
!
!  Read and generate fields
        call FEM_analyze_snapshot(i_step, MHD_files1,                   &
     &      FEM_MHD1%geofem, FEM_MHD1%ele_mesh, FEM_MHD1%iphys,         &
     &      FEM_model1, MHD_CG1%ak_MHD, MHD_step1, visval, FEM_SGS1,    &
     &      SGS_MHD_wk1, FEM_MHD1%field, MHD_IO1, fem_sq1)
!
!  Visualization
        if (visval.eq.0) then
          if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+5)
          call visualize_all(MHD_step1%viz_step, MHD_step1%time_d,      &
     &        FEM_MHD1%geofem, FEM_MHD1%ele_mesh, FEM_MHD1%field,       &
     &        SGS_MHD_wk1%fem_int%next_tbl%neib_ele,                    &
     &        SGS_MHD_wk1%fem_int%jcs, vizs_F)
          if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+5)
        end if
      end do
!
      call FEM_finalize_snapshot                                        &
     &   (MHD_files1, MHD_step1, MHD_IO1)
!
      call copy_COMM_TIME_to_elaps
      call output_elapsed_times
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_snap

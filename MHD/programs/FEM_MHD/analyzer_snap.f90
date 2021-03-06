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
      use m_elapsed_labels_SEND_RECV
      use FEM_analyzer_snapshot
      use t_visualizer
      use t_VIZ_mesh_field
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
     &    SGS_MHD_wk1%ele_fld, VIZ_DAT2, FEM_model1%bc_FEM_IO,          &
     &    FEM_SGS1%FEM_filters, SGS_MHD_wk1%FEM_SGS_wk, MHD_CG1,        &
     &    vizs_rprt_c_F%vizs_ctl, vizs_rprt_c_F%repart_ctl)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
!
!     --------------------- 
!
      call FEM_initialize_snapshot                                      &
     &   (MHD_files1, MHD_step1, FEM_model1, MHD_CG1%ak_MHD, FEM_MHD1,  &
     &    FEM_SGS1, SGS_MHD_wk1, MHD_IO1, fem_sq1)
!
      call init_FEM_MHD_to_VIZ_bridge(MHD_step1%viz_step,               &
     &    SGS_MHD_wk1%fem_int%next_tbl, SGS_MHD_wk1%fem_int%jcs,        &
     &    FEM_MHD1%geofem, FEM_MHD1%field, VIZ_DAT2)
      call init_visualize(VIZ_DAT2%viz_fem, VIZ_DAT2%edge_comm,         &
     &    VIZ_DAT2%viz_fld, vizs_rprt_c_F%vizs_ctl, vizs_F)
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
     &           MHD_step1%finish_d%i_end_step
!
!  Read and generate fields
        call FEM_analyze_snapshot(i_step, MHD_files1, FEM_model1,       &
     &      MHD_CG1%ak_MHD, MHD_step1, FEM_SGS1, SGS_MHD_wk1,           &
     &      FEM_MHD1, MHD_IO1, fem_sq1)
!
!  Visualization
        visval = MHD_viz_routine_flag                                   &
     &       (MHD_step1%flex_p, MHD_step1%time_d, MHD_step1%viz_step)
        if(visval) then
          if(iflag_MHD_time) call start_elapsed_time(ist_elapsed_MHD+4)
          call MHD_viz_routine_step                                     &
     &       (MHD_step1%flex_p, MHD_step1%time_d, MHD_step1%viz_step)
          call s_FEM_to_VIZ_bridge(FEM_MHD1%field, FEM_MHD1%v_sol,      &
     &        VIZ_DAT2)
          call visualize_all(MHD_step1%viz_step, MHD_step1%time_d,      &
     &        VIZ_DAT2%viz_fem, VIZ_DAT2%edge_comm, VIZ_DAT2%viz_fld,   &
     &        VIZ_DAT2%ele_4_nod, VIZ_DAT2%jacobians, vizs_F)
          if(iflag_MHD_time) call end_elapsed_time(ist_elapsed_MHD+4)
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
      end module analyzer_snap

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
      use FEM_analyzer_filtered
      use visualizer_all
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
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
!
      num_elapsed = 68
      call allocate_elapsed_times
!
      elapse_labels(1) = 'Total time                 '
      elapse_labels(2) = 'Initialization time        '
      elapse_labels(3) = 'Time evolution loop time   '
      elapse_labels(4) = 'Data IO time               '
      elapse_labels(5) = 'Linear solver time         '
      elapse_labels(6) = 'Communication for RHS      '
      elapse_labels(7) = 'Communication time         '
!
      elapse_labels(60) = 'Sectioning initialization.    '
      elapse_labels(61) = 'Isosurfaceing initialization.    '
      elapse_labels(62) = 'Volume rendering initialization.    '
      elapse_labels(63) = 'fieldline initialization.    '
!
      elapse_labels(65) = 'Sectioning.    '
      elapse_labels(66) = 'Isosurfaceing.    '
      elapse_labels(67) = 'Volume rendering.    '
      elapse_labels(68) = 'fieldline.    '
!
!     --------------------- 
!
      call input_control_4_FEM_snap                                     &
     &   (MHD_files1, FEM_model1%FEM_prm, FEM_SGS1%SGS_par,             &
     &    MHD_step1, FEM_model1%MHD_prop, FEM_model1%MHD_BC,            &
     &    FEM_MHD1%geofem, FEM_MHD1%ele_mesh, FEM_MHD1%field,           &
     &    SGS_MHD_wk1%ele_fld, FEM_model1%bc_FEM_IO,                    &
     &    FEM_SGS1%FEM_filters, SGS_MHD_wk1%FEM_SGS_wk, MHD_CG1)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
!
!     --------------------- 
!
      call FEM_initialize_snapshot(MHD_files1, MHD_step1,               &
     &    FEM_MHD1%geofem, FEM_MHD1%ele_mesh, FEM_MHD1%iphys,           &
     &    FEM_MHD1%field, FEM_model1, MHD_CG1%ak_MHD, FEM_SGS1,         &
     &    SGS_MHD_wk1, range1, fem_ucd1, fem_sq1, FEM_MHD1%label_sim)
!
      call init_visualize                                               &
     &   (FEM_MHD1%geofem, FEM_MHD1%ele_mesh, FEM_MHD1%field)
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use FEM_analyzer_snapshot
!
      integer(kind=kint ) :: i_step, visval
!
!
      do i_step = MHD_step1%init_d%i_time_step,                         &
     &           MHD_step1%finish_d%i_end_step
!
!  Read and generate fields
        call FEM_analyze_filtered(i_step, MHD_files1,                   &
     &      FEM_MHD1%geofem, FEM_MHD1%ele_mesh, FEM_MHD1%iphys,         &
     &      FEM_model1, MHD_CG1%ak_MHD, MHD_step1, visval, FEM_SGS1,    &
     &      SGS_MHD_wk1, FEM_MHD1%field, fem_ucd1, fem_sq1)
!
!  Visualization
        if (visval.eq.0) then
          call visualize_all(MHD_step1%viz_step, MHD_step1%time_d,      &
     &        FEM_MHD1%geofem, FEM_MHD1%ele_mesh, FEM_MHD1%field,       &
     &        SGS_MHD_wk1%fem_int%next_tbl%neib_ele,                    &
     &        SGS_MHD_wk1%fem_int%jcs)
        end if
      end do
!
      call FEM_finalize_snapshot                                        &
     &   (MHD_files1, MHD_step1, range1, fem_ucd1)
      call output_elapsed_times
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_filtering

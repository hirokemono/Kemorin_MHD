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
      use m_MHD_step_parameter
      use m_mesh_data
      use m_node_phys_data
      use m_physical_property
      use m_mean_square_values
      use m_FEM_MHD_model_data
      use m_work_FEM_SGS_MHD
      use m_solver_djds_MHD
      use FEM_analyzer_snap_tmp
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
      use m_bc_data_list
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
      elapse_labels(12) = 'Visualizatio time         '
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
     &   (MHD_files1, FEM_model1%FEM_prm, FEM_SGS1%SGS_par, MHD_step1,  &
     &    MHD_prop1, MHD_BC1, femmesh1, ele_mesh1, nod_fld1,            &
     &    SGS_MHD_wk1%ele_fld, bc_FEM_IO1, FEM_SGS1%FEM_filters,        &
     &    SGS_MHD_wk1%FEM_SGS_wk, MHD_CG1)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
!
!     --------------------- 
!
      call FEM_initialize_snap_tmp                                      &
     &   (MHD_files1, bc_FEM_IO1, MHD_step1, femmesh1, ele_mesh1,       &
     &    iphys_nod1, nod_fld1, FEM_model1, MHD_CG1%ak_MHD, FEM_SGS1,   &
     &    SGS_MHD_wk1, range1, fem_ucd1, fem_sq1, label_sim)
!
      call init_visualize(femmesh1, ele_mesh1, nod_fld1)
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
     &            MHD_step1%finish_d%i_end_step
!
!  Read and generate fields
        call FEM_analyze_snap_tmp(i_step, MHD_files1, FEM_model1,       &
     &      femmesh1, ele_mesh1, iphys_nod1, MHD_CG1%ak_MHD,            &
     &      MHD_step1, visval, FEM_SGS1, SGS_MHD_wk1,                   &
     &      nod_fld1, fem_ucd1, fem_sq1)
!
!  Visualization
        if (visval.eq.0) then
          call start_elapsed_time(12)
          call visualize_all(MHD_step1%viz_step, MHD_step1%time_d,      &
     &        femmesh1, ele_mesh1, nod_fld1,                            &
     &        SGS_MHD_wk1%fem_int%next_tbl%neib_ele,                    &
     &        SGS_MHD_wk1%fem_int%jcs)
          call end_elapsed_time(12)
        end if
      end do
!
      call FEM_finalize_snap_tmp                                        &
     &   (MHD_files1, MHD_step1, range1, fem_ucd1)
      call output_elapsed_times
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_snap_tmp

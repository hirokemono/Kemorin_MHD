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
      use m_control_parameter
      use m_MHD_step_parameter
      use m_SGS_control_parameter
      use m_mesh_data
      use m_node_phys_data
      use m_physical_property
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
      use m_bc_data_list
      use m_3d_filter_coef_MHD
      use m_boundary_field_IO
      use m_type_AMG_data
      use m_type_AMG_data_4_MHD
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
      call input_control_4_snapshot                                     &
     &   (MHD_files1, FEM_prm1, SGS_par1, MHD_step1,                    &
     &    MHD_prop1, MHD_BC1, mesh1, group1, ele_mesh1, nod_fld1,       &
     &    bc_FEM_IO1, filtering1, wide_filtering, wk_filter1,           &
     &    MGCG_WK1, MGCG_FEM1, MGCG_MHD_FEM1)
      call copy_delta_t(MHD_step1%init_d, MHD_step1%time_d)
!
!     --------------------- 
!
      call FEM_initialize_snapshot(MHD_files1, bc_FEM_IO1, MHD_step1)
!
      call init_visualize(mesh1, group1, ele_mesh1, nod_fld1)
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use m_finite_element_matrix
      use FEM_analyzer_snapshot
!
      integer(kind=kint ) :: i_step, visval
!
!
      do i_step = MHD_step1%init_d%i_time_step,                         &
     &           MHD_step1%finish_d%i_end_step
!
!  Read and generate fields
        call FEM_analyze_filtered                                       &
     &     (i_step, MHD_files1, MHD_step1, visval)
!
!  Visualization
        if (visval.eq.0) then
          call visualize_all(MHD_step1%viz_step, MHD_step1%time_d,      &
     &        mesh1, group1, ele_mesh1, nod_fld1,                       &
     &        fem_int1%next_tbl%neib_ele, fem_int1%jcs)
        end if
      end do
!
      call FEM_finalize_snapshot(MHD_files1, MHD_step1)
      call output_elapsed_times
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_filtering

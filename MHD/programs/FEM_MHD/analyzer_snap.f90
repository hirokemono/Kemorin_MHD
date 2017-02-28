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
      use m_control_parameter
      use m_SGS_control_parameter
      use m_mesh_data
      use m_node_phys_data
      use FEM_analyzer_snapshot
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
      use input_control
      use m_3d_filter_coef_MHD
      use m_boundary_field_IO
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
      call input_control_4_snapshot                                     &
     &   (FEM_prm1, SGS_par1, mesh1, group1, ele_mesh1, nod_fld1,       &
     &    IO_bc1, filtering1, wide_filtering, wk_filter1)
!
!     --------------------- 
!
      call FEM_initialize_snapshot
!
      call init_visualize(mesh1, group1, ele_mesh1, nod_fld1)
!
      end subroutine init_analyzer
!
! ----------------------------------------------------------------------
!
      subroutine analyze
!
      use m_t_step_parameter
      use m_jacobians
      use m_element_id_4_node
!
      integer(kind=kint ) :: i_step, visval
!
!
      do i_step = i_step_init, i_step_number
!
!  Read and generate fields
        call FEM_analyze_snapshot(i_step, viz_step1, visval)
!
!  Visualization
        if (visval.eq.0) then
          call start_eleps_time(12)
          call visualize_all                                            &
     &       (viz_step1, mesh1, group1, ele_mesh1, nod_fld1,            &
     &        next_tbl1%neib_ele, jac1_3d_q)
          call end_eleps_time(12)
        end if
      end do
!
      call FEM_finalize_snapshot
      call output_elapsed_times
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_snap

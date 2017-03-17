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
!
      use m_MHD_step_parameter
      use m_SGS_control_parameter
      use m_mesh_data
      use m_node_phys_data
!
      use FEM_analyzer_MHD
      use sections_for_1st
!
      implicit none
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
      use m_control_parameter
      use m_3d_filter_coef_MHD
      use m_boundary_field_IO
      use m_solver_djds_MHD
!
!
      total_start = MPI_WTIME()
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
      call start_eleps_time(1)
!
      call start_eleps_time(4)
      call input_control_4_MHD(FEM_prm1, SGS_par1,                      &
     &    mesh1, group1, ele_mesh1, nod_fld1, IO_bc1,                   &
     &    filtering1, wide_filtering, wk_filter1, MHD1_matrices)
      call end_eleps_time(4)
!
      call start_eleps_time(2)
      call FEM_initialize_MHD(MHD_step1)
!
      call init_visualize_surface(mesh1, group1, ele_mesh1, nod_fld1)
      call end_eleps_time(2)
!
      end subroutine initialization_MHD
!
! ----------------------------------------------------------------------
!
      subroutine evolution_MHD
!
      integer(kind=kint ) :: visval
      integer(kind=kint ) :: retval
!
!
      retval = 1
      call start_eleps_time(3)
!
      do
!  Time evolution
        call FEM_analyze_MHD(MHD_step1, visval, retval)
!
!     ---------------------
!
!  Visualization
        if (visval.eq.0) then
          call start_eleps_time(4)
          call visualize_surface                                        &
     &       (MHD_step1%viz_step, time_d1, mesh1, ele_mesh1, nod_fld1)
          call end_eleps_time(4)
        end if
!
        if (retval .eq. 0) exit
      end do
!
      call end_eleps_time(3)
!
!  time evolution end
!
      call FEM_finalize_MHD(MHD_step1)
!
      call copy_COMM_TIME_to_eleps(num_elapsed)
      call end_eleps_time(1)
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

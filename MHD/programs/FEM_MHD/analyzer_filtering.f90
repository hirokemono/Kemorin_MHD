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
      use m_mesh_data
      use m_geometry_data
      use m_node_phys_data
      use m_node_phys_data
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
      num_elapsed = 7
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
!     --------------------- 
!
      call input_control_4_snapshot(mesh1, group1)
!
!     --------------------- 
!
      call FEM_initialize_snapshot
!
      call init_visualize(mesh1, group1, surf1, edge1, ele_mesh1%edge_comm,       &
     &    nod_fld1)
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
      use FEM_analyzer_snapshot
!
      integer(kind=kint ) :: i_step, visval
      integer(kind=kint ) :: istep_psf, istep_iso
      integer(kind=kint ) :: istep_pvr, istep_fline
!
!
      do i_step = i_step_init, i_step_number
!
!  Read and generate fields
        call FEM_analyze_filtered(i_step,                               &
     &      istep_psf, istep_iso, istep_pvr, istep_fline, visval)
!
!  Visualization
        if (visval.eq.0) then
          call visualize_all                                            &
     &       (istep_psf, istep_iso, istep_pvr, istep_fline,             &
     &        mesh1, group1, surf1, edge1, ele_mesh1%edge_comm, nod_fld1, &
     &        next_tbl1%neib_ele, jac1_3d_q)
        end if
      end do
!
      call FEM_finalize_snapshot
!
      end subroutine analyze
!
! ----------------------------------------------------------------------
!
      end module analyzer_filtering

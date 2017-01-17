!analyzer_pvr.f90
!      module analyzer_pvr
!
!     Written by H. Matsui on July, 2006
!
      module analyzer_pvr
!
      use m_precision
!
      use m_visualization
!
      use FEM_analyzer_viz_pvr
      use volume_rendering
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine initialization
!
      use calypso_mpi
      use m_control_data_vizs
      use set_control_visualizer
!
      integer(kind = kint) :: ierr
!
!     read controls
!
      if (iflag_debug.gt.0) write(*,*) 'set_control_params_4_viz'
      call read_control_data_vizs
      call set_control_params_4_viz(my_rank, t_viz_ctl,                 &
     &   mesh_file_VIZ, ucd_VIZ, ierr)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!  FEM Initialization
      call FEM_initialize_pvr
!
!  VIZ Initialization
      call PVR_initialize(femmesh_VIZ%mesh%node, femmesh_VIZ%mesh%ele,  &
     &    elemesh_VIZ%surf, femmesh_VIZ%group, field_VIZ)
      call calypso_MPI_barrier
!
      end subroutine initialization
!
!  ---------------------------------------------------------------------
!
      subroutine analyze
!
      integer(kind=kint ) :: i_step, istep_pvr
!
!
      do i_step = i_step_init, i_step_number
!
!  Load field data
        call FEM_analyze_pvr(i_step, istep_pvr)
!
!  Rendering
        call PVR_visualize                                              &
     &     (istep_pvr, femmesh_VIZ%mesh%node, femmesh_VIZ%mesh%ele,     &
     &      elemesh_VIZ%surf, femmesh_VIZ%group, jac_VIZ_q, field_VIZ)
      end do
!
      end subroutine analyze
!
!  ---------------------------------------------------------------------
!
      end module analyzer_pvr

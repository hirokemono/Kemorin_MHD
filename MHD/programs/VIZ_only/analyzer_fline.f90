!analyzer_fline.f90
!      module analyzer_fline
!
      module analyzer_fline
!
!     Written by H. Matsui on July, 2006
!
      use m_precision
!
      use m_visualization
!
      use FEM_analyzer_viz_fline
      use fieldline
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine initialize_fline
!
      use calypso_mpi
      use m_control_data_vizs
      use set_control_visualizer
!
      integer(kind = kint) :: ierr
!
!     read controls
!
!
      if (iflag_debug.gt.0) write(*,*) 'set_control_params_4_viz'
      call read_control_data_vizs
      call set_control_params_4_viz(my_rank, ierr, ucd_VIZ)
!
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!  FEM Initialization
      call FEM_initialize_fline
!
!  VIZ Initialization
      call FLINE_initialize                                             &
     &   (femmesh_VIZ%mesh%node, femmesh_VIZ%mesh%ele,                  &
     &    femmesh_VIZ%group%ele_grp, femmesh_VIZ%group%surf_grp,        &
     &    field_VIZ)
!
      end subroutine initialize_fline
!
!  ---------------------------------------------------------------------
!
      subroutine analyze
!
      use m_t_step_parameter
!
      integer(kind=kint ) :: i_step, istep_fline
!
!
      do i_step = i_step_init, i_step_number
!
!  Load field data
        call FEM_analyze_fline(i_step, istep_fline)
!
!  Generate field lines
        call FLINE_visualize                                            &
     &     (istep_fline, femmesh_VIZ%mesh%node, femmesh_VIZ%mesh%ele,   &
     &      elemesh_VIZ%surf, femmesh_VIZ%group%ele_grp,                &
     &      ele_4_nod_VIZ, field_VIZ, femmesh_VIZ%mesh%nod_comm)
      end do
!
      end subroutine analyze
!
!  ---------------------------------------------------------------------
!
      end module analyzer_fline

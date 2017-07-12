!analyzer_fline.f90
!      module analyzer_fline
!
!     Written by H. Matsui on July, 2006
!
      module analyzer_fline
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
!
      integer(kind = kint) :: ierr
!
!     read controls
!
!
      if (iflag_debug.gt.0) write(*,*) 'set_control_params_4_viz'
      call read_control_data_vizs
      call set_control_params_4_viz(my_rank, t_viz_ctl, viz_plt,        &
     &   mesh_file_VIZ, ucd_VIZ, ierr)
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
      integer(kind = kint) :: i_step
!
!
      do i_step = t_VIZ%init_d%i_time_step, t_VIZ%finish_d%i_end_step
        if(set_IO_step_flag(i_step,t_VIZ%ucd_step) .ne. izero) cycle
!
!  Load field data
        call FEM_analyze_fline(i_step, t_VIZ, viz_step_V%FLINE_t)
!
!  Generate field lines
        call FLINE_visualize( viz_step_V%FLINE_t%istep_file,            &
     &      femmesh_VIZ%mesh%node, femmesh_VIZ%mesh%ele,                &
     &      elemesh_VIZ%surf, femmesh_VIZ%group%ele_grp,                &
     &      ele_4_nod_VIZ, field_VIZ, femmesh_VIZ%mesh%nod_comm)
      end do
!
      end subroutine analyze
!
!  ---------------------------------------------------------------------
!
      end module analyzer_fline

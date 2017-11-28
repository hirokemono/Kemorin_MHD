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
      use t_fieldline
!
      implicit none
!
      type(fieldline_module), save :: fline_v
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
      use t_control_data_vizs
!
      integer(kind = kint) :: ierr
!
!     read controls
!
!
      if (iflag_debug.gt.0) write(*,*) 'set_control_params_4_viz'
      call read_control_file_vizs
      call set_control_params_4_viz(my_rank, t_viz_ctl, viz_plt,        &
     &   mesh_file_VIZ, ucd_file_VIZ, ierr)
!
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!  FEM Initialization
      call FEM_initialize_fline(ucd_file_VIZ)
!
!  VIZ Initialization
      call FLINE_initialize                                             &
     &   (femmesh_VIZ, field_VIZ, viz_ctl_v%fline_ctls, fline_v)
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
        call FEM_analyze_fline                                          &
     &     (i_step, ucd_file_VIZ, t_VIZ, viz_step_V%FLINE_t)
!
!  Generate field lines
        call FLINE_visualize                                            &
     &     (viz_step_V%FLINE_t%istep_file, femmesh_VIZ, elemesh_VIZ,    &
     &      ele_4_nod_VIZ, field_VIZ, fline_v)
      end do
!
      end subroutine analyze
!
!  ---------------------------------------------------------------------
!
      end module analyzer_fline

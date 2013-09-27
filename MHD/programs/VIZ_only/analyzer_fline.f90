!analyzer_fline.f90
!      module analyzer_fline
!
      module analyzer_fline
!
!     Written by H. Matsui on July, 2006
!
      use m_precision
!
      use m_parallel_var_dof
!
      use FEM_analyzer_viz_fline
      use fieldline_1st
!
!
      implicit none
!
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine initialize_fline
!
      use m_control_data_vizs
      use set_control_visualizer
!
!
!     read controls
!
!
      if (iflag_debug.gt.0) write(*,*) 'set_control_params_4_viz'
      call read_control_data_vizs
      call set_control_params_4_viz(my_rank, ierr)
!
      if(ierr .gt. 0) call parallel_abort(ierr, e_message)
!
!  FEM Initialization
      call FEM_initialize_fline
!
!  VIZ Initialization
      call init_visualize_fline(ierr)
      if(ierr .gt. 0) call parallel_abort(ierr, e_message)
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
        if(istep_fline .ge. 0) call visualize_fline(istep_fline, ierr)
      end do
!
      end subroutine analyze
!
!  ---------------------------------------------------------------------
!
      end module analyzer_fline

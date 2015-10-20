!analyzer_fline.f90
!      module analyzer_fline
!
      module analyzer_fline
!
!     Written by H. Matsui on July, 2006
!
      use m_precision
!
      use t_ucd_data
!
      use FEM_analyzer_viz_fline
      use fieldline_1st
!
!
      implicit none
!
!>      Instance for FEM field data IO
      type(ucd_data), save :: input_ucd
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
      call set_control_params_4_viz(my_rank, ierr, input_ucd)
!
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!  FEM Initialization
      call FEM_initialize_fline(input_ucd)
!
!  VIZ Initialization
      call init_visualize_fline
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
        call FEM_analyze_fline(i_step, istep_fline, input_ucd)
!
!  Generate field lines
        if(istep_fline .ge. 0) call visualize_fline(istep_fline)
      end do
!
      end subroutine analyze
!
!  ---------------------------------------------------------------------
!
      end module analyzer_fline

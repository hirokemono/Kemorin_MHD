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
      use fieldline_1st
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
      call FEM_initialize_fline                                         &
     &   (ele_4_nod_VIZ, jac_VIZ_l, jac_VIZ_q, ucd_VIZ)
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
        call FEM_analyze_fline(i_step, istep_fline, ucd_VIZ)
!
!  Generate field lines
        if(istep_fline .ge. 0) then
          call visualize_fline(istep_fline, ele_4_nod_VIZ)
        end if
      end do
!
      end subroutine analyze
!
!  ---------------------------------------------------------------------
!
      end module analyzer_fline

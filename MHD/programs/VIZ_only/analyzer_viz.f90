!analyzer_viz.f90
!      module analyzer_viz
!
!     Written by H. Matsui on July, 2006
!
!      subroutine init_analyzer
!      subroutine analyze
!
      module analyzer_viz
!
      use m_precision
      use m_machine_parameter
!
      use FEM_analyzer_viz
      use visualizer_all
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
      subroutine init_analyzer
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
      call set_control_params_4_viz(my_rank, ierr)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!
!  FEM Initialization
      if(iflag_debug .gt. 0)  write(*,*) 'FEM_initialize_vizs'
      call FEM_initialize_vizs
!
!  VIZ Initialization
      call init_visualize
!
      end subroutine init_analyzer
!
!  ---------------------------------------------------------------------
!
      subroutine analyze
!
      integer(kind=kint ) :: i_step, visval
      integer(kind=kint ) :: istep_psf, istep_iso
      integer(kind=kint ) :: istep_pvr, istep_fline
!
!
      do i_step = i_step_init, i_step_number
!  Load field data
        call FEM_analyze_vizs(i_step,                                   &
     &      istep_psf, istep_iso, istep_pvr, istep_fline, visval)
!
!  Rendering
        if(visval .eq. 0) then
          call visualize_all(istep_psf, istep_iso, istep_pvr,           &
     &        istep_fline)
        end if
      end do
!
      end subroutine analyze
!
!  ---------------------------------------------------------------------
!
      end module analyzer_viz

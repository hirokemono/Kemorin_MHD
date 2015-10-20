!
!      module analyzer_psf
!
!     Written by H. Matsui on July, 2006
!
!      subroutine init_analyzer
!      subroutine analyze
!
      module analyzer_psf
!
      use m_precision
!
      use t_ucd_data
!
      use FEM_analyzer_viz_surf
      use sections_for_1st
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
      subroutine init_analyzer
!
      use calypso_mpi
      use m_control_data_section_only
      use set_control_visualizer
!
      integer(kind = kint) :: ierr
!
!     read controls
!
      if (iflag_debug.gt.0) write(*,*) 'set_control_params_4_viz'
      call read_control_data_section_only
      call set_control_params_4_viz(my_rank, ierr, input_ucd)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!  FEM Initialization
      call FEM_initialize_surface(input_ucd)
!
!  VIZ Initialization
      call init_visualize_surface
!
      end subroutine init_analyzer
!
!  ---------------------------------------------------------------------
!
      subroutine analyze
!
      use m_t_step_parameter
!
      integer(kind=kint ) :: i_step, istep_psf, istep_iso
!
!
      do i_step = i_step_init, i_step_number
!
!  Load field data
        call FEM_analyze_surface                                        &
     &     (i_step, istep_psf, istep_iso, input_ucd)
!
!  Generate field lines
        if(istep_psf.ge.0 .or. istep_iso.ge.0) then
           call visualize_surface(istep_psf, istep_iso)
        end if
      end do
!
      end subroutine analyze
!
!  ---------------------------------------------------------------------
!
      end module analyzer_psf

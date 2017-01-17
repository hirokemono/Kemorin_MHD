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
      use m_work_time
      use m_visualization
!
      use FEM_analyzer_viz_surf
      use sections_for_1st
 !
      implicit none
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
      num_elapsed = 68
      call allocate_elapsed_times
!
      elapse_labels(12) = 'Visualizatio time         '
!
      elapse_labels(60) = 'Sectioning initialization.    '
      elapse_labels(61) = 'Isosurfaceing initialization.    '
      elapse_labels(62) = 'Volume rendering initialization.    '
      elapse_labels(63) = 'fieldline initialization.    '
!
      elapse_labels(65) = 'Sectioning.    '
      elapse_labels(66) = 'Isosurfaceing.    '
      elapse_labels(67) = 'Volume rendering.    '
      elapse_labels(68) = 'fieldline.    '
!
      elapse_labels(num_elapsed) = 'Communication time        '
!
!     read controls
!
      if (iflag_debug.gt.0) write(*,*) 'set_control_params_4_viz'
      call read_control_data_section_only
      call set_control_params_4_viz(my_rank, t_sect_ctl, sect_plt,      &
     &    mesh_file_VIZ, ucd_VIZ, ierr)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!  FEM Initialization
      call FEM_initialize_surface
!
!  VIZ Initialization
      call init_visualize_surface(femmesh_VIZ%mesh, femmesh_VIZ%group,  &
     &    elemesh_VIZ, field_VIZ)
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
        call FEM_analyze_surface(i_step, istep_psf, istep_iso)
!
!  Generate field lines
        call start_eleps_time(12)
        call visualize_surface(istep_psf, istep_iso, femmesh_VIZ%mesh,  &
     &      elemesh_VIZ, field_VIZ)
        call end_eleps_time(12)
      end do
!
      call output_elapsed_times
!
      end subroutine analyze
!
!  ---------------------------------------------------------------------
!
      end module analyzer_psf

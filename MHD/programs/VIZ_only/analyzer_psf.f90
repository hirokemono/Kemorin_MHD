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
      use t_viz_sections
!
      implicit none
!
      type(surfacing_modules), save :: viz_psfs_v
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
     &    mesh_file_VIZ, ucd_file_VIZ, ierr)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!  FEM Initialization
      call FEM_initialize_surface(ucd_file_VIZ)
!
!  VIZ Initialization
      call init_visualize_surface(femmesh_VIZ, elemesh_VIZ, field_VIZ,  &
     &    sect_psf_ctls, sect_iso_ctls, viz_psfs_v)
!
      end subroutine init_analyzer
!
!  ---------------------------------------------------------------------
!
      subroutine analyze
!
      integer(kind=kint ) :: i_step
!
!
      do i_step = t_VIZ%init_d%i_time_step, t_VIZ%finish_d%i_end_step
        if(set_IO_step_flag(i_step,t_VIZ%ucd_step) .ne. izero) cycle
!
!  Load field data
        call FEM_analyze_surface                                        &
     &     (i_step, ucd_file_VIZ, t_VIZ, viz_step_V)
!
!  Generate field lines
        call start_elapsed_time(12)
        call visualize_surface(viz_step_V, t_VIZ%time_d,                 &
     &      femmesh_VIZ, elemesh_VIZ, field_VIZ, viz_psfs_v)
        call end_elapsed_time(12)
      end do
!
      call output_elapsed_times
!
      end subroutine analyze
!
!  ---------------------------------------------------------------------
!
      end module analyzer_psf

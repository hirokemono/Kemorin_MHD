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
      use m_elapsed_labels_4_VIZ
!
      integer(kind = kint) :: ierr
!
      num_elapsed = 0
      call allocate_elapsed_times
      call elpsed_label_4_VIZ
      call append_COMM_TIME_to_elapsed
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
        if(output_IO_flag(i_step,t_VIZ%ucd_step) .ne. izero) cycle
        call set_IO_step_flag(i_step,t_VIZ%ucd_step)
!
!  Load field data
        call FEM_analyze_surface                                        &
     &     (i_step, ucd_file_VIZ, t_VIZ, viz_step_V)
!
!  Generate field lines
        call start_elapsed_time(1)
        call visualize_surface(viz_step_V, t_VIZ%time_d,                 &
     &      femmesh_VIZ, elemesh_VIZ, field_VIZ, viz_psfs_v)
        call end_elapsed_time(1)
      end do
!
      call copy_COMM_TIME_to_elaps
      call output_elapsed_times
!
      end subroutine analyze
!
!  ---------------------------------------------------------------------
!
      end module analyzer_psf

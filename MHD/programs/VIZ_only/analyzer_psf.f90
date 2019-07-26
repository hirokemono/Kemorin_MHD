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
      use t_control_data_section_only
!
      implicit none
!
      type(control_data_section_only), save :: sec_viz_ctl1
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
      use m_elapsed_labels_4_VIZ
      use m_elapsed_labels_SEND_RECV
!
      integer(kind = kint) :: ierr
!
      call init_elapse_time_by_TOTAL
      call elpsed_label_4_VIZ
      call elpsed_label_field_send_recv
!
!     read controls
!
      if (iflag_debug.gt.0) write(*,*) 'set_control_params_4_viz'
      if(iflag_TOT_time) call start_elapsed_time(ied_total_elapsed)
      call read_control_data_section_only(sec_viz_ctl1)
      call set_control_params_4_viz                                     &
     &   (sec_viz_ctl1%t_sect_ctl, sec_viz_ctl1%sect_plt,               &
     &    mesh_file_VIZ, ucd_file_VIZ, t_VIZ, viz_step_V, ierr)
      if(ierr .gt. 0) call calypso_MPI_abort(ierr, e_message)
!
!  FEM Initialization
      call FEM_initialize_surface(ucd_file_VIZ)
!
!  VIZ Initialization
      call init_visualize_surface(femmesh_VIZ, field_VIZ,               &
     &    sec_viz_ctl1%sect_psf_ctls, sec_viz_ctl1%sect_iso_ctls,       &
     &    viz_psfs_v)
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
        call visualize_surface(viz_step_V, t_VIZ%time_d,                &
     &      femmesh_VIZ, field_VIZ, viz_psfs_v)
      end do
!
      if(iflag_TOT_time) call end_elapsed_time(ied_total_elapsed)
      call output_elapsed_times
!
      end subroutine analyze
!
!  ---------------------------------------------------------------------
!
      end module analyzer_psf

!visualizer_all.f90
!      module visualizer_all
!
!      Written by H. Matsui on July, 2006
!
      module visualizer_all
!
      use m_precision
!
      use m_machine_parameter
      use m_parallel_var_dof
!
      use m_control_params_4_psf
      use m_control_params_4_iso
      use m_control_params_4_pvr
      use m_control_params_4_fline
!
      use sections_for_1st
      use fieldline_1st
      use volume_rendering_1st
!
      implicit  none
!
!      subroutine psf_init(ierr)
!      subroutine psf_main(istep_psf, ierr)
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_visualize(ierror)
!
      use m_quad_2_triangle
      use m_control_data_sections
      use m_control_data_pvrs
      use m_control_data_flines
      use set_psf_case_table
!
      integer(kind = kint), intent(inout) :: ierror
!
!
      if ( (num_psf_ctl+num_iso_ctl+num_pvr_ctl) .gt. 0) then
        if (iflag_debug.eq.1)  write(*,*) 'set_sectioning_case_table'
        call set_sectioning_case_table
      end if
!
      num_psf = num_psf_ctl
      if (num_psf .gt. 0)  call cross_section_init_1st
      call time_prog_barrier
!
      num_iso = num_iso_ctl
      if (num_iso .gt. 0) call isosurface_init_1st
      call time_prog_barrier
!
      num_pvr = num_pvr_ctl
      if (num_pvr .gt. 0) call pvr_init_1st
      call time_prog_barrier
!
      num_fline = num_fline_ctl
      if (num_fline .gt. 0) call field_line_init_1st
      call time_prog_barrier
!
      ierror = ierr
!
      end subroutine init_visualize
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_all(istep_psf, istep_iso, istep_pvr,         &
     &          istep_fline, ierror)
!
      integer(kind = kint), intent(in) :: istep_psf, istep_iso
      integer(kind = kint), intent(in) :: istep_pvr, istep_fline
      integer(kind = kint), intent(inout) :: ierror
!
!
      if (num_psf.gt.0 .and. istep_psf.gt.0) then
        call cross_section_main_1st(istep_psf)
      end if
      if (num_iso.gt.0 .and. istep_iso.gt.0) then
        call isosurface_main_1st(istep_iso)
      end if
      if (num_pvr.gt.0 .and. istep_pvr.gt.0) then
        call pvr_main_1st(istep_pvr)
      end if
      if (num_fline.gt.0 .and. istep_fline.gt.0) then
        call field_line_main_1st(istep_fline)
      end if
!
      ierror = ierr
!
      end subroutine visualize_all
!
!  ---------------------------------------------------------------------
!
      end module visualizer_all

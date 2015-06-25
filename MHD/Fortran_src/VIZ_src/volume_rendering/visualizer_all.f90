!visualizer_all.f90
!      module visualizer_all
!
!      Written by H. Matsui on July, 2006
!
!      subroutine init_visualize
!      subroutine visualize_all(istep_psf, istep_iso, istep_pvr,        &
!     &          istep_fline)
!
      module visualizer_all
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
!
      use m_control_params_4_fline
!
      use volume_rendering
      use sections_for_1st
      use fieldline_1st
      use volume_rendering_1st
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine init_visualize
!
      use m_quad_2_triangle
      use m_control_data_sections
      use m_control_data_pvrs
      use m_control_data_flines
      use m_cross_section
      use m_isosurface
      use set_psf_case_table
!
!
      if ( (num_psf_ctl+num_iso_ctl+num_pvr_ctl) .gt. 0) then
        if (iflag_debug.eq.1)  write(*,*) 'set_sectioning_case_table'
        call set_sectioning_case_table
      end if
!
      num_psf = num_psf_ctl
      if (num_psf .gt. 0)  call cross_section_init_1st
!
      num_iso = num_iso_ctl
      if (num_iso .gt. 0) call isosurface_init_1st
!
      num_pvr = num_pvr_ctl
      if (num_pvr .gt. 0) call pvr_init_1st
!
      num_fline = num_fline_ctl
      if (num_fline .gt. 0) call field_line_init_1st
!
      end subroutine init_visualize
!
!  ---------------------------------------------------------------------
!
      subroutine visualize_all(istep_psf, istep_iso, istep_pvr,         &
     &          istep_fline)
!
      use m_cross_section
      use m_isosurface
!
      integer(kind = kint), intent(in) :: istep_psf, istep_iso
      integer(kind = kint), intent(in) :: istep_pvr, istep_fline
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
      end subroutine visualize_all
!
!  ---------------------------------------------------------------------
!
      end module visualizer_all

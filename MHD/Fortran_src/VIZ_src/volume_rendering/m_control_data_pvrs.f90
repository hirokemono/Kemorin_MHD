!
!      module m_control_data_pvrs
!
!      Written by H. Matsui on July, 2006
!
!      subroutine allocate_pvr_ctl_struct
!      subroutine deallocate_pvr_file_header_ctl
!
!      subroutine read_viz_control_data
!      subroutine bcast_viz_control_data
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array  volume_rendering  1
!!      file  volume_rendering  'ctl_pvr_temp'
!!    end array volume_rendering
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
      module m_control_data_pvrs
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use t_control_data_pvrs
!
      implicit  none
!
!
      type(volume_rendering_controls), save :: pvr_ctls1
!
!     label for entry
!
      character(len=kchara), parameter                                  &
     &                    :: hd_viz_control = 'visual_control'
      integer (kind=kint) :: i_viz_control = 0
!
!     lavel for volume rendering
!
!     Top level
      character(len=kchara), parameter                                  &
     &             :: hd_section_ctl = 'cross_section_ctl'
      character(len=kchara), parameter                                  &
     &             :: hd_isosurf_ctl = 'isosurface_ctl'
!
!      Deprecated labels
      character(len=kchara), parameter                                  &
     &             :: hd_psf_ctl = 'surface_rendering'
      character(len=kchara), parameter                                  &
     &             :: hd_iso_ctl = 'isosurf_rendering'
      character(len=kchara), parameter                                  &
     &             :: hd_pvr_ctl = 'volume_rendering'
      character(len=kchara), parameter :: hd_fline_ctl =  'fieldline'
      private :: hd_section_ctl, hd_psf_ctl
      private :: hd_isosurf_ctl, hd_iso_ctl
      private :: hd_pvr_ctl, hd_fline_ctl
!
      private :: hd_viz_control, i_viz_control
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_viz_control_data
!
      use m_read_control_elements
!
      use m_control_data_sections
      use m_control_data_flines
!
      use skip_comment_f
!
!
      if(right_begin_flag(hd_viz_control) .eq. 0) return
      if (i_viz_control .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_viz_control, i_viz_control)
        if(i_viz_control .eq. 1) exit
!
        call find_control_array_flag(hd_psf_ctl, psf_ctls1%num_psf_ctl)
        if(psf_ctls1%num_psf_ctl .gt. 0) then
          call read_files_4_psf_ctl(psf_ctls1)
        end if
!
        call find_control_array_flag                                    &
     &     (hd_section_ctl, psf_ctls1%num_psf_ctl)
        if(psf_ctls1%num_psf_ctl .gt. 0) then
          call read_files_4_psf_ctl(psf_ctls1)
        end if
!
        call find_control_array_flag(hd_iso_ctl, iso_ctls1%num_iso_ctl)
        if(iso_ctls1%num_iso_ctl .gt. 0) then
          call read_files_4_iso_ctl(iso_ctls1)
        end if
!
        call find_control_array_flag                                    &
     &     (hd_isosurf_ctl, iso_ctls1%num_iso_ctl)
        if(iso_ctls1%num_iso_ctl .gt. 0) then
          call read_files_4_iso_ctl(iso_ctls1)
        end if
!
        call find_control_array_flag(hd_pvr_ctl, pvr_ctls1%num_pvr_ctl)
        if(pvr_ctls1%num_pvr_ctl .gt. 0) then
          call read_files_4_pvr_ctl(pvr_ctls1)
        end if
!
        call find_control_array_flag                                    &
     &     (hd_fline_ctl, fline_ctls1%num_fline_ctl)
        if(fline_ctls1%num_fline_ctl .gt. 0) then
          call read_files_4_fline_ctl(fline_ctls1)
        end if
      end do
!
      end subroutine read_viz_control_data
!
!   --------------------------------------------------------------------
!
      subroutine bcast_viz_control_data
!
      use m_control_data_sections
      use m_control_data_flines
!
!
      call bcast_files_4_psf_ctl(psf_ctls1)
      call bcast_files_4_iso_ctl(iso_ctls1)
      call bcast_files_4_pvr_ctl(pvr_ctls1)
      call bcast_files_4_fline_ctl(fline_ctls1)
!
      end subroutine bcast_viz_control_data
!
!   --------------------------------------------------------------------
!
      end module m_control_data_pvrs

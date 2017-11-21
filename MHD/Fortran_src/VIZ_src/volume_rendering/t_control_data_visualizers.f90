!
!      module t_control_data_visualizers
!
!      Written by H. Matsui on July, 2006
!
!!      subroutine read_viz_control_data(viz_ctls)
!!      subroutine bcast_viz_control_data(viz_ctls)
!!        type(visualizer_controls), intent(inout) :: viz_ctls
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array  volume_rendering  1
!!      file  volume_rendering  'ctl_pvr_temp'
!!    end array volume_rendering
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
      module t_control_data_visualizers
!
      use m_precision
!
      use m_machine_parameter
      use calypso_mpi
      use t_control_data_pvrs
      use t_control_data_sections
      use t_control_data_flines
!
      implicit  none
!
!
      type visualizer_controls
        type(section_controls)  :: psf_ctls
        type(isosurf_controls)  :: iso_ctls
        type(volume_rendering_controls) :: pvr_ctls
        type(fieldline_controls) :: fline_ctls
      end type visualizer_controls
!
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
      subroutine read_viz_control_data(viz_ctls)
!
      use m_read_control_elements
      use skip_comment_f
!
      type(visualizer_controls), intent(inout) :: viz_ctls
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
        call find_control_array_flag                                    &
     &     (hd_psf_ctl, viz_ctls%psf_ctls%num_psf_ctl)
        if(viz_ctls%psf_ctls%num_psf_ctl .gt. 0) then
          call read_files_4_psf_ctl(viz_ctls%psf_ctls)
        end if
!
        call find_control_array_flag                                    &
     &     (hd_section_ctl, viz_ctls%psf_ctls%num_psf_ctl)
        if(viz_ctls%psf_ctls%num_psf_ctl .gt. 0) then
          call read_files_4_psf_ctl(viz_ctls%psf_ctls)
        end if
!
        call find_control_array_flag                                    &
     &     (hd_iso_ctl, viz_ctls%iso_ctls%num_iso_ctl)
        if(viz_ctls%iso_ctls%num_iso_ctl .gt. 0) then
          call read_files_4_iso_ctl(viz_ctls%iso_ctls)
        end if
!
        call find_control_array_flag                                    &
     &     (hd_isosurf_ctl, viz_ctls%iso_ctls%num_iso_ctl)
        if(viz_ctls%iso_ctls%num_iso_ctl .gt. 0) then
          call read_files_4_iso_ctl(viz_ctls%iso_ctls)
        end if
!
        call find_control_array_flag                                    &
     &     (hd_pvr_ctl, viz_ctls%pvr_ctls%num_pvr_ctl)
        if(viz_ctls%pvr_ctls%num_pvr_ctl .gt. 0) then
          call read_files_4_pvr_ctl(viz_ctls%pvr_ctls)
        end if
!
        call find_control_array_flag                                    &
     &     (hd_fline_ctl, viz_ctls%fline_ctls%num_fline_ctl)
        if(viz_ctls%fline_ctls%num_fline_ctl .gt. 0) then
          call read_files_4_fline_ctl(viz_ctls%fline_ctls)
        end if
      end do
!
      end subroutine read_viz_control_data
!
!   --------------------------------------------------------------------
!
      subroutine bcast_viz_control_data(viz_ctls)
!
      type(visualizer_controls), intent(inout) :: viz_ctls
!
!
      call bcast_files_4_psf_ctl(viz_ctls%psf_ctls)
      call bcast_files_4_iso_ctl(viz_ctls%iso_ctls)
      call bcast_files_4_pvr_ctl(viz_ctls%pvr_ctls)
      call bcast_files_4_fline_ctl(viz_ctls%fline_ctls)
!
      end subroutine bcast_viz_control_data
!
!   --------------------------------------------------------------------
!
      end module t_control_data_visualizers

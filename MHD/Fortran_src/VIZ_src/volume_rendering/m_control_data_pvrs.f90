!
!      module m_control_data_pvrs
!
!      Written by H. Matsui on July, 2006
!
!      subroutine allocate_pvr_ctl_struct
!      subroutine deallocate_pvr_file_header_ctl
!
!      subroutine read_viz_control_data
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
      use m_control_data_4_pvr
!
      implicit  none
!
!
      integer(kind = kint) :: num_pvr_ctl = 0
      character(len = kchara), allocatable :: fname_pvr_ctl(:)
      type(pvr_ctl), pointer, save :: pvr_ctl_struct(:)
!
!     label for entry
!
      character(len=kchara), parameter                                  &
     &                    :: hd_viz_control = 'visual_control'
      integer (kind=kint) :: i_viz_control = 0
!
!     lavel for volume rendering
!
      character(len=kchara), parameter                                  &
     &                    :: hd_pvr_ctl = 'volume_rendering'
      integer (kind=kint) :: i_pvr_ctl =   0
!
      private :: hd_viz_control, i_viz_control, hd_pvr_ctl, i_pvr_ctl
      private :: allocate_pvr_file_header_ctl
      private :: read_files_4_pvr_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_pvr_file_header_ctl
!
      allocate(fname_pvr_ctl(num_pvr_ctl))
!
      end subroutine allocate_pvr_file_header_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_pvr_ctl_struct
!
      allocate(pvr_ctl_struct(num_pvr_ctl))
!
      end subroutine allocate_pvr_ctl_struct
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_pvr_file_header_ctl
!
      deallocate(pvr_ctl_struct)
      deallocate(fname_pvr_ctl)
!
      end subroutine deallocate_pvr_file_header_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine read_viz_control_data
!
      use m_read_control_elements
!
      use m_control_data_sections
      use m_control_data_flines
      use m_control_data_4_psf
      use m_control_data_4_iso
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
        call find_control_array_flag(hd_psf_ctl, num_psf_ctl)
        if(num_psf_ctl .gt. 0) call read_files_4_psf_ctl
        call find_control_array_flag(hd_section_ctl, num_psf_ctl)
        if(num_psf_ctl .gt. 0) call read_files_4_psf_ctl
!
        call find_control_array_flag(hd_iso_ctl, num_iso_ctl)
        if(num_iso_ctl .gt. 0) call read_files_4_iso_ctl
        call find_control_array_flag(hd_isosurf_ctl, num_iso_ctl)
        if(num_iso_ctl .gt. 0) call read_files_4_iso_ctl
!
        call find_control_array_flag(hd_pvr_ctl, num_pvr_ctl)
        if(num_pvr_ctl .gt. 0) call read_files_4_pvr_ctl
!
        call find_control_array_flag(hd_fline_ctl, num_fline_ctl)
        if(num_fline_ctl .gt. 0) call read_files_4_fline_ctl
      end do
!
      end subroutine read_viz_control_data
!
!   --------------------------------------------------------------------
!
      subroutine read_files_4_pvr_ctl
!
      use m_read_control_elements
      use skip_comment_f
!
!
      if (i_pvr_ctl .gt. 0) return
!
      call allocate_pvr_file_header_ctl
      do
        call load_ctl_label_and_line
!
        call find_control_end_array_flag(hd_pvr_ctl,                    &
     &      num_pvr_ctl, i_pvr_ctl)
        if(i_pvr_ctl .ge. num_pvr_ctl) exit
!
        if(right_file_flag(hd_pvr_ctl) .gt. 0) then
          call read_file_names_from_ctl_line(num_pvr_ctl,               &
     &        i_pvr_ctl, fname_pvr_ctl)
        end if
!
        if(right_begin_flag(hd_pvr_ctl) .gt. 0) then
          i_pvr_ctl = i_pvr_ctl + 1
          fname_pvr_ctl(i_pvr_ctl) = 'NO_FILE'
          call read_vr_psf_ctl(pvr_ctl_struct(i_pvr_ctl))
        end if
      end do
!
      end subroutine read_files_4_pvr_ctl
!
!   --------------------------------------------------------------------
!
      end module m_control_data_pvrs

!
!      module m_control_data_flines
!
!      Written by H. Matsui on July, 2006
!
!      subroutine allocate_fline_ctl_struct
!      subroutine deallocate_fline_fhead_ctl
!
!      subroutine read_files_4_fline_ctl
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!    array  fieldline  1
!!      file  fieldline  'ctl_fline_magne'
!!    end array fieldline
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      module m_control_data_flines
!
      use m_precision
!
      use m_machine_parameter
      use m_control_data_4_fline
!
      implicit  none
!
!
      integer(kind = kint) :: num_fline_ctl = 0
      character(len = kchara), allocatable :: fname_fline_ctl(:)
      type(fline_ctl), pointer :: fline_ctl_struct(:)
!
!      fieldline flag
!
      character(len=kchara), parameter :: hd_fline_ctl =  'fieldline'
      integer (kind=kint) :: i_fline_ctl =  0
!
      private :: i_fline_ctl
!
      private :: allocate_fline_fhead_ctl
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_fline_fhead_ctl
!
      allocate(fname_fline_ctl(num_fline_ctl))
!
      end subroutine allocate_fline_fhead_ctl
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_fline_ctl_struct
!
      allocate(fline_ctl_struct(num_fline_ctl))
!
      end subroutine allocate_fline_ctl_struct
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_fline_fhead_ctl
!
      deallocate(fname_fline_ctl)
      deallocate(fline_ctl_struct)
!
      end subroutine deallocate_fline_fhead_ctl
!
!  ---------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine read_files_4_fline_ctl
!
      use m_read_control_elements
      use skip_comment_f
!
!
!
      if (i_fline_ctl .gt. 0) return
!
      call allocate_fline_fhead_ctl
      do
        call load_ctl_label_and_line
!
        call find_control_end_array_flag(hd_fline_ctl, num_fline_ctl,   &
     &        i_fline_ctl)
        if(i_fline_ctl .ge. num_fline_ctl) exit
!
        if(right_file_flag(hd_fline_ctl) .gt. 0) then
          call read_file_names_from_ctl_line(num_fline_ctl,             &
     &        i_fline_ctl, fname_fline_ctl)
        end if
!
        if(right_begin_flag(hd_fline_ctl) .gt. 0) then
          i_fline_ctl = i_fline_ctl + 1
          fname_fline_ctl(i_fline_ctl) = 'NO_FILE'
          call read_field_line_ctl(fline_ctl_struct(i_fline_ctl))
        end if
      end do
!
      end subroutine read_files_4_fline_ctl
!
!   --------------------------------------------------------------------
!
      end module m_control_data_flines

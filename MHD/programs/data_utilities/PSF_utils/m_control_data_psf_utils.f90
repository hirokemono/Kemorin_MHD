!
!      module m_control_data_psf_utils
!
!      Written by H. Matsui on July, 2006
!
!      subroutine read_control_data_psf_utils(ierr)
!
      module m_control_data_psf_utils
!
      use m_precision
!
      use m_machine_parameter
!
      implicit  none
!
!
      integer(kind = kint), parameter :: viz_ctl_file_code = 11
      character(len = kchara), parameter :: fname_viz_ctl = "ctl_viz"
!
!     Zero level
!
      character(len=kchara), parameter                                  &
     &                    :: hd_viz_only_file = 'visualizer'
      integer (kind=kint) :: i_viz_only_file = 0
!
      private :: viz_ctl_file_code, fname_viz_ctl
      private :: read_psf_vizs_control_data
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_data_psf_utils(ierr)
!
      use m_read_control_elements
      use skip_comment_f
!
      integer(kind = kint), intent(inout) :: ierr
!
!
      ctl_file_code = viz_ctl_file_code
      open (ctl_file_code, file=fname_viz_ctl, status='old' )
!
      call load_ctl_label_and_line
      call read_psf_vizs_control_data(ierr)
!
      close(ctl_file_code)
!
      end subroutine read_control_data_psf_utils
!
!   --------------------------------------------------------------------
!
      subroutine read_psf_vizs_control_data(ierr)
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_time_steps
      use m_control_data_sections
      use m_read_control_elements
!
      use skip_comment_f
!
      integer(kind=kint), intent(inout) :: ierr
      integer(kind=kint) :: iflag
!
!
      ierr = 0
      if(right_begin_flag(hd_viz_only_file) .eq. 0) return
      if (i_viz_only_file .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_viz_only_file, i_viz_only_file)
        if(i_viz_only_file .eq. 1) exit
!
        call read_ctl_data_4_platform(plt1)
        call read_time_step_ctl
!
        call read_sections_control_data
      end do
!
      end subroutine read_psf_vizs_control_data
!
!   --------------------------------------------------------------------
!
      end module m_control_data_psf_utils

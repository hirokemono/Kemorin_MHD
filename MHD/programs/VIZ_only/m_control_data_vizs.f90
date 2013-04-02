!
!      module m_control_data_vizs
!
!      Written by H. Matsui on July, 2006
!
!      subroutine read_control_data_vizs
!
!!   --------------------------------------------------------------------
!!    Example of control block
!!
!!  begin visualizer
!!    begin data_files_def
!!      ...
!!    end data_files_def
!!
!!    begin time_step_ctl
!!      ...
!!    end time_step_ctl
!!
!!    begin visual_control
!!      ...
!!    end  visual_control
!!  end  visualizer
!!
!!    -------------------------------------------------------------------
!
!
      module m_control_data_vizs
!
      use m_precision
      use m_machine_parameter
      use m_parallel_var_dof
!
      implicit  none
!
!
      integer(kind = kint), parameter :: viz_ctl_file_code = 11
      character(len = kchara), parameter :: fname_viz_ctl = "ctl_viz"
!
!     Top level
!
      character(len=kchara), parameter                                  &
     &                    :: hd_viz_only_file = 'visualizer'
      integer (kind=kint) :: i_viz_only_file = 0
!
!
      private :: viz_ctl_file_code, fname_viz_ctl
!
      private :: read_vizs_control_data
!
!   --------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine read_control_data_vizs
!
      use m_read_control_elements
      use skip_comment_f
!
!
      ctl_file_code = viz_ctl_file_code
      open (ctl_file_code, file=fname_viz_ctl, status='old' )
!
      call load_ctl_label_and_line
      call read_vizs_control_data
!
      close(ctl_file_code)
!
      end subroutine read_control_data_vizs
!
!   --------------------------------------------------------------------
!
      subroutine read_vizs_control_data
!
      use m_ctl_data_4_platforms
      use m_ctl_data_4_time_steps
      use m_read_control_elements
!
      use skip_comment_f
      use m_control_data_pvrs
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
        call read_ctl_data_4_platform
        call read_time_step_ctl
!
        call read_viz_control_data
      end do
!
      end subroutine read_vizs_control_data
!
!   --------------------------------------------------------------------
!
      end module m_control_data_vizs

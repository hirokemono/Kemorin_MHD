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
      use t_ctl_data_4_time_steps
!
      implicit  none
!
!
      integer(kind = kint), parameter :: viz_ctl_file_code = 11
      character(len = kchara), parameter :: fname_viz_ctl = "ctl_viz"
!
!>      Structure for time stepping control
      type(time_data_control), save :: t_viz_ctl
!
!     Top level
!
      character(len=kchara), parameter                                  &
     &                    :: hd_viz_only_file = 'visualizer'
      integer (kind=kint) :: i_viz_only_file = 0
!
      character(len=kchara), parameter                                  &
     &      :: hd_time_step = 'time_step_ctl'
      integer (kind=kint) :: i_tstep =      0
!
      private :: hd_viz_only_file, i_viz_only_file
      private :: hd_time_step, i_tstep
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
      use m_ctl_data_4_platforms
      use m_control_data_pvrs
      use m_read_control_elements
      use skip_comment_f
      use bcast_4_platform_ctl
      use bcast_4_time_step_ctl
!
!
      if(my_rank .eq. 0) then
        ctl_file_code = viz_ctl_file_code
        open (ctl_file_code, file=fname_viz_ctl, status='old' )
!
        call load_ctl_label_and_line
        call read_vizs_control_data
!
        close(ctl_file_code)
      end if
!
      call bcast_ctl_data_4_platform(plt1)
      call bcast_ctl_data_4_time_step(t_viz_ctl)
      call bcast_viz_control_data
!
      end subroutine read_control_data_vizs
!
!   --------------------------------------------------------------------
!
      subroutine read_vizs_control_data
!
      use m_ctl_data_4_platforms
      use m_control_data_pvrs
      use m_read_control_elements
!
      use skip_comment_f
!
!
      if(right_begin_flag(hd_viz_only_file) .eq. 0) return
      if (i_viz_only_file .gt. 0) return
      do
        call load_ctl_label_and_line
!
        call find_control_end_flag(hd_viz_only_file, i_viz_only_file)
        if(i_viz_only_file .eq. 1) exit
!
        call read_ctl_data_4_platform
        call read_control_time_step_data                                &
     &     (hd_time_step, i_tstep, t_viz_ctl)
!
        call read_viz_control_data
      end do
!
      end subroutine read_vizs_control_data
!
!   --------------------------------------------------------------------
!
      end module m_control_data_vizs

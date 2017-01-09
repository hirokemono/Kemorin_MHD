!m_ctl_data_node_monitor.f90
!      module m_ctl_data_node_monitor
!
!        programmed by H.Matsui on March. 2006
!
!!      subroutine read_monitor_data_control
!!
!!   --------------------------------------------------------------------
!!
!!   control for monitor nodal data
!!
!!  begin monitor_data_ctl
!!    array monitor_grp_ctl   1
!!      monitor_grp_ctl     mid_equator   end
!!    end array
!!  end  monitor_data_ctl
!!
!!   --------------------------------------------------------------------
!
      module m_ctl_data_node_monitor
!
      use m_precision
      use t_ctl_data_node_monitor
!
      implicit  none
!
!
!>      Structure for monitoring plave list
      type(node_monitor_control), save :: nmtr_ctl1
!
!   entry label
!
      character(len=kchara), parameter                                  &
     &      :: hd_monitor_data = 'monitor_data_ctl'
      integer (kind=kint) :: i_monitor_data = 0
!
      private :: hd_monitor_data, i_monitor_data
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine read_monitor_data_control
!
!
      call read_monitor_data_ctl                                        &
     &   (hd_monitor_data, i_monitor_data, nmtr_ctl1)
!
      end subroutine read_monitor_data_control
!
!   --------------------------------------------------------------------
!
      end module m_ctl_data_node_monitor

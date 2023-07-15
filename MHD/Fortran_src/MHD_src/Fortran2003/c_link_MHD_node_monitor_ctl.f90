!>@file   c_link_MHD_node_monitor_ctl.f90
!!@brief  module c_link_MHD_node_monitor_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!!
!>@brief C binding routines for sphere_data_control structure
!!@verbatim
!!      type(c_ptr) function c_node_monitor_ctl_block_name(c_ctl)       &
!!     &          bind(C, NAME = 'c_node_monitor_ctl_block_name')
!!      type(c_ptr) function c_node_monitor_ctl_iflag(c_ctl)            &
!!     &          bind(C, NAME = 'c_node_monitor_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_node_monitor_xx_ctl(c_ctl)               &
!!     &          bind(C, NAME = 'c_node_monitor_xx_ctl')
!!      type(c_ptr) function c_node_monitor_node_ctl(c_ctl)             &
!!     &          bind(C, NAME = 'c_node_monitor_node_ctl')
!!      type(c_ptr) function c_node_monitor_group_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_node_monitor_group_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_MHD_node_monitor_ctl
!
      use iso_c_binding
      use t_ctl_data_node_monitor
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_node_monitor_ctl_block_name(c_ctl)         &
     &          bind(C, NAME = 'c_node_monitor_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(node_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_node_monitor_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_node_monitor_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_node_monitor_ctl_iflag(c_ctl)              &
     &          bind(C, NAME = 'c_node_monitor_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(node_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_node_monitor_ctl_iflag = C_loc(f_ctl%i_monitor_data)
      end function c_node_monitor_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_node_monitor_xx_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_node_monitor_xx_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(node_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_node_monitor_xx_ctl = C_loc(f_ctl%xx_4_monitor_ctl)
      end function c_node_monitor_xx_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_node_monitor_node_ctl(c_ctl)               &
     &          bind(C, NAME = 'c_node_monitor_node_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(node_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_node_monitor_node_ctl = C_loc(f_ctl%node_4_monitor_ctl)
      end function c_node_monitor_node_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_node_monitor_group_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_node_monitor_group_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(node_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_node_monitor_group_ctl = C_loc(f_ctl%group_4_monitor_ctl)
      end function c_node_monitor_group_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_MHD_node_monitor_ctl

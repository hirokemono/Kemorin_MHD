!>@file   c_link_pick_sph_spectr_ctl.f90
!!@brief  module c_link_pick_sph_spectr_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!!
!>@brief C binding routines for sphere_data_control structure
!!@verbatim
!!      type(c_ptr) function c_pick_spectr_ctl_block_name(c_ctl)        &
!!     &          bind(C, NAME = 'c_pick_spectr_ctl_block_name')
!!      type(c_ptr) function c_pick_spectr_ctl_iflag(c_ctl)             &
!!     &          bind(C, NAME = 'c_pick_spectr_ctl_iflag')
!!
!!      type(c_ptr) function c_pick_spectr_ctl_block_name(c_ctl)        &
!!     &          bind(C, NAME = 'c_pick_spectr_ctl_block_name')
!!      type(c_ptr) function c_pick_spectr_ctl_iflag(c_ctl)             &
!!     &          bind(C, NAME = 'c_pick_spectr_ctl_iflag')
!!      type(c_ptr) function c_sph_picked_mode_head_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_sph_picked_mode_head_ctl')
!!      type(c_ptr) function c_sph_picked_mode_fmt_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_sph_picked_mode_fmt_ctl')
!!      type(c_ptr) function c_sph_idx_pick_layer_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_sph_idx_pick_layer_ctl')
!!      type(c_ptr) function c_sph_pick_radius_ctl(c_ctl)               &
!!     &          bind(C, NAME = 'c_sph_pick_radius_ctl')
!!      type(c_ptr) function c_sph_idx_pick_sph_ctl(c_ctl)              &
!!     &          bind(C, NAME = 'c_sph_idx_pick_sph_ctl')
!!      type(c_ptr) function c_sph_idx_pick_sph_l_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_sph_idx_pick_sph_l_ctl')
!!      type(c_ptr) function c_sph_idx_pick_sph_m_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_sph_idx_pick_sph_m_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_pick_sph_spectr_ctl
!
      use iso_c_binding
      use t_ctl_data_pick_sph_spectr
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_pick_spectr_ctl_block_name(c_ctl)          &
     &          bind(C, NAME = 'c_pick_spectr_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pick_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_pick_spectr_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_pick_spectr_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_pick_spectr_ctl_iflag(c_ctl)               &
     &          bind(C, NAME = 'c_pick_spectr_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pick_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_pick_spectr_ctl_iflag = C_loc(f_ctl%i_pick_sph)
      end function c_pick_spectr_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_picked_mode_head_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_sph_picked_mode_head_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pick_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_picked_mode_head_ctl = C_loc(f_ctl%picked_mode_head_ctl)
      end function c_sph_picked_mode_head_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_picked_mode_fmt_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_sph_picked_mode_fmt_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pick_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_picked_mode_fmt_ctl = C_loc(f_ctl%picked_mode_fmt_ctl)
      end function c_sph_picked_mode_fmt_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_idx_pick_layer_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_sph_idx_pick_layer_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pick_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_idx_pick_layer_ctl = C_loc(f_ctl%idx_pick_layer_ctl)
      end function c_sph_idx_pick_layer_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_pick_radius_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_sph_pick_radius_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pick_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_pick_radius_ctl = C_loc(f_ctl%pick_radius_ctl)
      end function c_sph_pick_radius_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_idx_pick_sph_ctl(c_ctl)                &
     &          bind(C, NAME = 'c_sph_idx_pick_sph_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pick_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_idx_pick_sph_ctl = C_loc(f_ctl%idx_pick_sph_ctl)
      end function c_sph_idx_pick_sph_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_idx_pick_sph_l_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_sph_idx_pick_sph_l_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pick_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_idx_pick_sph_l_ctl = C_loc(f_ctl%idx_pick_sph_l_ctl)
      end function c_sph_idx_pick_sph_l_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_idx_pick_sph_m_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_sph_idx_pick_sph_m_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(pick_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_idx_pick_sph_m_ctl = C_loc(f_ctl%idx_pick_sph_m_ctl)
      end function c_sph_idx_pick_sph_m_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_pick_sph_spectr_ctl

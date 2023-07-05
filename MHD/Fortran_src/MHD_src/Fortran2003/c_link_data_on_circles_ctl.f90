!>@file   c_link_data_on_circles_ctl.f90
!!@brief  module c_link_data_on_circles_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!!
!>@brief C binding routines for sphere_data_control structure
!!@verbatim
!!      type(c_ptr) function c_data_on_circle_block_name(c_ctl)         &
!!     &          bind(C, NAME = 'c_data_on_circle_block_name')
!!      type(c_ptr) function c_data_on_circle_iflag(c_ctl)              &
!!     &          bind(C, NAME = 'c_data_on_circle_iflag')
!!      type(c_ptr) function c_data_on_circle_field_file(c_ctl)         &
!!     &          bind(C, NAME = 'c_data_on_circle_field_file')
!!      type(c_ptr) function c_data_on_circle_spectr_file(c_ctl)        &
!!     &          bind(C, NAME = 'c_data_on_circle_spectr_file')
!!      type(c_ptr) function c_data_on_circle_file_fmt_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_data_on_circle_file_fmt_ctl')
!!      type(c_ptr) function c_data_on_circle_coord_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_data_on_circle_coord_ctl')
!!      type(c_ptr) function c_data_on_circle_nphi_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_data_on_circle_nphi_ctl')
!!      type(c_ptr) function c_data_on_circle_pick_s_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_data_on_circle_pick_s_ctl')
!!      type(c_ptr) function c_data_on_circle_pick_z_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_data_on_circle_pick_z_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_data_on_circles_ctl
!
      use iso_c_binding
      use t_ctl_data_circles
      use t_ctl_data_mid_equator
      use ctl_array_chara_to_c
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_data_on_circle_block_name(c_ctl)           &
     &          bind(C, NAME = 'c_data_on_circle_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mid_equator_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_data_on_circle_block_name = C_loc(f_ctl%block_name)
      end function c_data_on_circle_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_data_on_circle_iflag(c_ctl)                &
     &          bind(C, NAME = 'c_data_on_circle_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mid_equator_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_data_on_circle_iflag = C_loc(f_ctl%i_mid_equator_ctl)
      end function c_data_on_circle_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_data_on_circle_field_file(c_ctl)           &
     &          bind(C, NAME = 'c_data_on_circle_field_file')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mid_equator_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_data_on_circle_field_file = C_loc(f_ctl%circle_field_file_ctl)
      end function c_data_on_circle_field_file
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_data_on_circle_spectr_file(c_ctl)          &
     &          bind(C, NAME = 'c_data_on_circle_spectr_file')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mid_equator_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_data_on_circle_spectr_file= C_loc(f_ctl%circle_spectr_file_ctl)
      end function c_data_on_circle_spectr_file
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_data_on_circle_file_fmt_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_data_on_circle_file_fmt_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mid_equator_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_data_on_circle_file_fmt_ctl                                     &
     &           = C_loc(f_ctl%circle_file_format_ctl)
      end function c_data_on_circle_file_fmt_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_data_on_circle_coord_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_data_on_circle_coord_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mid_equator_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_data_on_circle_coord_ctl = C_loc(f_ctl%pick_circle_coord_ctl)
      end function c_data_on_circle_coord_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_data_on_circle_nphi_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_data_on_circle_nphi_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mid_equator_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_data_on_circle_nphi_ctl = C_loc(f_ctl%nphi_mid_eq_ctl)
      end function c_data_on_circle_nphi_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_data_on_circle_pick_s_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_data_on_circle_pick_s_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mid_equator_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_data_on_circle_pick_s_ctl = C_loc(f_ctl%pick_s_ctl)
      end function c_data_on_circle_pick_s_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_data_on_circle_pick_z_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_data_on_circle_pick_z_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mid_equator_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_data_on_circle_pick_z_ctl = C_loc(f_ctl%pick_z_ctl)
      end function c_data_on_circle_pick_z_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_data_on_circles_ctl

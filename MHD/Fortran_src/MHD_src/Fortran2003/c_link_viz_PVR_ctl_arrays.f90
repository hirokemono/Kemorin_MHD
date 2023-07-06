!>@file   c_link_viz_PVR_ctl_arrays.f90
!!@brief  module c_link_viz_PVR_ctl_arrays
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!!
!>@brief C binding routines for sphere_data_control structure
!!@verbatim
!!      type(c_ptr) function c_map_render_ctls_block_name(c_ctl)        &
!!     &          bind(C, NAME = 'c_map_render_ctls_block_name')
!!      integer(c_int) function c_map_render_ctls_num_map_ctl(c_ctl)    &
!!     &          bind(C, NAME = 'c_map_render_ctls_num_map_ctl')
!!      type(c_ptr) function c_map_render_ctls_fname(idx_in, c_ctl)     &
!!     &          bind(C, NAME = 'c_map_render_ctls_fname')
!!      type(c_ptr) function c_map_render_ctls_map_ctl(idx_in, c_ctl)   &
!!     &          bind(C, NAME = 'c_map_render_ctls_map_ctl')
!!
!!      type(c_ptr) function c_append_viz_map_render_ctls               &
!!     &                  (idx, c_name, c_ctl)                          &
!!     &                  bind(C, NAME = 'c_append_viz_map_render_ctls')
!!      type(c_ptr) function c_delete_viz_map_render_ctls(idx, c_ctl)   &
!!     &                  bind(C, NAME = 'c_delete_viz_map_render_ctls')
!!        integer(c_int), value, intent(in) :: num
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_viz_PVR_ctl_arrays
!
      use iso_c_binding
      use t_control_data_maps
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
      type(c_ptr) function c_map_render_ctls_block_name(c_ctl)          &
     &          bind(C, NAME = 'c_map_render_ctls_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_rendering_controls), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_map_render_ctls_block_name = C_loc(f_ctl%block_name)
      end function c_map_render_ctls_block_name
!
!  ---------------------------------------------------------------------
!
      integer(c_int) function c_map_render_ctls_num_map_ctl(c_ctl)      &
     &          bind(C, NAME = 'c_map_render_ctls_num_map_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_rendering_controls), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_map_render_ctls_num_map_ctl = f_ctl%num_map_ctl
      end function c_map_render_ctls_num_map_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_map_render_ctls_fname(idx_in, c_ctl)       &
     &          bind(C, NAME = 'c_map_render_ctls_fname')
      integer(c_int), value, intent(in) :: idx_in
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_rendering_controls), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_map_render_ctls_fname = C_loc(f_ctl%fname_map_ctl(idx_in+1))
      end function c_map_render_ctls_fname
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_map_render_ctls_map_ctl(idx_in, c_ctl)     &
     &          bind(C, NAME = 'c_map_render_ctls_map_ctl')
      integer(c_int), value, intent(in) :: idx_in
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_rendering_controls), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      c_map_render_ctls_map_ctl = C_loc(f_ctl%map_ctl_struct(idx_in+1))
      end function c_map_render_ctls_map_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_append_viz_map_render_ctls                 &
     &                  (idx, c_name, c_ctl)                            &
     &                  bind(C, NAME = 'c_append_viz_map_render_ctls')
!
      integer(c_int), value, intent(in) :: idx
      character(C_char), intent(in) :: c_name(*)
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_rendering_controls), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      call append_map_render_control(idx, copy_char_from_c(c_name),     &
     &                               f_ctl)
      c_append_viz_map_render_ctls = C_loc(f_ctl%map_ctl_struct)
      f_ctl%map_ctl_struct(idx+1)%i_map_ctl = 1
!
      end function c_append_viz_map_render_ctls
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_delete_viz_map_render_ctls(idx, c_ctl)     &
     &                  bind(C, NAME = 'c_delete_viz_map_render_ctls')
      use ctl_data_volume_spectr_IO
!
      integer(c_int), value, intent(in) :: idx
      type(c_ptr), value, intent(in) :: c_ctl
      type(map_rendering_controls), pointer :: f_ctl
!
      call c_f_pointer(c_ctl, f_ctl)
      call delete_map_render_control((idx+1), f_ctl)
      c_delete_viz_map_render_ctls = C_loc(f_ctl%map_ctl_struct)
!
      end function c_delete_viz_map_render_ctls
!
!  ---------------------------------------------------------------------
!
      end module c_link_viz_PVR_ctl_arrays

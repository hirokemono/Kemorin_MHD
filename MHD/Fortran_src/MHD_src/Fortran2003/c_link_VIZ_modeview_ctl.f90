!>@file   c_link_VIZ_modeview_ctl.f90
!!@brief  module c_link_VIZ_modeview_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for forces control structure
!!@verbatim
!!      type(c_ptr) function c_modeview_ctl_block_name(c_ctl)           &
!!     &          bind(C, NAME = 'c_modeview_ctl_block_name')
!!      type(c_ptr) function c_modeview_ctl_iflag(c_ctl)                &
!!     &          bind(C, NAME = 'c_modeview_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_modeview_mat_ctl_fname(c_ctl)            &
!!     &          bind(C, NAME = 'c_modeview_mat_ctl_fname')
!!      type(c_ptr) function c_modeview_ctl_pixel(c_ctl)                &
!!     &          bind(C, NAME = 'c_modeview_ctl_pixel')
!!      type(c_ptr) function c_modeview_ctl_proj(c_ctl)                 &
!!     &          bind(C, NAME = 'c_modeview_ctl_proj')
!!      type(c_ptr) function c_modeview_ctl_streo(c_ctl)                &
!!     &          bind(C, NAME = 'c_modeview_ctl_streo')
!!      type(c_ptr) function c_modeview_modelview_mat_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_modeview_modelview_mat_ctl')
!!      type(c_ptr) function c_modeview_lookpoint_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_modeview_lookpoint_ctl')
!!      type(c_ptr) function c_modeview_viewpoint_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_modeview_viewpoint_ctl')
!!      type(c_ptr) function c_modeview_up_dir_ctl(c_ctl)               &
!!     &          bind(C, NAME = 'c_modeview_up_dir_ctl')
!!      type(c_ptr) function c_modeview_view_rot_vec_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_modeview_view_rot_vec_ctl')
!!      type(c_ptr) function c_modeview_rotation_deg_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_modeview_rotation_deg_ctl')
!!      type(c_ptr) function c_modeview_scale_factor_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_modeview_scale_factor_ctl')
!!      type(c_ptr) function c_modeview_scale_vector_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_modeview_scale_vector_ctl')
!!      type(c_ptr) function c_modeview_viewpt_in_view_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_modeview_viewpt_in_view_ctl')
!!      type(c_ptr) function c_modeview_projection_type_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_modeview_projection_type_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_VIZ_modeview_ctl
!
      use iso_c_binding
      use t_ctl_data_4_view_transfer
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_modeview_ctl_block_name(c_ctl)             &
     &          bind(C, NAME = 'c_modeview_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(modeview_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_modeview_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_modeview_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_modeview_ctl_iflag(c_ctl)                  &
     &          bind(C, NAME = 'c_modeview_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(modeview_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_modeview_ctl_iflag = C_loc(f_ctl%i_view_transform)
      end function c_modeview_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_modeview_mat_ctl_fname(c_ctl)              &
     &          bind(C, NAME = 'c_modeview_mat_ctl_fname')
      type(c_ptr), value, intent(in) :: c_ctl
      type(modeview_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_modeview_mat_ctl_fname = C_loc(f_ctl%mat_ctl_fname)
      end function c_modeview_mat_ctl_fname
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_modeview_ctl_pixel(c_ctl)                  &
     &          bind(C, NAME = 'c_modeview_ctl_pixel')
      type(c_ptr), value, intent(in) :: c_ctl
      type(modeview_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_modeview_ctl_pixel = C_loc(f_ctl%pixel)
      end function c_modeview_ctl_pixel
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_modeview_ctl_proj(c_ctl)                   &
     &          bind(C, NAME = 'c_modeview_ctl_proj')
      type(c_ptr), value, intent(in) :: c_ctl
      type(modeview_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_modeview_ctl_proj = C_loc(f_ctl%proj)
      end function c_modeview_ctl_proj
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_modeview_ctl_streo(c_ctl)                  &
     &          bind(C, NAME = 'c_modeview_ctl_streo')
      type(c_ptr), value, intent(in) :: c_ctl
      type(modeview_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_modeview_ctl_streo = C_loc(f_ctl%streo)
      end function c_modeview_ctl_streo
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_modeview_modelview_mat_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_modeview_modelview_mat_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(modeview_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_modeview_modelview_mat_ctl = C_loc(f_ctl%modelview_mat_ctl)
      end function c_modeview_modelview_mat_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_modeview_lookpoint_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_modeview_lookpoint_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(modeview_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_modeview_lookpoint_ctl = C_loc(f_ctl%lookpoint_ctl)
      end function c_modeview_lookpoint_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_modeview_viewpoint_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_modeview_viewpoint_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(modeview_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_modeview_viewpoint_ctl = C_loc(f_ctl%viewpoint_ctl)
      end function c_modeview_viewpoint_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_modeview_up_dir_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_modeview_up_dir_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(modeview_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_modeview_up_dir_ctl = C_loc(f_ctl%up_dir_ctl)
      end function c_modeview_up_dir_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_modeview_view_rot_vec_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_modeview_view_rot_vec_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(modeview_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_modeview_view_rot_vec_ctl = C_loc(f_ctl%view_rot_vec_ctl)
      end function c_modeview_view_rot_vec_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_modeview_rotation_deg_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_modeview_rotation_deg_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(modeview_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_modeview_rotation_deg_ctl = C_loc(f_ctl%view_rotation_deg_ctl)
      end function c_modeview_rotation_deg_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_modeview_scale_factor_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_modeview_scale_factor_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(modeview_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_modeview_scale_factor_ctl = C_loc(f_ctl%scale_factor_ctl)
      end function c_modeview_scale_factor_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_modeview_scale_vector_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_modeview_scale_vector_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(modeview_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_modeview_scale_vector_ctl = C_loc(f_ctl%scale_vector_ctl)
      end function c_modeview_scale_vector_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_modeview_viewpt_in_view_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_modeview_viewpt_in_view_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(modeview_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_modeview_viewpt_in_view_ctl = C_loc(f_ctl%viewpt_in_viewer_ctl)
      end function c_modeview_viewpt_in_view_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_modeview_projection_type_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_modeview_projection_type_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(modeview_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_modeview_projection_type_ctl = C_loc(f_ctl%projection_type_ctl)
      end function c_modeview_projection_type_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_VIZ_modeview_ctl

!>@file   c_link_VIZ_projection_ctl.f90
!!@brief  module c_link_VIZ_projection_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for forces control structure
!!@verbatim
!!      type(c_ptr) function c_screen_pixel_ctl_block_name(c_ctl)       &
!!     &          bind(C, NAME = 'c_screen_pixel_ctl_block_name')
!!      type(c_ptr) function c_screen_pixel_ctl_iflag(c_ctl)            &
!!     &          bind(C, NAME = 'c_screen_pixel_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_screen_num_xpixel_ctl(c_ctl)             &
!!     &          bind(C, NAME = 'c_screen_num_xpixel_ctl')
!!      type(c_ptr) function c_screen_num_ypixel_ctl(c_ctl)             &
!!     &          bind(C, NAME = 'c_screen_num_ypixel_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!!!
!!      type(c_ptr) function c_projection_ctl_block_name(c_ctl)         &
!!     &          bind(C, NAME = 'c_projection_ctl_block_name')
!!      type(c_ptr) function c_projection_ctl_iflag(c_ctl)              &
!!     &          bind(C, NAME = 'c_projection_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_projection_perspect_agl_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_projection_perspect_agl_ctl')
!!      type(c_ptr) function c_projection_xy_ratio_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_projection_xy_ratio_ctl')
!!      type(c_ptr) function c_projection_near_ctl(c_ctl)               &
!!     &          bind(C, NAME = 'c_projection_near_ctl')
!!      type(c_ptr) function c_projection_far_ctl(c_ctl)                &
!!     &          bind(C, NAME = 'c_projection_far_ctl')
!!      type(c_ptr) function c_projection_horiz_range_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_projection_horiz_range_ctl')
!!      type(c_ptr) function c_projection_vert_range_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_projection_vert_range_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!!!
!!      type(c_ptr) function c_streo_view_ctl_block_name(c_ctl)         &
!!     &          bind(C, NAME = 'c_streo_view_ctl_block_name')
!!      type(c_ptr) function c_streo_view_ctl_iflag(c_ctl)              &
!!     &          bind(C, NAME = 'c_streo_view_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_streo_view_focalpoint_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_streo_view_focalpoint_ctl')
!!      type(c_ptr) function c_streo_view_eye_separate_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_streo_view_eye_separate_ctl')
!!      type(c_ptr) function c_streo_view_eye_sep_angle_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_streo_view_eye_sep_angle_ctl')
!!      type(c_ptr) function c_streo_view_step_eye_sep_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_streo_view_step_eye_sep_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_VIZ_projection_ctl
!
      use iso_c_binding
      use t_ctl_data_4_screen_pixel
      use t_ctl_data_4_projection
      use t_ctl_data_4_streo_view
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_screen_pixel_ctl_block_name(c_ctl)         &
     &          bind(C, NAME = 'c_screen_pixel_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(screen_pixel_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_screen_pixel_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_screen_pixel_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_screen_pixel_ctl_iflag(c_ctl)              &
     &          bind(C, NAME = 'c_screen_pixel_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(screen_pixel_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_screen_pixel_ctl_iflag = C_loc(f_ctl%i_image_size)
      end function c_screen_pixel_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_screen_num_xpixel_ctl(c_ctl)               &
     &          bind(C, NAME = 'c_screen_num_xpixel_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(screen_pixel_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_screen_num_xpixel_ctl = C_loc(f_ctl%num_xpixel_ctl)
      end function c_screen_num_xpixel_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_screen_num_ypixel_ctl(c_ctl)               &
     &          bind(C, NAME = 'c_screen_num_ypixel_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(screen_pixel_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_screen_num_ypixel_ctl = C_loc(f_ctl%num_ypixel_ctl)
      end function c_screen_num_ypixel_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_projection_ctl_block_name(c_ctl)           &
     &          bind(C, NAME = 'c_projection_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(projection_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_projection_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_projection_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_projection_ctl_iflag(c_ctl)                &
     &          bind(C, NAME = 'c_projection_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(projection_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_projection_ctl_iflag = C_loc(f_ctl%i_project_mat)
      end function c_projection_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_projection_perspect_agl_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_projection_perspect_agl_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(projection_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_projection_perspect_agl_ctl= C_loc(f_ctl%perspective_angle_ctl)
      end function c_projection_perspect_agl_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_projection_xy_ratio_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_projection_xy_ratio_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(projection_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_projection_xy_ratio_ctl = C_loc(f_ctl%perspective_xy_ratio_ctl)
      end function c_projection_xy_ratio_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_projection_near_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_projection_near_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(projection_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_projection_near_ctl = C_loc(f_ctl%perspective_near_ctl)
      end function c_projection_near_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_projection_far_ctl(c_ctl)                  &
     &          bind(C, NAME = 'c_projection_far_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(projection_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_projection_far_ctl = C_loc(f_ctl%perspective_far_ctl)
      end function c_projection_far_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_projection_horiz_range_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_projection_horiz_range_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(projection_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_projection_horiz_range_ctl = C_loc(f_ctl%horizontal_range_ctl)
      end function c_projection_horiz_range_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_projection_vert_range_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_projection_vert_range_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(projection_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_projection_vert_range_ctl = C_loc(f_ctl%vertical_range_ctl)
      end function c_projection_vert_range_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_streo_view_ctl_block_name(c_ctl)           &
     &          bind(C, NAME = 'c_streo_view_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(streo_view_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_streo_view_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_streo_view_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_streo_view_ctl_iflag(c_ctl)                &
     &          bind(C, NAME = 'c_streo_view_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(streo_view_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_streo_view_ctl_iflag = C_loc(f_ctl%i_stereo_view)
      end function c_streo_view_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_streo_view_focalpoint_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_streo_view_focalpoint_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(streo_view_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_streo_view_focalpoint_ctl = C_loc(f_ctl%focalpoint_ctl)
      end function c_streo_view_focalpoint_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_streo_view_eye_separate_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_streo_view_eye_separate_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(streo_view_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_streo_view_eye_separate_ctl = C_loc(f_ctl%eye_separation_ctl)
      end function c_streo_view_eye_separate_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_streo_view_eye_sep_angle_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_streo_view_eye_sep_angle_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(streo_view_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_streo_view_eye_sep_angle_ctl = C_loc(f_ctl%eye_sep_angle_ctl)
      end function c_streo_view_eye_sep_angle_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_streo_view_step_eye_sep_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_streo_view_step_eye_sep_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(streo_view_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_streo_view_step_eye_sep_ctl                                     &
     &            = C_loc(f_ctl%step_eye_sep_angle_ctl)
      end function c_streo_view_step_eye_sep_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_VIZ_projection_ctl

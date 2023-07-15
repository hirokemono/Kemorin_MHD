!>@file   c_link_VIZ_FLINE_ctl.f90
!!@brief  module c_link_VIZ_FLINE_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for forces control structure
!!@verbatim
!!      type(c_ptr) function c_VIZ_FLINE_ctl_block_name(c_ctl)          &
!!     &          bind(C, NAME = 'c_VIZ_FLINE_ctl_block_name')
!!      type(c_ptr) function c_VIZ_FLINE_ctl_iflag(c_ctl)               &
!!     &          bind(C, NAME = 'c_VIZ_FLINE_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_VIZ_FLINE_file_head_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_VIZ_FLINE_file_head_ctl')
!!      type(c_ptr) function c_VIZ_FLINE_output_type_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_VIZ_FLINE_output_type_ctl')
!!      type(c_ptr) function c_VIZ_FLINE_field_ctl(c_ctl)               &
!!     &          bind(C, NAME = 'c_VIZ_FLINE_field_ctl')
!!      type(c_ptr) function c_VIZ_FLINE_color_field_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_VIZ_FLINE_color_field_ctl')
!!      type(c_ptr) function c_VIZ_FLINE_color_comp_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_VIZ_FLINE_color_comp_ctl')
!!      type(c_ptr) function c_VIZ_FLINE_area_grp_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_VIZ_FLINE_area_grp_ctl')
!!      type(c_ptr) function c_VIZ_FLINE_starting_type_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_VIZ_FLINE_starting_type_ctl')
!!      type(c_ptr) function c_VIZ_FLINE_selection_type_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_VIZ_FLINE_selection_type_ctl')
!!      type(c_ptr) function c_VIZ_FLINE_line_direction_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_VIZ_FLINE_line_direction_ctl')
!!      type(c_ptr) function c_VIZ_FLINE_start_surf_grp_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_VIZ_FLINE_start_surf_grp_ctl')
!!      type(c_ptr) function c_VIZ_FLINE_num_fieldline_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_VIZ_FLINE_num_fieldline_ctl')
!!      type(c_ptr) function c_VIZ_FLINE_max_line_step_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_VIZ_FLINE_max_line_step_ctl')
!!      type(c_ptr) function c_VIZ_FLINE_seed_point_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_VIZ_FLINE_seed_point_ctl')
!!      type(c_ptr) function c_VIZ_FLINE_seed_surface_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_VIZ_FLINE_seed_surface_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_VIZ_FLINE_ctl
!
      use iso_c_binding
      use t_ctl_data_field_line
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_FLINE_ctl_block_name(c_ctl)            &
     &          bind(C, NAME = 'c_VIZ_FLINE_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(fline_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_FLINE_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_VIZ_FLINE_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_FLINE_ctl_iflag(c_ctl)                 &
     &          bind(C, NAME = 'c_VIZ_FLINE_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(fline_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_FLINE_ctl_iflag = C_loc(f_ctl%i_vr_fline_ctl)
      end function c_VIZ_FLINE_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_FLINE_file_head_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_VIZ_FLINE_file_head_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(fline_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_FLINE_file_head_ctl = C_loc(f_ctl%fline_file_head_ctl)
      end function c_VIZ_FLINE_file_head_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_FLINE_output_type_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_VIZ_FLINE_output_type_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(fline_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_FLINE_output_type_ctl = C_loc(f_ctl%fline_output_type_ctl)
      end function c_VIZ_FLINE_output_type_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_FLINE_field_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_VIZ_FLINE_field_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(fline_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_FLINE_field_ctl = C_loc(f_ctl%fline_field_ctl)
      end function c_VIZ_FLINE_field_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_FLINE_color_field_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_VIZ_FLINE_color_field_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(fline_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_FLINE_color_field_ctl = C_loc(f_ctl%fline_color_field_ctl)
      end function c_VIZ_FLINE_color_field_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_FLINE_color_comp_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_VIZ_FLINE_color_comp_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(fline_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_FLINE_color_comp_ctl = C_loc(f_ctl%fline_color_comp_ctl)
      end function c_VIZ_FLINE_color_comp_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_FLINE_area_grp_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_VIZ_FLINE_area_grp_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(fline_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_FLINE_area_grp_ctl = C_loc(f_ctl%fline_area_grp_ctl)
      end function c_VIZ_FLINE_area_grp_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_FLINE_starting_type_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_VIZ_FLINE_starting_type_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(fline_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_FLINE_starting_type_ctl = C_loc(f_ctl%starting_type_ctl)
      end function c_VIZ_FLINE_starting_type_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_FLINE_selection_type_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_VIZ_FLINE_selection_type_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(fline_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_FLINE_selection_type_ctl = C_loc(f_ctl%selection_type_ctl)
      end function c_VIZ_FLINE_selection_type_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_FLINE_line_direction_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_VIZ_FLINE_line_direction_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(fline_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_FLINE_line_direction_ctl = C_loc(f_ctl%line_direction_ctl)
      end function c_VIZ_FLINE_line_direction_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_FLINE_start_surf_grp_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_VIZ_FLINE_start_surf_grp_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(fline_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_FLINE_start_surf_grp_ctl = C_loc(f_ctl%start_surf_grp_ctl)
      end function c_VIZ_FLINE_start_surf_grp_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_FLINE_num_fieldline_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_VIZ_FLINE_num_fieldline_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(fline_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_FLINE_num_fieldline_ctl = C_loc(f_ctl%num_fieldline_ctl)
      end function c_VIZ_FLINE_num_fieldline_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_FLINE_max_line_step_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_VIZ_FLINE_max_line_step_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(fline_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_FLINE_max_line_step_ctl= C_loc(f_ctl%max_line_stepping_ctl)
      end function c_VIZ_FLINE_max_line_step_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_FLINE_seed_point_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_VIZ_FLINE_seed_point_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(fline_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_FLINE_seed_point_ctl= C_loc(f_ctl%seed_point_ctl)
      end function c_VIZ_FLINE_seed_point_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_FLINE_seed_surface_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_VIZ_FLINE_seed_surface_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(fline_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_FLINE_seed_surface_ctl= C_loc(f_ctl%seed_surface_ctl)
      end function c_VIZ_FLINE_seed_surface_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_VIZ_FLINE_ctl

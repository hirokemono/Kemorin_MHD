!>@file   c_link_MHD_reftemp_ctl.f90
!!@brief  module c_link_MHD_reftemp_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for sphere_data_control structure
!!@verbatim
!!      type(c_ptr) function c_val_r_variation_ctl(c_ctl)               &
!!     &          bind(C, NAME = 'c_val_r_variation_ctl')
!!      type(c_ptr) function c_val_variation_file_name(c_ctl)           &
!!     &          bind(C, NAME = 'c_val_variation_file_name')
!!      type(c_ptr) function c_val_diffusivity_list_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_val_diffusivity_list_ctl')
!!
!!      type(c_ptr) function c_val_diffuse_ICB_radius(c_ctl)            &
!!     &          bind(C, NAME = 'c_val_diffuse_ICB_radius')
!!      type(c_ptr) function c_val_diffuse_ICB_ratio(c_ctl)             &
!!     &          bind(C, NAME = 'c_val_diffuse_ICB_ratio')
!!      type(c_ptr) function c_val_diffuse_ICB_width(c_ctl)             &
!!     &          bind(C, NAME = 'c_val_diffuse_ICB_width')
!!
!!      type(c_ptr) function c_reftemp_point_ctl_block_name(c_ctl)      &
!!     &          bind(C, NAME = 'c_reftemp_point_ctl_block_name')
!!      type(c_ptr) function c_reftemp_point_ctl_iflag(c_ctl)           &
!!     &          bind(C, NAME = 'c_reftemp_point_ctl_iflag')
!!      type(c_ptr) function c_reftemp_point_value_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_reftemp_point_value_ctl')
!!      type(c_ptr) function c_reftemp_point_depth_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_reftemp_point_depth_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_MHD_reftemp_ctl
!
      use iso_c_binding
      use t_ctl_data_valuable_diffuse
      use t_ctl_data_ref_point
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_val_r_variation_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_val_r_variation_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(val_diffuse_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_val_r_variation_ctl = C_loc(f_ctl%r_variation_ctl)
      end function c_val_r_variation_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_val_variation_file_name(c_ctl)             &
     &          bind(C, NAME = 'c_val_variation_file_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(val_diffuse_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_val_variation_file_name = C_loc(f_ctl%variation_file_name)
      end function c_val_variation_file_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_val_diffusivity_list_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_val_diffusivity_list_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(val_diffuse_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_val_diffusivity_list_ctl = C_loc(f_ctl%diffusivity_list_ctl)
      end function c_val_diffusivity_list_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_val_diffuse_ICB_radius(c_ctl)              &
     &          bind(C, NAME = 'c_val_diffuse_ICB_radius')
      type(c_ptr), value, intent(in) :: c_ctl
      type(val_diffuse_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_val_diffuse_ICB_radius = C_loc(f_ctl%ICB_reduction_radius)
      end function c_val_diffuse_ICB_radius
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_val_diffuse_ICB_ratio(c_ctl)               &
     &          bind(C, NAME = 'c_val_diffuse_ICB_ratio')
      type(c_ptr), value, intent(in) :: c_ctl
      type(val_diffuse_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_val_diffuse_ICB_ratio = C_loc(f_ctl%ICB_reduction_ratio)
      end function c_val_diffuse_ICB_ratio
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_val_diffuse_ICB_width(c_ctl)               &
     &          bind(C, NAME = 'c_val_diffuse_ICB_width')
      type(c_ptr), value, intent(in) :: c_ctl
      type(val_diffuse_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_val_diffuse_ICB_width = C_loc(f_ctl%ICB_reduction_width)
      end function c_val_diffuse_ICB_width
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_reftemp_point_ctl_block_name(c_ctl)        &
     &          bind(C, NAME = 'c_reftemp_point_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(reference_point_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_reftemp_point_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_reftemp_point_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_reftemp_point_ctl_iflag(c_ctl)             &
     &          bind(C, NAME = 'c_reftemp_point_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(reference_point_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_reftemp_point_ctl_iflag = C_loc(f_ctl%i_referenced)
      end function c_reftemp_point_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_reftemp_point_value_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_reftemp_point_value_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(reference_point_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_reftemp_point_value_ctl = C_loc(f_ctl%value)
      end function c_reftemp_point_value_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_reftemp_point_depth_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_reftemp_point_depth_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(reference_point_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_reftemp_point_depth_ctl = C_loc(f_ctl%depth)
      end function c_reftemp_point_depth_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_MHD_reftemp_ctl

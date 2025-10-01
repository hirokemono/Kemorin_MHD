!>@file   c_link_MHD_temp_model_ctl.f90
!!@brief  module c_link_MHD_temp_model_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for sphere_data_control structure
!!@verbatim
!!      type(c_ptr) function c_temp_model_ctl_block_name(c_ctl)         &
!!     &          bind(C, NAME = 'c_temp_model_ctl_block_name')
!!      type(c_ptr) function c_temp_model_ctl_iflag(c_ctl)              &
!!     &          bind(C, NAME = 'c_temp_model_ctl_iflag')
!!      type(c_ptr) function c_temp_model_filter_advect_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_temp_model_filter_advect_ctl')
!!      type(c_ptr) function c_temp_model_reference_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_temp_model_reference_ctl')
!!      type(c_ptr) function c_temp_model_stratified_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_temp_model_stratified_ctl')
!!      type(c_ptr) function c_temp_model_ref_file_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_temp_model_ref_file_ctl')
!!      type(c_ptr) function c_temp_model_ICB_diffuse_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_temp_model_ICB_diffuse_ctl')
!!      type(c_ptr) function c_temp_model_low_ctl(c_ctl)                &
!!     &          bind(C, NAME = 'c_temp_model_low_ctl')
!!      type(c_ptr) function c_temp_model_high_ctl(c_ctl)               &
!!     &          bind(C, NAME = 'c_temp_model_high_ctl')
!!      type(c_ptr) function c_temp_model_takepiro_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_temp_model_takepiro_ctl')
!!
!!        type(c_ptr), value, intent(in) :: c_ctl
!!      type(c_ptr) function c_reftemp_point_ctl_block_name(c_ctl)      &
!!     &          bind(C, NAME = 'c_reftemp_point_ctl_block_name')
!!      type(c_ptr) function c_reftemp_point_ctl_iflag(c_ctl)           &
!!     &          bind(C, NAME = 'c_reftemp_point_ctl_iflag')
!!      type(c_ptr) function c_reftemp_point_value_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_reftemp_point_value_ctl')
!!      type(c_ptr) function c_reftemp_point_depth_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_reftemp_point_depth_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_takepiro_model_ctl_block_name(c_ctl)     &
!!     &          bind(C, NAME = 'c_takepiro_model_ctl_block_name')
!!      type(c_ptr) function c_takepiro_model_ctl_iflag(c_ctl)          &
!!     &          bind(C, NAME = 'c_takepiro_model_ctl_iflag')
!!      type(c_ptr) function c_takepiro_stratified_sigma_ctl(c_ctl)     &
!!     &          bind(C, NAME = 'c_takepiro_stratified_sigma_ctl')
!!      type(c_ptr) function c_takepiro_stratified_width_ctl(c_ctl)     &
!!     &          bind(C, NAME = 'c_takepiro_stratified_width_ctl')
!!      type(c_ptr) function c_takepiro_stratified_rout_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_takepiro_stratified_rout_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_MHD_temp_model_ctl
!
      use iso_c_binding
      use t_ctl_data_temp_model
      use t_ctl_data_stratified_model
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_takepiro_model_ctl_block_name(c_ctl)       &
     &          bind(C, NAME = 'c_takepiro_model_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(takepiro_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_takepiro_model_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_takepiro_model_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_takepiro_model_ctl_iflag(c_ctl)            &
     &          bind(C, NAME = 'c_takepiro_model_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(takepiro_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_takepiro_model_ctl_iflag = C_loc(f_ctl%i_takepiro_t_ctl)
      end function c_takepiro_model_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_takepiro_stratified_sigma_ctl(c_ctl)       &
     &          bind(C, NAME = 'c_takepiro_stratified_sigma_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(takepiro_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_takepiro_stratified_sigma_ctl                                   &
     &                       = C_loc(f_ctl%stratified_sigma_ctl)
      end function c_takepiro_stratified_sigma_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_takepiro_stratified_width_ctl(c_ctl)       &
     &          bind(C, NAME = 'c_takepiro_stratified_width_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(takepiro_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_takepiro_stratified_width_ctl                                   &
     &                       = C_loc(f_ctl%stratified_width_ctl)
      end function c_takepiro_stratified_width_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_takepiro_stratified_rout_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_takepiro_stratified_rout_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(takepiro_model_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_takepiro_stratified_rout_ctl                                   &
     &                       = C_loc(f_ctl%stratified_outer_r_ctl)
      end function c_takepiro_stratified_rout_ctl
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
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_temp_model_ctl_block_name(c_ctl)           &
     &          bind(C, NAME = 'c_temp_model_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(reference_temperature_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_temp_model_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_temp_model_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_temp_model_ctl_iflag(c_ctl)                &
     &          bind(C, NAME = 'c_temp_model_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(reference_temperature_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_temp_model_ctl_iflag = C_loc(f_ctl%i_temp_def)
      end function c_temp_model_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_temp_model_filter_advect_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_temp_model_filter_advect_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(reference_temperature_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_temp_model_filter_advect_ctl = C_loc(f_ctl%filterd_advect_ctl)
      end function c_temp_model_filter_advect_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_temp_model_reference_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_temp_model_reference_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(reference_temperature_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_temp_model_reference_ctl = C_loc(f_ctl%reference_ctl)
      end function c_temp_model_reference_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_temp_model_stratified_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_temp_model_stratified_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(reference_temperature_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_temp_model_stratified_ctl = C_loc(f_ctl%stratified_ctl)
      end function c_temp_model_stratified_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_temp_model_ref_file_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_temp_model_ref_file_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(reference_temperature_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_temp_model_ref_file_ctl = C_loc(f_ctl%ref_file_ctl)
      end function c_temp_model_ref_file_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_temp_model_ICB_diffuse_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_temp_model_ICB_diffuse_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(reference_temperature_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_temp_model_ICB_diffuse_ctl                                      &
     &             = C_loc(f_ctl%ICB_diffuse_reduction_ctl)
      end function c_temp_model_ICB_diffuse_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_temp_model_low_ctl(c_ctl)                  &
     &          bind(C, NAME = 'c_temp_model_low_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(reference_temperature_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_temp_model_low_ctl = C_loc(f_ctl%low_ctl)
      end function c_temp_model_low_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_temp_model_high_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_temp_model_high_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(reference_temperature_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_temp_model_high_ctl = C_loc(f_ctl%high_ctl)
      end function c_temp_model_high_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_temp_model_takepiro_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_temp_model_takepiro_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(reference_temperature_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_temp_model_takepiro_ctl = C_loc(f_ctl%takepiro_ctl)
      end function c_temp_model_takepiro_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_MHD_temp_model_ctl

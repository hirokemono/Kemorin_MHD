!>@file   c_link_MHD_takepiro_mdl_ctl.f90
!!@brief  module c_link_MHD_takepiro_mdl_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for sphere_data_control structure
!!@verbatim
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
      module c_link_MHD_takepiro_mdl_ctl
!
      use iso_c_binding
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
!
      end module c_link_MHD_takepiro_mdl_ctl

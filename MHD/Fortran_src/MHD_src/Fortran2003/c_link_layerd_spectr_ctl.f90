!>@file   c_link_layerd_spectr_ctl.f90
!!@brief  module c_link_layerd_spectr_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!!
!>@brief C binding routines for sphere_data_control structure
!!@verbatim
!!      type(c_ptr) function c_sph_l_spectr_ctl_block_name(c_ctl)       &
!!     &          bind(C, NAME = 'c_sph_l_spectr_ctl_block_name')
!!      type(c_ptr) function c_sph_l_spectr_ctl_iflag(c_ctl)            &
!!     &          bind(C, NAME = 'c_sph_l_spectr_ctl_iflag')
!!
!!      type(c_ptr) function c_sph_layer_pwr_spectr_prefix(c_ctl)       &
!!     &          bind(C, NAME = 'c_sph_layer_pwr_spectr_prefix')
!!      type(c_ptr) function c_sph_layer_pwr_spectr_format(c_ctl)       &
!!     &          bind(C, NAME = 'c_sph_layer_pwr_spectr_format')
!!      type(c_ptr) function c_sph_l_spec_degree_switch(c_ctl)          &
!!     &          bind(C, NAME = 'c_sph_l_spec_degree_switch')
!!      type(c_ptr) function c_sph_l_spec_order_switch(c_ctl)           &
!!     &          bind(C, NAME = 'c_sph_l_spec_order_switch')
!!      type(c_ptr) function c_sph_l_spec_diff_lm_switch(c_ctl)         &
!!     &          bind(C, NAME = 'c_sph_l_spec_diff_lm_switch')
!!      type(c_ptr) function c_sph_l_spec_axis_power_switch(c_ctl)      &
!!     &          bind(C, NAME = 'c_sph_l_spec_axis_power_switch')
!!      type(c_ptr) function c_sph_l_spectr_r_idx_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_sph_l_spectr_r_idx_ctl')
!!      type(c_ptr) function c_sph_l_spectr_radius_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_sph_l_spectr_radius_ctl')
!!       type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_layerd_spectr_ctl
!
      use iso_c_binding
      use t_ctl_data_sph_layer_spectr
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_l_spectr_ctl_block_name(c_ctl)         &
     &          bind(C, NAME = 'c_sph_l_spectr_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layerd_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_l_spectr_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_sph_l_spectr_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_l_spectr_ctl_iflag(c_ctl)              &
     &          bind(C, NAME = 'c_sph_l_spectr_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layerd_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_l_spectr_ctl_iflag = C_loc(f_ctl%i_layer_spectr_ctl)
      end function c_sph_l_spectr_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_layer_pwr_spectr_prefix(c_ctl)         &
     &          bind(C, NAME = 'c_sph_layer_pwr_spectr_prefix')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layerd_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_layer_pwr_spectr_prefix                                     &
     &        = C_loc(f_ctl%layered_pwr_spectr_prefix)
      end function c_sph_layer_pwr_spectr_prefix
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_layer_pwr_spectr_format(c_ctl)         &
     &          bind(C, NAME = 'c_sph_layer_pwr_spectr_format')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layerd_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_layer_pwr_spectr_format                                     &
     &        = C_loc(f_ctl%layered_pwr_spectr_format)
      end function c_sph_layer_pwr_spectr_format
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_l_spec_degree_switch(c_ctl)            &
     &          bind(C, NAME = 'c_sph_l_spec_degree_switch')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layerd_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_l_spec_degree_switch= C_loc(f_ctl%degree_spectra_switch)
      end function c_sph_l_spec_degree_switch
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_l_spec_order_switch(c_ctl)             &
     &          bind(C, NAME = 'c_sph_l_spec_order_switch')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layerd_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_l_spec_order_switch = C_loc(f_ctl%order_spectra_switch)
      end function c_sph_l_spec_order_switch
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_l_spec_diff_lm_switch(c_ctl)           &
     &          bind(C, NAME = 'c_sph_l_spec_diff_lm_switch')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layerd_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_l_spec_diff_lm_switch                                       &
     &        = C_loc(f_ctl%diff_lm_spectra_switch)
      end function c_sph_l_spec_diff_lm_switch
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_l_spec_axis_power_switch(c_ctl)        &
     &          bind(C, NAME = 'c_sph_l_spec_axis_power_switch')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layerd_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_l_spec_axis_power_switch = C_loc(f_ctl%axis_power_switch)
      end function c_sph_l_spec_axis_power_switch
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_l_spectr_r_idx_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_sph_l_spectr_r_idx_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layerd_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_l_spectr_r_idx_ctl = C_loc(f_ctl%idx_spec_layer_ctl)
      end function c_sph_l_spectr_r_idx_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_l_spectr_radius_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_sph_l_spectr_radius_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layerd_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_l_spectr_radius_ctl = C_loc(f_ctl%layer_radius_ctl)
      end function c_sph_l_spectr_radius_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_layerd_spectr_ctl

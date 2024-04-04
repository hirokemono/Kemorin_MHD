!>@file   c_link_sph_vol_spectr_ctl.f90
!!@brief  module c_link_sph_vol_spectr_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!!
!>@brief C binding routines for sphere_data_control structure
!!@verbatim
!!      type(c_ptr) function c_sph_v_spectr_ctl_block_name(c_ctl)       &
!!     &          bind(C, NAME = 'c_sph_v_spectr_ctl_block_name')
!!      type(c_ptr) function c_sph_v_spectr_ctl_iflag(c_ctl)            &
!!     &          bind(C, NAME = 'c_sph_v_spectr_ctl_iflag')
!!
!!      type(c_ptr) function c_sph_volume_spec_file_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_sph_volume_spec_file_ctl')
!!      type(c_ptr) function c_sph_volume_ave_file_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_sph_volume_ave_file_ctl')
!!      type(c_ptr) function c_sph_volume_lor_spec_file_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_sph_volume_lor_spec_file_ctl')
!!      type(c_ptr) function c_sph_volume_spec_format_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_sph_volume_spec_format_ctl')
!!      type(c_ptr) function c_sph_degree_v_spectra_switch(c_ctl)       &
!!     &          bind(C, NAME = 'c_sph_degree_v_spectra_switch')
!!      type(c_ptr) function c_sph_order_v_spectra_switch(c_ctl)        &
!!     &          bind(C, NAME = 'c_sph_order_v_spectra_switch')
!!      type(c_ptr) function c_sph_diff_v_lm_spectra_switch(c_ctl)      &
!!     &          bind(C, NAME = 'c_sph_diff_v_lm_spectra_switch')
!!      type(c_ptr) function c_sph_axis_v_power_switch(c_ctl)           &
!!     &          bind(C, NAME = 'c_sph_axis_v_power_switch')
!!
!!      type(c_ptr) function c_sph_v_spec_inner_radius_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_sph_v_spec_inner_radius_ctl')
!!      type(c_ptr) function c_sph_v_spec_outer_radius_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_sph_v_spec_outer_radius_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_sph_vol_spectr_ctl
!
      use iso_c_binding
      use t_ctl_data_sph_vol_spectr
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_v_spectr_ctl_block_name(c_ctl)         &
     &          bind(C, NAME = 'c_sph_v_spectr_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(volume_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_v_spectr_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_sph_v_spectr_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_v_spectr_ctl_iflag(c_ctl)              &
     &          bind(C, NAME = 'c_sph_v_spectr_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(volume_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_v_spectr_ctl_iflag = C_loc(f_ctl%i_vol_spectr_ctl)
      end function c_sph_v_spectr_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_volume_spec_file_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_sph_volume_spec_file_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(volume_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_volume_spec_file_ctl = C_loc(f_ctl%volume_spec_file_ctl)
      end function c_sph_volume_spec_file_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_volume_ave_file_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_sph_volume_ave_file_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(volume_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_volume_ave_file_ctl = C_loc(f_ctl%volume_ave_file_ctl)
      end function c_sph_volume_ave_file_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_volume_lor_spec_file_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_sph_volume_lor_spec_file_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(volume_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_volume_lor_spec_file_ctl                                    &
     &     = C_loc(f_ctl%volume_lor_spec_file_ctl)
      end function c_sph_volume_lor_spec_file_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_volume_spec_format_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_sph_volume_spec_format_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(volume_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_volume_spec_format_ctl = C_loc(f_ctl%volume_spec_format_ctl)
      end function c_sph_volume_spec_format_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_degree_v_spectra_switch(c_ctl)         &
     &          bind(C, NAME = 'c_sph_degree_v_spectra_switch')
      type(c_ptr), value, intent(in) :: c_ctl
      type(volume_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_degree_v_spectra_switch                                     &
     &             = C_loc(f_ctl%degree_v_spectra_switch)
      end function c_sph_degree_v_spectra_switch
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_order_v_spectra_switch(c_ctl)          &
     &          bind(C, NAME = 'c_sph_order_v_spectra_switch')
      type(c_ptr), value, intent(in) :: c_ctl
      type(volume_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_order_v_spectra_switch                                      &
     &             = C_loc(f_ctl%order_v_spectra_switch)
      end function c_sph_order_v_spectra_switch
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_diff_v_lm_spectra_switch(c_ctl)        &
     &          bind(C, NAME = 'c_sph_diff_v_lm_spectra_switch')
      type(c_ptr), value, intent(in) :: c_ctl
      type(volume_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_diff_v_lm_spectra_switch                                    &
     &             = C_loc(f_ctl%diff_v_lm_spectra_switch)
      end function c_sph_diff_v_lm_spectra_switch
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_axis_v_power_switch(c_ctl)             &
     &          bind(C, NAME = 'c_sph_axis_v_power_switch')
      type(c_ptr), value, intent(in) :: c_ctl
      type(volume_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_axis_v_power_switch = C_loc(f_ctl%axis_v_power_switch)
      end function c_sph_axis_v_power_switch
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_v_spec_inner_radius_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_sph_v_spec_inner_radius_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(volume_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_v_spec_inner_radius_ctl = C_loc(f_ctl%inner_radius_ctl)
      end function c_sph_v_spec_inner_radius_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_v_spec_outer_radius_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_sph_v_spec_outer_radius_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(volume_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_v_spec_outer_radius_ctl = C_loc(f_ctl%outer_radius_ctl)
      end function c_sph_v_spec_outer_radius_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_sph_vol_spectr_ctl

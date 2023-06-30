!>@file   c_link_sph_monitor_control.f90
!!@brief  module c_link_sph_monitor_control
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!!
!>@brief C binding routines for sphere_data_control structure
!!@verbatim
!!      type(c_ptr) function c_sph_monitor_ctl_block_name(c_ctl)        &
!!     &          bind(C, NAME = 'c_sph_monitor_ctl_block_name')
!!      type(c_ptr) function c_sph_monitor_ctl_iflag(c_ctl)             &
!!     &          bind(C, NAME = 'c_sph_monitor_ctl_iflag')
!!
!!      integer(c_int) function c_sph_monitor_num_vspec_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_sph_monitor_num_vspec_ctl')
!!      type(c_ptr) function c_sph_monitor_vspec_ctl(idx_in, c_ctl)     &
!!     &          bind(C, NAME = 'c_sph_monitor_vspec_ctl')
!!
!!      type(c_ptr) function c_sph_monitor_lp_ctl(c_ctl)                &
!!     &          bind(C, NAME = 'c_sph_monitor_lp_ctl')
!!      type(c_ptr) function c_sph_monitor_g_pwr(c_ctl)                 &
!!     &          bind(C, NAME = 'c_sph_monitor_g_pwr')
!!      type(c_ptr) function c_sph_monitor_pspec_ctl(c_ctl)             &
!!     &          bind(C, NAME = 'c_sph_monitor_pspec_ctl')
!!      type(c_ptr) function c_sph_monitor_circ_ctls(c_ctl)             &
!!     &          bind(C, NAME = 'c_sph_monitor_circ_ctls')
!!      type(c_ptr) function c_sph_monitor_dbench_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_sph_monitor_dbench_ctl')
!!      type(c_ptr) function c_sph_monitor_fdip_ctl(c_ctl)              &
!!     &          bind(C, NAME = 'c_sph_monitor_fdip_ctl')
!!      type(c_ptr) function c_sph_monitor_vol_ave_prefix(c_ctl)        &
!!     &          bind(C, NAME = 'c_sph_monitor_vol_ave_prefix')
!!      type(c_ptr) function c_sph_monitor_vol_pspec_prefix(c_ctl)      &
!!     &          bind(C, NAME = 'c_sph_monitor_vol_pspec_prefix')
!!      type(c_ptr) function c_sph_mntr_v_pwr_spectr_fmt(c_ctl)         &
!!     &          bind(C, NAME = 'c_sph_mntr_v_pwr_spectr_fmt')
!!      type(c_ptr) function c_sph_mntr_degree_v_spectra_ctl(c_ctl)     &
!!     &          bind(C, NAME = 'c_sph_mntr_degree_v_spectra_ctl')
!!      type(c_ptr) function c_sph_mntr_order_v_spectra_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_sph_mntr_order_v_spectra_ctl')
!!      type(c_ptr) function c_sph_mntr_diff_v_lm_spectr_ctl(c_ctl)     &
!!     &          bind(C, NAME = 'c_sph_mntr_diff_v_lm_spectr_ctl')
!!      type(c_ptr) function c_sph_mntr_axis_v_power_switch(c_ctl)      &
!!     &          bind(C, NAME = 'c_sph_mntr_axis_v_power_switch')
!!      type(c_ptr) function c_sph_mntr_h_Nusselt_file_pfx(c_ctl)       &
!!     &          bind(C, NAME = 'c_sph_mntr_h_Nusselt_file_pfx')
!!      type(c_ptr) function c_sph_mntr_c_Nusselt_file_pfx(c_ctl)       &
!!     &          bind(C, NAME = 'c_sph_mntr_c_Nusselt_file_pfx')
!!      type(c_ptr) function c_sph_mntr_h_Nusselt_file_fmt(c_ctl)       &
!!     &          bind(C, NAME = 'c_sph_mntr_h_Nusselt_file_fmt')
!!      type(c_ptr) function c_sph_mntr_c_Nusselt_file_fmt(c_ctl)       &
!!     &          bind(C, NAME = 'c_sph_mntr_c_Nusselt_file_fmt')
!!      type(c_ptr) function c_sph_mntr_lscale_file_pfix_ctl(c_ctl)     &
!!     &          bind(C, NAME = 'c_sph_mntr_lscale_file_pfix_ctl')
!!      type(c_ptr) function c_sph_mntr_lscale_file_fmt_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_sph_mntr_lscale_file_fmt_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_sph_monitor_control
!
      use iso_c_binding
      use t_ctl_data_mhd_evo_scheme
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_monitor_ctl_block_name(c_ctl)          &
     &          bind(C, NAME = 'c_sph_monitor_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_monitor_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_sph_monitor_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_monitor_ctl_iflag(c_ctl)               &
     &          bind(C, NAME = 'c_sph_monitor_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_monitor_ctl_iflag = C_loc(f_ctl%i_sph_monitor)
      end function c_sph_monitor_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      integer(c_int) function c_sph_monitor_num_vspec_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_sph_monitor_num_vspec_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_monitor_num_vspec_ctl = f_ctl%num_vspec_ctl
      end function c_sph_monitor_num_vspec_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_monitor_vspec_ctl(idx_in, c_ctl)       &
     &          bind(C, NAME = 'c_sph_monitor_vspec_ctl')
      integer(c_int), value, intent(in) :: idx_in
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_monitor_vspec_ctl = C_loc(f_ctl%v_pwr(idx_in+1))
      end function c_sph_monitor_vspec_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_monitor_lp_ctl(c_ctl)                  &
     &          bind(C, NAME = 'c_sph_monitor_lp_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_monitor_lp_ctl = C_loc(f_ctl%lp_ctl)
      end function c_sph_monitor_lp_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_monitor_g_pwr(c_ctl)                   &
     &          bind(C, NAME = 'c_sph_monitor_g_pwr')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_monitor_g_pwr = C_loc(f_ctl%g_pwr)
      end function c_sph_monitor_g_pwr
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_monitor_pspec_ctl(c_ctl)               &
     &          bind(C, NAME = 'c_sph_monitor_pspec_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_monitor_pspec_ctl = C_loc(f_ctl%pspec_ctl)
      end function c_sph_monitor_pspec_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_monitor_circ_ctls(c_ctl)               &
     &          bind(C, NAME = 'c_sph_monitor_circ_ctls')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_monitor_circ_ctls = C_loc(f_ctl%circ_ctls)
      end function c_sph_monitor_circ_ctls
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_monitor_dbench_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_sph_monitor_dbench_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_monitor_dbench_ctl = C_loc(f_ctl%dbench_ctl)
      end function c_sph_monitor_dbench_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_monitor_fdip_ctl(c_ctl)                &
     &          bind(C, NAME = 'c_sph_monitor_fdip_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_monitor_fdip_ctl = C_loc(f_ctl%fdip_ctl)
      end function c_sph_monitor_fdip_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_monitor_vol_ave_prefix(c_ctl)          &
     &          bind(C, NAME = 'c_sph_monitor_vol_ave_prefix')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_monitor_vol_ave_prefix = C_loc(f_ctl%volume_average_prefix)
      end function c_sph_monitor_vol_ave_prefix
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_monitor_vol_pspec_prefix(c_ctl)        &
     &          bind(C, NAME = 'c_sph_monitor_vol_pspec_prefix')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_monitor_vol_pspec_prefix                                    &
     &                     = C_loc(f_ctl%volume_pwr_spectr_prefix)
      end function c_sph_monitor_vol_pspec_prefix
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_mntr_v_pwr_spectr_fmt(c_ctl)           &
     &          bind(C, NAME = 'c_sph_mntr_v_pwr_spectr_fmt')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_mntr_v_pwr_spectr_fmt                                       &
     &           = C_loc(f_ctl%volume_pwr_spectr_format)
      end function c_sph_mntr_v_pwr_spectr_fmt
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_mntr_degree_v_spectra_ctl(c_ctl)       &
     &          bind(C, NAME = 'c_sph_mntr_degree_v_spectra_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_mntr_degree_v_spectra_ctl                                   &
     &          = C_loc(f_ctl%degree_v_spectra_switch)
      end function c_sph_mntr_degree_v_spectra_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_mntr_order_v_spectra_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_sph_mntr_order_v_spectra_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_mntr_order_v_spectra_ctl                                    &
     &          = C_loc(f_ctl%order_v_spectra_switch)
      end function c_sph_mntr_order_v_spectra_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_mntr_diff_v_lm_spectr_ctl(c_ctl)       &
     &          bind(C, NAME = 'c_sph_mntr_diff_v_lm_spectr_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_mntr_diff_v_lm_spectr_ctl                                   &
     &          = C_loc(f_ctl%diff_v_lm_spectra_switch)
      end function c_sph_mntr_diff_v_lm_spectr_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_mntr_axis_v_power_switch(c_ctl)        &
     &          bind(C, NAME = 'c_sph_mntr_axis_v_power_switch')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_mntr_axis_v_power_switch = C_loc(f_ctl%axis_v_power_switch)
      end function c_sph_mntr_axis_v_power_switch
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_mntr_h_Nusselt_file_pfx(c_ctl)         &
     &          bind(C, NAME = 'c_sph_mntr_h_Nusselt_file_pfx')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_mntr_h_Nusselt_file_pfx                                     &
     &             = C_loc(f_ctl%heat_Nusselt_file_prefix)
      end function c_sph_mntr_h_Nusselt_file_pfx
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_mntr_c_Nusselt_file_pfx(c_ctl)         &
     &          bind(C, NAME = 'c_sph_mntr_c_Nusselt_file_pfx')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_mntr_c_Nusselt_file_pfx                                     &
     &             = C_loc(f_ctl%comp_Nusselt_file_prefix)
      end function c_sph_mntr_c_Nusselt_file_pfx
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_mntr_h_Nusselt_file_fmt(c_ctl)         &
     &          bind(C, NAME = 'c_sph_mntr_h_Nusselt_file_fmt')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_mntr_h_Nusselt_file_fmt                                     &
     &             = C_loc(f_ctl%heat_Nusselt_file_format)
      end function c_sph_mntr_h_Nusselt_file_fmt
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_mntr_c_Nusselt_file_fmt(c_ctl)         &
     &          bind(C, NAME = 'c_sph_mntr_c_Nusselt_file_fmt')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_mntr_c_Nusselt_file_fmt                                     &
     &             = C_loc(f_ctl%comp_Nusselt_file_format)
      end function c_sph_mntr_c_Nusselt_file_fmt
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_mntr_lscale_file_pfix_ctl(c_ctl)       &
     &          bind(C, NAME = 'c_sph_mntr_lscale_file_pfix_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_mntr_lscale_file_pfix_ctl                                   &
     &             = C_loc(f_ctl%typ_scale_file_prefix_ctl)
      end function c_sph_mntr_lscale_file_pfix_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_mntr_lscale_file_fmt_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_sph_mntr_lscale_file_fmt_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_monitor_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_mntr_lscale_file_fmt_ctl                                    &
     &             = C_loc(f_ctl%typ_scale_file_format_ctl)
      end function c_sph_mntr_lscale_file_fmt_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_sph_monitor_control

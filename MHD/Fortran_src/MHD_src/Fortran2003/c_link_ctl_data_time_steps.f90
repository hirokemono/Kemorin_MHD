!>@file   c_link_ctl_data_time_steps.f90
!!@brief  module c_link_ctl_data_time_steps
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for time_data_control structure
!!@verbatim
!!      type(c_ptr) function c_MHD_restart_ctl_block_name(c_ctl)        &
!!     &          bind(C, NAME = 'c_MHD_restart_ctl_block_name')
!!      type(c_ptr) function c_MHD_restart_ctl_iflag(c_ctl)             &
!!     &          bind(C, NAME = 'c_MHD_restart_ctl_iflag')
!!
!!      type(c_ptr) function c_MHD_restart_flag_ctl(c_ctl)              &
!!     &          bind(C, NAME = 'c_MHD_restart_flag_ctl')
!!
!!
!!      type(c_ptr) function c_time_steps_ctl_block_name(c_ctl)         &
!!     &          bind(C, NAME = 'c_time_steps_ctl_block_name')
!!      type(c_ptr) function c_time_steps_ctl_iflag(c_ctl)              &
!!     &          bind(C, NAME = 'c_time_steps_ctl_iflag')
!!
!!      type(c_ptr) function c_time_steps_i_step_init_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_time_steps_i_step_init_ctl')
!!      type(c_ptr) function c_time_steps_i_step_number(c_ctl)          &
!!     &          bind(C, NAME = 'c_time_steps_i_step_number')
!!      type(c_ptr) function c_time_steps_elapsed_time_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_time_steps_elapsed_time_ctl')
!!      type(c_ptr) function c_time_steps_i_step_check_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_time_steps_i_step_check_ctl')
!!      type(c_ptr) function c_time_steps_i_step_rst_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_time_steps_i_step_rst_ctl')
!!
!!      type(c_ptr) function c_time_steps_i_step_pvr_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_time_steps_i_step_pvr_ctl')
!!      type(c_ptr) function c_time_steps_i_step_psf_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_time_steps_i_step_psf_ctl')
!!      type(c_ptr) function c_time_steps_i_step_map_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_time_steps_i_step_map_ctl')
!!      type(c_ptr) function c_time_steps_i_step_iso_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_time_steps_i_step_iso_ctl')
!!      type(c_ptr) function c_time_steps_i_step_lic_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_time_steps_i_step_lic_ctl')
!!      type(c_ptr) function c_time_steps_i_step_tracer_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_time_steps_i_step_tracer_ctl')
!!      type(c_ptr) function c_time_steps_i_step_fline_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_time_steps_i_step_fline_ctl')
!!      type(c_ptr) function c_time_steps_i_step_ucd_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_time_steps_i_step_ucd_ctl')
!!
!!      type(c_ptr) function c_time_steps_i_step_monitor(c_ctl)         &
!!     &          bind(C, NAME = 'c_time_steps_i_step_monitor')
!!      type(c_ptr) function c_time_steps_i_step_sgs_coef(c_ctl)        &
!!     &          bind(C, NAME = 'c_time_steps_i_step_sgs_coef')
!!      type(c_ptr) function c_time_steps_i_step_boundary(c_ctl)        &
!!     &          bind(C, NAME = 'c_time_steps_i_step_boundary')
!!
!!      type(c_ptr) function c_time_steps_dt_ctl(c_ctl)                 &
!!     &          bind(C, NAME = 'c_time_steps_dt_ctl')
!!      type(c_ptr) function c_time_steps_time_init_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_time_steps_time_init_ctl')
!!
!!      type(c_ptr) function c_time_steps_i_diff_steps_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_time_steps_i_diff_steps_ctl')
!!      type(c_ptr) function c_time_steps_flexible_step(c_ctl)          &
!!     &          bind(C, NAME = 'c_time_steps_flexible_step')
!!
!!      type(c_ptr) function c_time_steps_ratio_to_cfl(c_ctl)           &
!!     &          bind(C, NAME = 'c_time_steps_ratio_to_cfl')
!!      type(c_ptr) function c_time_steps_start_rst_step(c_ctl)         &
!!     &          bind(C, NAME = 'c_time_steps_start_rst_step')
!!      type(c_ptr) function c_time_steps_end_rst_step(c_ctl)           &
!!     &          bind(C, NAME = 'c_time_steps_end_rst_step')
!!
!!      type(c_ptr) function c_time_steps_min_delta_t_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_time_steps_min_delta_t_ctl')
!!      type(c_ptr) function c_time_steps_max_delta_t_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_time_steps_max_delta_t_ctl')
!!      type(c_ptr) function c_time_steps_max_eps_shrink(c_ctl)         &
!!     &          bind(C, NAME = 'c_time_steps_max_eps_shrink')
!!      type(c_ptr) function c_time_steps_min_eps_expand(c_ctl)         &
!!     &          bind(C, NAME = 'c_time_steps_min_eps_expand')
!!
!!      type(c_ptr) function c_time_steps_delta_t_check(c_ctl)          &
!!     &          bind(C, NAME = 'c_time_steps_delta_t_check')
!!      type(c_ptr) function c_time_steps_delta_t_rst_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_time_steps_delta_t_rst_ctl')
!!
!!      type(c_ptr) function c_time_steps_delta_t_psf_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_time_steps_delta_t_psf_ctl')
!!      type(c_ptr) function c_time_steps_delta_t_iso_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_time_steps_delta_t_iso_ctl')
!!      type(c_ptr) function c_time_steps_delta_t_map_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_time_steps_delta_t_map_ctl')
!!      type(c_ptr) function c_time_steps_delta_t_pvr_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_time_steps_delta_t_pvr_ctl')
!!      type(c_ptr) function c_time_steps_delta_t_fline(c_ctl)          &
!!     &          bind(C, NAME = 'c_time_steps_delta_t_fline')
!!      type(c_ptr) function c_time_steps_delta_t_lic_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_time_steps_delta_t_lic_ctl')
!!      type(c_ptr) function c_time_steps_delta_t_tracer_ctl(c_ctl)     &
!!     &          bind(C, NAME = 'c_time_steps_delta_t_tracer_ctl')
!!
!!      type(c_ptr) function c_time_steps_delta_t_field(c_ctl)          &
!!     &          bind(C, NAME = 'c_time_steps_delta_t_field')
!!      type(c_ptr) function c_time_steps_delta_t_monitor(c_ctl)        &
!!     &          bind(C, NAME = 'c_time_steps_delta_t_monitor')
!!      type(c_ptr) function c_time_steps_dt_sgs_coef_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_time_steps_dt_sgs_coef_ctl')
!!      type(c_ptr) function c_time_steps_dt_boundary_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_time_steps_dt_boundary_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_ctl_data_time_steps
!
      use iso_c_binding
      use t_ctl_data_4_time_steps
      use t_ctl_data_mhd_restart
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_restart_ctl_block_name(c_ctl)          &
     &          bind(C, NAME = 'c_MHD_restart_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_restart_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_restart_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_restart_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_restart_ctl_iflag(c_ctl)               &
     &          bind(C, NAME = 'c_MHD_restart_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_restart_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_restart_ctl_iflag = C_loc(f_ctl%i_restart_file)
      end function c_MHD_restart_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_restart_flag_ctl(c_ctl)                &
     &          bind(C, NAME = 'c_MHD_restart_flag_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_restart_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_restart_flag_ctl= C_loc(f_ctl%restart_flag_ctl)
      end function c_MHD_restart_flag_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_ctl_block_name(c_ctl)           &
     &          bind(C, NAME = 'c_time_steps_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_time_steps_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_ctl_iflag(c_ctl)                &
     &          bind(C, NAME = 'c_time_steps_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_ctl_iflag = C_loc(f_ctl%i_tstep)
      end function c_time_steps_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_i_step_init_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_time_steps_i_step_init_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_i_step_init_ctl= C_loc(f_ctl%i_step_init_ctl)
      end function c_time_steps_i_step_init_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_i_step_number(c_ctl)            &
     &          bind(C, NAME = 'c_time_steps_i_step_number')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_i_step_number= C_loc(f_ctl%i_step_number_ctl)
      end function c_time_steps_i_step_number
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_elapsed_time_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_time_steps_elapsed_time_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_elapsed_time_ctl= C_loc(f_ctl%elapsed_time_ctl)
      end function c_time_steps_elapsed_time_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_i_step_check_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_time_steps_i_step_check_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_i_step_check_ctl= C_loc(f_ctl%i_step_check_ctl)
      end function c_time_steps_i_step_check_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_i_step_rst_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_time_steps_i_step_rst_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_i_step_rst_ctl= C_loc(f_ctl%i_step_rst_ctl)
      end function c_time_steps_i_step_rst_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_i_step_pvr_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_time_steps_i_step_pvr_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_i_step_pvr_ctl= C_loc(f_ctl%i_step_pvr_ctl)
      end function c_time_steps_i_step_pvr_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_i_step_psf_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_time_steps_i_step_psf_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_i_step_psf_ctl= C_loc(f_ctl%i_step_psf_ctl)
      end function c_time_steps_i_step_psf_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_i_step_map_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_time_steps_i_step_map_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_i_step_map_ctl= C_loc(f_ctl%i_step_map_ctl)
      end function c_time_steps_i_step_map_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_i_step_iso_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_time_steps_i_step_iso_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_i_step_iso_ctl= C_loc(f_ctl%i_step_iso_ctl)
      end function c_time_steps_i_step_iso_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_i_step_lic_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_time_steps_i_step_lic_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_i_step_lic_ctl= C_loc(f_ctl%i_step_lic_ctl)
      end function c_time_steps_i_step_lic_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_i_step_tracer_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_time_steps_i_step_tracer_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_i_step_tracer_ctl                                    &
     &      = C_loc(f_ctl%i_step_tracer_output_ctl)
      end function c_time_steps_i_step_tracer_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_i_step_fline_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_time_steps_i_step_fline_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_i_step_fline_ctl= C_loc(f_ctl%i_step_fline_ctl)
      end function c_time_steps_i_step_fline_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_i_step_ucd_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_time_steps_i_step_ucd_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_i_step_ucd_ctl= C_loc(f_ctl%i_step_ucd_ctl)
      end function c_time_steps_i_step_ucd_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_i_step_monitor(c_ctl)           &
     &          bind(C, NAME = 'c_time_steps_i_step_monitor')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_i_step_monitor= C_loc(f_ctl%i_step_monitor_ctl)
      end function c_time_steps_i_step_monitor
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_i_step_sgs_coef(c_ctl)          &
     &          bind(C, NAME = 'c_time_steps_i_step_sgs_coef')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_i_step_sgs_coef= C_loc(f_ctl%i_step_sgs_coefs_ctl)
      end function c_time_steps_i_step_sgs_coef
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_i_step_boundary(c_ctl)          &
     &          bind(C, NAME = 'c_time_steps_i_step_boundary')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_i_step_boundary= C_loc(f_ctl%i_step_boundary_ctl)
      end function c_time_steps_i_step_boundary
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_dt_ctl(c_ctl)                   &
     &          bind(C, NAME = 'c_time_steps_dt_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_dt_ctl= C_loc(f_ctl%dt_ctl)
      end function c_time_steps_dt_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_time_init_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_time_steps_time_init_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_time_init_ctl= C_loc(f_ctl%time_init_ctl)
      end function c_time_steps_time_init_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_i_diff_steps_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_time_steps_i_diff_steps_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_i_diff_steps_ctl= C_loc(f_ctl%i_diff_steps_ctl)
      end function c_time_steps_i_diff_steps_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_flexible_step(c_ctl)            &
     &          bind(C, NAME = 'c_time_steps_flexible_step')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_flexible_step= C_loc(f_ctl%flexible_step_ctl)
      end function c_time_steps_flexible_step
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_ratio_to_cfl(c_ctl)             &
     &          bind(C, NAME = 'c_time_steps_ratio_to_cfl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_ratio_to_cfl= C_loc(f_ctl%ratio_to_cfl_ctl)
      end function c_time_steps_ratio_to_cfl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_start_rst_step(c_ctl)           &
     &          bind(C, NAME = 'c_time_steps_start_rst_step')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_start_rst_step= C_loc(f_ctl%start_rst_step_ctl)
      end function c_time_steps_start_rst_step
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_end_rst_step(c_ctl)             &
     &          bind(C, NAME = 'c_time_steps_end_rst_step')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_end_rst_step= C_loc(f_ctl%end_rst_step_ctl)
      end function c_time_steps_end_rst_step
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_min_delta_t_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_time_steps_min_delta_t_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_min_delta_t_ctl= C_loc(f_ctl%min_delta_t_ctl)
      end function c_time_steps_min_delta_t_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_max_delta_t_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_time_steps_max_delta_t_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_max_delta_t_ctl= C_loc(f_ctl%max_delta_t_ctl)
      end function c_time_steps_max_delta_t_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_max_eps_shrink(c_ctl)           &
     &          bind(C, NAME = 'c_time_steps_max_eps_shrink')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_max_eps_shrink= C_loc(f_ctl%max_eps_to_shrink_ctl)
      end function c_time_steps_max_eps_shrink
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_min_eps_expand(c_ctl)           &
     &          bind(C, NAME = 'c_time_steps_min_eps_expand')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_min_eps_expand= C_loc(f_ctl%min_eps_to_expand_ctl)
      end function c_time_steps_min_eps_expand
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_delta_t_check(c_ctl)            &
     &          bind(C, NAME = 'c_time_steps_delta_t_check')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_delta_t_check= C_loc(f_ctl%delta_t_check_ctl)
      end function c_time_steps_delta_t_check
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_delta_t_rst_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_time_steps_delta_t_rst_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_delta_t_rst_ctl= C_loc(f_ctl%delta_t_rst_ctl)
      end function c_time_steps_delta_t_rst_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_delta_t_psf_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_time_steps_delta_t_psf_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_delta_t_psf_ctl= C_loc(f_ctl%delta_t_psf_ctl)
      end function c_time_steps_delta_t_psf_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_delta_t_iso_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_time_steps_delta_t_iso_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_delta_t_iso_ctl= C_loc(f_ctl%delta_t_iso_ctl)
      end function c_time_steps_delta_t_iso_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_delta_t_map_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_time_steps_delta_t_map_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_delta_t_map_ctl= C_loc(f_ctl%delta_t_map_ctl)
      end function c_time_steps_delta_t_map_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_delta_t_pvr_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_time_steps_delta_t_pvr_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_delta_t_pvr_ctl= C_loc(f_ctl%delta_t_pvr_ctl)
      end function c_time_steps_delta_t_pvr_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_delta_t_fline(c_ctl)            &
     &          bind(C, NAME = 'c_time_steps_delta_t_fline')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_delta_t_fline= C_loc(f_ctl%delta_t_fline_ctl)
      end function c_time_steps_delta_t_fline
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_delta_t_lic_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_time_steps_delta_t_lic_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_delta_t_lic_ctl= C_loc(f_ctl%delta_t_lic_ctl)
      end function c_time_steps_delta_t_lic_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_delta_t_tracer_ctl(c_ctl)       &
     &          bind(C, NAME = 'c_time_steps_delta_t_tracer_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_delta_t_tracer_ctl                                   &
     &       = C_loc(f_ctl%delta_t_tracer_output_ctl)
      end function c_time_steps_delta_t_tracer_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_delta_t_field(c_ctl)            &
     &          bind(C, NAME = 'c_time_steps_delta_t_field')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_delta_t_field= C_loc(f_ctl%delta_t_field_ctl)
      end function c_time_steps_delta_t_field
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_delta_t_monitor(c_ctl)          &
     &          bind(C, NAME = 'c_time_steps_delta_t_monitor')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_delta_t_monitor= C_loc(f_ctl%delta_t_monitor_ctl)
      end function c_time_steps_delta_t_monitor
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_dt_sgs_coef_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_time_steps_dt_sgs_coef_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_dt_sgs_coef_ctl = C_loc(f_ctl%delta_t_sgs_coefs_ctl)
      end function c_time_steps_dt_sgs_coef_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_time_steps_dt_boundary_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_time_steps_dt_boundary_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(time_data_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_time_steps_dt_boundary_ctl= C_loc(f_ctl%delta_t_boundary_ctl)
      end function c_time_steps_dt_boundary_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_ctl_data_time_steps

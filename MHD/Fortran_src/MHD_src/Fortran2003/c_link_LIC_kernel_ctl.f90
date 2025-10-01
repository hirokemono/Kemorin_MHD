!>@file   c_link_LIC_kernel_ctl.f90
!!@brief  module c_link_LIC_kernel_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for forces control structure
!!@verbatim
!!      type(c_ptr) function c_LIC_kernel_ctl_block_name(c_ctl)         &
!!     &          bind(C, NAME = 'c_LIC_kernel_ctl_block_name')
!!      type(c_ptr) function c_LIC_kernel_ctl_iflag(c_ctl)              &
!!     &          bind(C, NAME = 'c_LIC_kernel_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_LIC_kernel_type_ctl(c_ctl)               &
!!     &          bind(C, NAME = 'c_LIC_kernel_type_ctl')
!!      type(c_ptr) function c_LIC_kernel_resolution_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_LIC_kernel_resolution_ctl')
!!      type(c_ptr) function c_LIC_kernel_peak_ctl(c_ctl)               &
!!     &          bind(C, NAME = 'c_LIC_kernel_peak_ctl')
!!      type(c_ptr) function c_LIC_kernel_kernel_sigma_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_LIC_kernel_kernel_sigma_ctl')
!!      type(c_ptr) function c_LIC_kernel_trace_len_mod_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_LIC_kernel_trace_len_mod_ctl')
!!      type(c_ptr) function c_LIC_kernel_half_length_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_LIC_kernel_half_length_ctl')
!!      type(c_ptr) function c_LIC_kernel_max_trace_cnt_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_LIC_kernel_max_trace_cnt_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!!!
!!      type(c_ptr) function c_LIC_noise_ctl_block_name(c_ctl)          &
!!     &          bind(C, NAME = 'c_LIC_noise_ctl_block_name')
!!      type(c_ptr) function c_LIC_noise_ctl_iflag(c_ctl)               &
!!     &          bind(C, NAME = 'c_LIC_noise_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_LIC_noise_type_ctl(c_ctl)                &
!!     &          bind(C, NAME = 'c_LIC_noise_type_ctl')
!!      type(c_ptr) function c_LIC_noise_file_name_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_LIC_noise_file_name_ctl')
!!      type(c_ptr) function c_LIC_noise_file_format_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_LIC_noise_file_format_ctl')
!!      type(c_ptr) function c_LIC_noise_resolution_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_LIC_noise_resolution_ctl')
!!      type(c_ptr) function c_LIC_noise_stepping_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_LIC_noise_stepping_ctl')
!!      type(c_ptr) function c_LIC_noise_cube_size_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_LIC_noise_cube_size_ctl')
!!      type(c_ptr) function c_LIC_noise_deltax_ctl(c_ctl)              &
!!     &          bind(C, NAME = 'c_LIC_noise_deltax_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_LIC_kernel_ctl
!
      use iso_c_binding
      use t_control_data_LIC_kernel
      use t_control_data_LIC_noise
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_LIC_kernel_ctl_block_name(c_ctl)           &
     &          bind(C, NAME = 'c_LIC_kernel_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(lic_kernel_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_LIC_kernel_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_LIC_kernel_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_LIC_kernel_ctl_iflag(c_ctl)                &
     &          bind(C, NAME = 'c_LIC_kernel_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(lic_kernel_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_LIC_kernel_ctl_iflag = C_loc(f_ctl%i_kernel_control)
      end function c_LIC_kernel_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_LIC_kernel_type_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_LIC_kernel_type_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(lic_kernel_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_LIC_kernel_type_ctl = C_loc(f_ctl%kernel_type_ctl)
      end function c_LIC_kernel_type_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_LIC_kernel_resolution_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_LIC_kernel_resolution_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(lic_kernel_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_LIC_kernel_resolution_ctl = C_loc(f_ctl%kernel_resolution_ctl)
      end function c_LIC_kernel_resolution_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_LIC_kernel_peak_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_LIC_kernel_peak_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(lic_kernel_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_LIC_kernel_peak_ctl = C_loc(f_ctl%kernel_peak_ctl)
      end function c_LIC_kernel_peak_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_LIC_kernel_kernel_sigma_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_LIC_kernel_kernel_sigma_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(lic_kernel_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_LIC_kernel_kernel_sigma_ctl = C_loc(f_ctl%kernel_sigma_ctl)
      end function c_LIC_kernel_kernel_sigma_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_LIC_kernel_trace_len_mod_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_LIC_kernel_trace_len_mod_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(lic_kernel_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_LIC_kernel_trace_len_mod_ctl                                    &
     &            = C_loc(f_ctl%trace_length_mode_ctl)
      end function c_LIC_kernel_trace_len_mod_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_LIC_kernel_half_length_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_LIC_kernel_half_length_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(lic_kernel_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_LIC_kernel_half_length_ctl = C_loc(f_ctl%half_length_ctl)
      end function c_LIC_kernel_half_length_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_LIC_kernel_max_trace_cnt_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_LIC_kernel_max_trace_cnt_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(lic_kernel_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_LIC_kernel_max_trace_cnt_ctl = C_loc(f_ctl%max_trace_count_ctl)
      end function c_LIC_kernel_max_trace_cnt_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_LIC_noise_ctl_block_name(c_ctl)            &
     &          bind(C, NAME = 'c_LIC_noise_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(cube_noise_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_LIC_noise_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_LIC_noise_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_LIC_noise_ctl_iflag(c_ctl)                 &
     &          bind(C, NAME = 'c_LIC_noise_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(cube_noise_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_LIC_noise_ctl_iflag = C_loc(f_ctl%i_cube_noise_control)
      end function c_LIC_noise_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_LIC_noise_type_ctl(c_ctl)                  &
     &          bind(C, NAME = 'c_LIC_noise_type_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(cube_noise_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_LIC_noise_type_ctl = C_loc(f_ctl%noise_type_ctl)
      end function c_LIC_noise_type_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_LIC_noise_file_name_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_LIC_noise_file_name_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(cube_noise_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_LIC_noise_file_name_ctl = C_loc(f_ctl%noise_file_name_ctl)
      end function c_LIC_noise_file_name_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_LIC_noise_file_format_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_LIC_noise_file_format_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(cube_noise_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_LIC_noise_file_format_ctl = C_loc(f_ctl%noise_file_format_ctl)
      end function c_LIC_noise_file_format_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_LIC_noise_resolution_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_LIC_noise_resolution_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(cube_noise_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_LIC_noise_resolution_ctl = C_loc(f_ctl%noise_resolution_ctl)
      end function c_LIC_noise_resolution_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_LIC_noise_stepping_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_LIC_noise_stepping_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(cube_noise_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_LIC_noise_stepping_ctl = C_loc(f_ctl%noise_stepping_ctl)
      end function c_LIC_noise_stepping_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_LIC_noise_cube_size_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_LIC_noise_cube_size_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(cube_noise_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_LIC_noise_cube_size_ctl = C_loc(f_ctl%noise_cube_size_ctl)
      end function c_LIC_noise_cube_size_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_LIC_noise_deltax_ctl(c_ctl)                &
     &          bind(C, NAME = 'c_LIC_noise_deltax_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(cube_noise_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_LIC_noise_deltax_ctl = C_loc(f_ctl%noise_deltax_ctl)
      end function c_LIC_noise_deltax_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_LIC_kernel_ctl

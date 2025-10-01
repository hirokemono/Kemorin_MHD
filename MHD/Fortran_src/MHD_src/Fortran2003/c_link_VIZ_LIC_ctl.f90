!>@file   c_link_VIZ_LIC_ctl.f90
!!@brief  module c_link_VIZ_LIC_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for forces control structure
!!@verbatim
!!      type(c_ptr) function c_VIZ_LIC_ctl_block_name(c_ctl)            &
!!     &          bind(C, NAME = 'c_VIZ_LIC_ctl_block_name')
!!      type(c_ptr) function c_VIZ_LIC_ctl_iflag(c_ctl)                 &
!!     &          bind(C, NAME = 'c_VIZ_LIC_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_VIZ_LIC_LIC_field_ctl(c_ctl)             &
!!     &          bind(C, NAME = 'c_VIZ_LIC_LIC_field_ctl')
!!      type(c_ptr) function c_VIZ_LIC_pe_elapsed_dump_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_VIZ_LIC_pe_elapsed_dump_ctl')
!!      type(c_ptr) function c_VIZ_LIC_color_field_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_VIZ_LIC_color_field_ctl')
!!      type(c_ptr) function c_VIZ_LIC_color_comp_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_VIZ_LIC_color_comp_ctl')
!!      type(c_ptr) function c_VIZ_LIC_opacity_field_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_VIZ_LIC_opacity_field_ctl')
!!      type(c_ptr) function c_VIZ_LIC_opacity_comp_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_VIZ_LIC_opacity_comp_ctl')
!!      type(c_ptr) function c_VIZ_LIC_mul_masking_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_VIZ_LIC_mul_masking_ctl')
!!      type(c_ptr) function c_VIZ_LIC_fname_noise_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_VIZ_LIC_fname_noise_ctl')
!!      type(c_ptr) function c_VIZ_LIC_noise_ctl(c_ctl)                 &
!!     &          bind(C, NAME = 'c_VIZ_LIC_noise_ctl')
!!      type(c_ptr) function c_VIZ_LIC_fname_kernel_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_VIZ_LIC_fname_kernel_ctl')
!!      type(c_ptr) function c_VIZ_LIC_kernel_ctl(c_ctl)                &
!!     &          bind(C, NAME = 'c_VIZ_LIC_kernel_ctl')
!!      type(c_ptr) function c_VIZ_LIC_vr_sample_mode_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_VIZ_LIC_vr_sample_mode_ctl')
!!      type(c_ptr) function c_VIZ_LIC_step_size_ctl(c_ctl)             &
!!     &          bind(C, NAME = 'c_VIZ_LIC_step_size_ctl')
!!      type(c_ptr) function c_VIZ_LIC_normalize_type_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_VIZ_LIC_normalize_type_ctl')
!!      type(c_ptr) function c_VIZ_LIC_normalize_value_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_VIZ_LIC_normalize_value_ctl')
!!      type(c_ptr) function c_VIZ_LIC_fname_vol_repart_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_VIZ_LIC_fname_vol_repart_ctl')
!!      type(c_ptr) function c_VIZ_LIC_repartition_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_VIZ_LIC_repartition_ctl')
!!        integer(c_int), value :: idx_in
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_VIZ_LIC_ctl
!
      use iso_c_binding
      use t_control_data_LIC
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_LIC_ctl_block_name(c_ctl)              &
     &          bind(C, NAME = 'c_VIZ_LIC_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(lic_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_LIC_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_VIZ_LIC_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_LIC_ctl_iflag(c_ctl)                   &
     &          bind(C, NAME = 'c_VIZ_LIC_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(lic_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_LIC_ctl_iflag = C_loc(f_ctl%i_lic_control)
      end function c_VIZ_LIC_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_LIC_LIC_field_ctl(c_ctl)               &
     &          bind(C, NAME = 'c_VIZ_LIC_LIC_field_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(lic_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_LIC_LIC_field_ctl = C_loc(f_ctl%LIC_field_ctl)
      end function c_VIZ_LIC_LIC_field_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_LIC_pe_elapsed_dump_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_VIZ_LIC_pe_elapsed_dump_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(lic_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_LIC_pe_elapsed_dump_ctl                                     &
     &            = C_loc(f_ctl%subdomain_elapsed_dump_ctl)
      end function c_VIZ_LIC_pe_elapsed_dump_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_LIC_color_field_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_VIZ_LIC_color_field_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(lic_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_LIC_color_field_ctl = C_loc(f_ctl%color_field_ctl)
      end function c_VIZ_LIC_color_field_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_LIC_color_comp_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_VIZ_LIC_color_comp_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(lic_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_LIC_color_comp_ctl = C_loc(f_ctl%color_component_ctl)
      end function c_VIZ_LIC_color_comp_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_LIC_opacity_field_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_VIZ_LIC_opacity_field_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(lic_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_LIC_opacity_field_ctl = C_loc(f_ctl%opacity_field_ctl)
      end function c_VIZ_LIC_opacity_field_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_LIC_opacity_comp_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_VIZ_LIC_opacity_comp_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(lic_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_LIC_opacity_comp_ctl = C_loc(f_ctl%opacity_component_ctl)
      end function c_VIZ_LIC_opacity_comp_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_LIC_mul_masking_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_VIZ_LIC_mul_masking_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(lic_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_LIC_mul_masking_ctl = C_loc(f_ctl%mul_mask_c)
      end function c_VIZ_LIC_mul_masking_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_LIC_fname_noise_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_VIZ_LIC_fname_noise_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(lic_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_LIC_fname_noise_ctl = C_loc(f_ctl%fname_LIC_noise_ctl)
      end function c_VIZ_LIC_fname_noise_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_LIC_noise_ctl(c_ctl)                   &
     &          bind(C, NAME = 'c_VIZ_LIC_noise_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(lic_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_LIC_noise_ctl = C_loc(f_ctl%noise_ctl)
      end function c_VIZ_LIC_noise_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_LIC_fname_kernel_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_VIZ_LIC_fname_kernel_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(lic_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_LIC_fname_kernel_ctl = C_loc(f_ctl%fname_LIC_kernel_ctl)
      end function c_VIZ_LIC_fname_kernel_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_LIC_kernel_ctl(c_ctl)                  &
     &          bind(C, NAME = 'c_VIZ_LIC_kernel_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(lic_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_LIC_kernel_ctl = C_loc(f_ctl%kernel_ctl)
      end function c_VIZ_LIC_kernel_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_LIC_vr_sample_mode_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_VIZ_LIC_vr_sample_mode_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(lic_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_LIC_vr_sample_mode_ctl = C_loc(f_ctl%vr_sample_mode_ctl)
      end function c_VIZ_LIC_vr_sample_mode_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_LIC_step_size_ctl(c_ctl)               &
     &          bind(C, NAME = 'c_VIZ_LIC_step_size_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(lic_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_LIC_step_size_ctl = C_loc(f_ctl%step_size_ctl)
      end function c_VIZ_LIC_step_size_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_LIC_normalize_type_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_VIZ_LIC_normalize_type_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(lic_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_LIC_normalize_type_ctl= C_loc(f_ctl%normalization_type_ctl)
      end function c_VIZ_LIC_normalize_type_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_LIC_normalize_value_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_VIZ_LIC_normalize_value_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(lic_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_LIC_normalize_value_ctl                                     &
     &            = C_loc(f_ctl%normalization_value_ctl)
      end function c_VIZ_LIC_normalize_value_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_LIC_fname_vol_repart_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_VIZ_LIC_fname_vol_repart_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(lic_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_LIC_fname_vol_repart_ctl= C_loc(f_ctl%fname_vol_repart_ctl)
      end function c_VIZ_LIC_fname_vol_repart_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_LIC_repartition_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_VIZ_LIC_repartition_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(lic_parameter_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_LIC_repartition_ctl = C_loc(f_ctl%repart_ctl)
      end function c_VIZ_LIC_repartition_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_VIZ_LIC_ctl

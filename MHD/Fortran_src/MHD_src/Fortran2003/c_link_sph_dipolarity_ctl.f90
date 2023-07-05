!>@file   c_link_sph_dipolarity_ctl.f90
!!@brief  module c_link_sph_dipolarity_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!!
!>@brief C binding routines for sphere_data_control structure
!!@verbatim
!!      type(c_ptr) function c_dipolarity_ctl_block_name(c_ctl)         &
!!     &          bind(C, NAME = 'c_dipolarity_ctl_block_name')
!!      type(c_ptr) function c_dipolarity_ctl_iflag(c_ctl)              &
!!     &          bind(C, NAME = 'c_dipolarity_ctl_iflag')
!!
!!      type(c_ptr) function c_dipolarity_truncation_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_dipolarity_truncation_ctl')
!!      type(c_ptr) function c_dipolarity_file_prefix_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_dipolarity_file_prefix_ctl')
!!      type(c_ptr) function c_dipolarity_file_format_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_dipolarity_file_format_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_dynamobench_ctl_block_name(c_ctl)        &
!!     &          bind(C, NAME = 'c_dynamobench_ctl_block_name')
!!      type(c_ptr) function c_dynamobench_ctl_iflag(c_ctl)             &
!!     &          bind(C, NAME = 'c_dynamobench_ctl_iflag')
!!
!!      type(c_ptr) function c_sph_dynamobench_file_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_sph_dynamobench_file_ctl')
!!      type(c_ptr) function c_sph_dynamobench_format_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_sph_dynamobench_format_ctl')
!!      type(c_ptr) function c_sph_detailed_dbench_file_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_sph_detailed_dbench_file_ctl')
!!      type(c_ptr) function c_sph_dbench_field_file_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_sph_dbench_field_file_ctl')
!!      type(c_ptr) function c_sph_dbench_spectr_file_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_sph_dbench_spectr_file_ctl')
!!      type(c_ptr) function c_sph_dbench_nphi_mid_eq_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_sph_dbench_nphi_mid_eq_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_sph_dipolarity_ctl
!
      use iso_c_binding
      use t_ctl_data_sph_dipolarity
      use t_ctl_data_dynamobench
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_dipolarity_ctl_block_name(c_ctl)           &
     &          bind(C, NAME = 'c_dipolarity_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_dipolarity_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_dipolarity_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_dipolarity_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_dipolarity_ctl_iflag(c_ctl)                &
     &          bind(C, NAME = 'c_dipolarity_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_dipolarity_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_dipolarity_ctl_iflag = C_loc(f_ctl%i_dipolarity_ctl)
      end function c_dipolarity_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_dipolarity_truncation_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_dipolarity_truncation_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_dipolarity_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_dipolarity_truncation_ctl = C_loc(f_ctl%fdip_truncation_ctl)
      end function c_dipolarity_truncation_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_dipolarity_file_prefix_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_dipolarity_file_prefix_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_dipolarity_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_dipolarity_file_prefix_ctl = C_loc(f_ctl%fdip_file_prefix_ctl)
      end function c_dipolarity_file_prefix_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_dipolarity_file_format_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_dipolarity_file_format_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(sph_dipolarity_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_dipolarity_file_format_ctl = C_loc(f_ctl%fdip_file_format_ctl)
      end function c_dipolarity_file_format_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_dynamobench_ctl_block_name(c_ctl)          &
     &          bind(C, NAME = 'c_dynamobench_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(dynamobench_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_dynamobench_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_dynamobench_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_dynamobench_ctl_iflag(c_ctl)               &
     &          bind(C, NAME = 'c_dynamobench_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(dynamobench_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_dynamobench_ctl_iflag = C_loc(f_ctl%i_dynamobench_ctl)
      end function c_dynamobench_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_dynamobench_file_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_sph_dynamobench_file_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(dynamobench_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_dynamobench_file_ctl = C_loc(f_ctl%dynamobench_file_ctl)
      end function c_sph_dynamobench_file_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_dynamobench_format_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_sph_dynamobench_format_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(dynamobench_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_dynamobench_format_ctl= C_loc(f_ctl%dynamobench_format_ctl)
      end function c_sph_dynamobench_format_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_detailed_dbench_file_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_sph_detailed_dbench_file_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(dynamobench_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_detailed_dbench_file_ctl                                    &
     &            = C_loc(f_ctl%detailed_dbench_file_ctl)
      end function c_sph_detailed_dbench_file_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_dbench_field_file_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_sph_dbench_field_file_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(dynamobench_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_dbench_field_file_ctl = C_loc(f_ctl%dbench_field_file_ctl)
      end function c_sph_dbench_field_file_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_dbench_spectr_file_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_sph_dbench_spectr_file_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(dynamobench_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_dbench_spectr_file_ctl= C_loc(f_ctl%dbench_spectr_file_ctl)
      end function c_sph_dbench_spectr_file_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_dbench_nphi_mid_eq_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_sph_dbench_nphi_mid_eq_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(dynamobench_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_dbench_nphi_mid_eq_ctl = C_loc(f_ctl%nphi_mid_eq_ctl)
      end function c_sph_dbench_nphi_mid_eq_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_sph_dipolarity_ctl

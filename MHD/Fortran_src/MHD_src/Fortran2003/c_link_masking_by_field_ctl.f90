!>@file   c_link_masking_by_field_ctl.f90
!!@brief  module c_link_masking_by_field_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for forces control structure
!!@verbatim
!!      type(c_ptr) function c_masking_fld_ctl_block_name(c_ctl)        &
!!     &          bind(C, NAME = 'c_masking_fld_ctl_block_name')
!!      type(c_ptr) function c_masking_fld_ctl_iflag(c_ctl)             &
!!     &          bind(C, NAME = 'c_masking_fld_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_masking_fld_mask_type_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_masking_fld_mask_type_ctl')
!!      type(c_ptr) function c_masking_fld_field_name_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_masking_fld_field_name_ctl')
!!      type(c_ptr) function c_masking_fld_component_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_masking_fld_component_ctl')
!!      type(c_ptr) function c_masking_fld_mask_range_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_masking_fld_mask_range_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_masking_by_field_ctl
!
      use iso_c_binding
      use t_control_data_masking
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_masking_fld_ctl_block_name(c_ctl)          &
     &          bind(C, NAME = 'c_masking_fld_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(masking_by_field_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_masking_fld_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_masking_fld_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_masking_fld_ctl_iflag(c_ctl)               &
     &          bind(C, NAME = 'c_masking_fld_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(masking_by_field_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_masking_fld_ctl_iflag = C_loc(f_ctl%i_mask_control)
      end function c_masking_fld_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_masking_fld_mask_type_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_masking_fld_mask_type_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(masking_by_field_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_masking_fld_mask_type_ctl = C_loc(f_ctl%mask_type_ctl)
      end function c_masking_fld_mask_type_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_masking_fld_field_name_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_masking_fld_field_name_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(masking_by_field_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_masking_fld_field_name_ctl = C_loc(f_ctl%field_name_ctl)
      end function c_masking_fld_field_name_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_masking_fld_component_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_masking_fld_component_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(masking_by_field_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_masking_fld_component_ctl = C_loc(f_ctl%component_ctl)
      end function c_masking_fld_component_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_masking_fld_mask_range_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_masking_fld_mask_range_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(masking_by_field_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_masking_fld_mask_range_ctl = C_loc(f_ctl%mask_range_ctl)
      end function c_masking_fld_mask_range_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_masking_by_field_ctl

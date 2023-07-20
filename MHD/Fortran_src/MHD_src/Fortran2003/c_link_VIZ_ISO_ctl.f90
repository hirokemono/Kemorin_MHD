!>@file   c_link_VIZ_ISO_ctl.f90
!!@brief  module c_link_VIZ_ISO_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for forces control structure
!!@verbatim
!!      type(c_ptr) function c_VIZ_ISO_ctl_block_name(c_ctl)            &
!!     &          bind(C, NAME = 'c_VIZ_ISO_ctl_block_name')
!!      type(c_ptr) function c_VIZ_ISO_ctl_iflag(c_ctl)                 &
!!     &          bind(C, NAME = 'c_VIZ_ISO_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_VIZ_ISO_iso_def_ctl(c_ctl)               &
!!     &          bind(C, NAME = 'c_VIZ_ISO_iso_def_ctl')
!!      type(c_ptr) function c_VIZ_ISO_fname_fld_on_iso(c_ctl)          &
!!     &          bind(C, NAME = 'c_VIZ_ISO_fname_fld_on_iso')
!!      type(c_ptr) function c_VIZ_ISO_fld_on_iso_c(c_ctl)              &
!!     &          bind(C, NAME = 'c_VIZ_ISO_fld_on_iso_c')
!!      type(c_ptr) function c_VIZ_ISO_file_head_ctl(c_ctl)             &
!!     &          bind(C, NAME = 'c_VIZ_ISO_file_head_ctl')
!!      type(c_ptr) function c_VIZ_ISO_output_type_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_VIZ_ISO_output_type_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!!!
!!      type(c_ptr) function c_VIZ_fld_on_psf_ctl_block_name(c_ctl)     &
!!     &          bind(C, NAME = 'c_VIZ_fld_on_psf_ctl_block_name')
!!      type(c_ptr) function c_VIZ_fld_on_psf_ctl_iflag(c_ctl)          &
!!     &          bind(C, NAME = 'c_VIZ_fld_on_psf_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_VIZ_fld_on_psf_field_out_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_VIZ_fld_on_psf_field_out_ctl')
!!      type(c_ptr) function c_VIZ_fld_on_psf_out_value_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_VIZ_fld_on_psf_out_value_ctl')
!!      type(c_ptr) function c_VIZ_fld_on_psf_out_type_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_VIZ_fld_on_psf_out_type_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!!!
!!      type(c_ptr) function c_VIZ_iso_define_ctl_block_name(c_ctl)     &
!!     &          bind(C, NAME = 'c_VIZ_iso_define_ctl_block_name')
!!      type(c_ptr) function c_VIZ_iso_define_ctl_iflag(c_ctl)          &
!!     &          bind(C, NAME = 'c_VIZ_iso_define_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_VIZ_isosurf_data_ctl(c_ctl)              &
!!     &          bind(C, NAME = 'c_VIZ_isosurf_data_ctl')
!!      type(c_ptr) function c_VIZ_isosurf_comp_ctl(c_ctl)              &
!!     &          bind(C, NAME = 'c_VIZ_isosurf_comp_ctl')
!!      type(c_ptr) function c_VIZ_isosurf_value_ctl(c_ctl)             &
!!     &          bind(C, NAME = 'c_VIZ_isosurf_value_ctl')
!!      type(c_ptr) function c_VIZ_isosurf_area_ctl(c_ctl)              &
!!     &          bind(C, NAME = 'c_VIZ_isosurf_area_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_VIZ_ISO_ctl
!
      use iso_c_binding
      use t_control_data_4_iso
      use t_control_data_4_fld_on_psf
      use t_control_data_4_iso_def
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_ISO_ctl_block_name(c_ctl)              &
     &          bind(C, NAME = 'c_VIZ_ISO_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(iso_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_ISO_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_VIZ_ISO_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_ISO_ctl_iflag(c_ctl)                   &
     &          bind(C, NAME = 'c_VIZ_ISO_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(iso_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_ISO_ctl_iflag = C_loc(f_ctl%i_iso_ctl)
      end function c_VIZ_ISO_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_ISO_iso_def_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_VIZ_ISO_iso_def_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(iso_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_ISO_iso_def_ctl = C_loc(f_ctl%iso_def_c)
      end function c_VIZ_ISO_iso_def_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_ISO_fname_fld_on_iso(c_ctl)            &
     &          bind(C, NAME = 'c_VIZ_ISO_fname_fld_on_iso')
      type(c_ptr), value, intent(in) :: c_ctl
      type(iso_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_ISO_fname_fld_on_iso = C_loc(f_ctl%fname_fld_on_iso)
      end function c_VIZ_ISO_fname_fld_on_iso
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_ISO_fld_on_iso_c(c_ctl)                &
     &          bind(C, NAME = 'c_VIZ_ISO_fld_on_iso_c')
      type(c_ptr), value, intent(in) :: c_ctl
      type(iso_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_ISO_fld_on_iso_c = C_loc(f_ctl%fld_on_iso_c)
      end function c_VIZ_ISO_fld_on_iso_c
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_ISO_file_head_ctl(c_ctl)               &
     &          bind(C, NAME = 'c_VIZ_ISO_file_head_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(iso_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_ISO_file_head_ctl = C_loc(f_ctl%iso_file_head_ctl)
      end function c_VIZ_ISO_file_head_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_ISO_output_type_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_VIZ_ISO_output_type_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(iso_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_ISO_output_type_ctl = C_loc(f_ctl%iso_output_type_ctl)
      end function c_VIZ_ISO_output_type_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_fld_on_psf_ctl_block_name(c_ctl)       &
     &          bind(C, NAME = 'c_VIZ_fld_on_psf_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(field_on_psf_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_fld_on_psf_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_VIZ_fld_on_psf_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_fld_on_psf_ctl_iflag(c_ctl)            &
     &          bind(C, NAME = 'c_VIZ_fld_on_psf_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(field_on_psf_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_fld_on_psf_ctl_iflag = C_loc(f_ctl%i_iso_result)
      end function c_VIZ_fld_on_psf_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_fld_on_psf_field_out_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_VIZ_fld_on_psf_field_out_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(field_on_psf_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_fld_on_psf_field_out_ctl = C_loc(f_ctl%field_output_ctl)
      end function c_VIZ_fld_on_psf_field_out_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_fld_on_psf_out_value_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_VIZ_fld_on_psf_out_value_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(field_on_psf_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_fld_on_psf_out_value_ctl = C_loc(f_ctl%output_value_ctl)
      end function c_VIZ_fld_on_psf_out_value_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_fld_on_psf_out_type_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_VIZ_fld_on_psf_out_type_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(field_on_psf_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_fld_on_psf_out_type_ctl = C_loc(f_ctl%output_type_ctl)
      end function c_VIZ_fld_on_psf_out_type_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_iso_define_ctl_block_name(c_ctl)       &
     &          bind(C, NAME = 'c_VIZ_iso_define_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(iso_define_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_iso_define_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_VIZ_iso_define_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_iso_define_ctl_iflag(c_ctl)            &
     &          bind(C, NAME = 'c_VIZ_iso_define_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(iso_define_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_iso_define_ctl_iflag = C_loc(f_ctl%i_iso_define)
      end function c_VIZ_iso_define_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_isosurf_data_ctl(c_ctl)                &
     &          bind(C, NAME = 'c_VIZ_isosurf_data_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(iso_define_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_isosurf_data_ctl = C_loc(f_ctl%isosurf_data_ctl)
      end function c_VIZ_isosurf_data_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_isosurf_comp_ctl(c_ctl)                &
     &          bind(C, NAME = 'c_VIZ_isosurf_comp_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(iso_define_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_isosurf_comp_ctl = C_loc(f_ctl%isosurf_comp_ctl)
      end function c_VIZ_isosurf_comp_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_isosurf_value_ctl(c_ctl)               &
     &          bind(C, NAME = 'c_VIZ_isosurf_value_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(iso_define_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_isosurf_value_ctl = C_loc(f_ctl%isosurf_value_ctl)
      end function c_VIZ_isosurf_value_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_isosurf_area_ctl(c_ctl)                &
     &          bind(C, NAME = 'c_VIZ_isosurf_area_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(iso_define_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_isosurf_area_ctl = C_loc(f_ctl%iso_area_ctl)
      end function c_VIZ_isosurf_area_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_VIZ_ISO_ctl

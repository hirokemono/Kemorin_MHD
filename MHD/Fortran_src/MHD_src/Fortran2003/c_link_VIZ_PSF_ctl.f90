!>@file   c_link_VIZ_PSF_ctl.f90
!!@brief  module c_link_VIZ_PSF_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for forces control structure
!!@verbatim
!!      type(c_ptr) function c_VIZ_PSF_ctl_block_name(c_ctl)            &
!!     &          bind(C, NAME = 'c_VIZ_PSF_ctl_block_name')
!!      type(c_ptr) function c_VIZ_PSF_ctl_iflag(c_ctl)                 &
!!     &          bind(C, NAME = 'c_VIZ_PSF_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_VIZ_PSF_fname_section_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_VIZ_PSF_fname_section_ctl')
!!      type(c_ptr) function c_VIZ_PSF_psf_def_c(c_ctl)                 &
!!     &          bind(C, NAME = 'c_VIZ_PSF_psf_def_c')
!!      type(c_ptr) function c_VIZ_PSF_fname_fld_on_psf(c_ctl)          &
!!     &          bind(C, NAME = 'c_VIZ_PSF_fname_fld_on_psf')
!!      type(c_ptr) function c_VIZ_PSF_fld_on_psf_c(c_ctl)              &
!!     &          bind(C, NAME = 'c_VIZ_PSF_fld_on_psf_c')
!!      type(c_ptr) function c_VIZ_PSF_file_head_ctl(c_ctl)             &
!!     &          bind(C, NAME = 'c_VIZ_PSF_file_head_ctl')
!!      type(c_ptr) function c_VIZ_PSF_output_type_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_VIZ_PSF_output_type_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!!!
!!      type(c_ptr) function c_VIZ_psf_define_ctl_block_name(c_ctl)     &
!!     &          bind(C, NAME = 'c_VIZ_psf_define_ctl_block_name')
!!      type(c_ptr) function c_VIZ_psf_define_ctl_iflag(c_ctl)          &
!!     &          bind(C, NAME = 'c_VIZ_psf_define_ctl_iflag')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_VIZ_psf_def_sect_method_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_VIZ_psf_def_sect_method_ctl')
!!      type(c_ptr) function c_VIZ_psf_define_coefs_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_VIZ_psf_define_coefs_ctl')
!!      type(c_ptr) function c_VIZ_psf_define_center_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_VIZ_psf_define_center_ctl')
!!      type(c_ptr) function c_VIZ_psf_define_normal_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_VIZ_psf_define_normal_ctl')
!!      type(c_ptr) function c_VIZ_psf_define_axis_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_VIZ_psf_define_axis_ctl')
!!      type(c_ptr) function c_VIZ_psf_define_radius_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_VIZ_psf_define_radius_ctl')
!!      type(c_ptr) function c_VIZ_psf_define_grp_name_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_VIZ_psf_define_grp_name_ctl')
!!      type(c_ptr) function c_VIZ_psf_define_area_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_VIZ_psf_define_area_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_VIZ_PSF_ctl
!
      use iso_c_binding
      use t_control_data_4_psf
      use t_control_data_4_psf_def
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PSF_ctl_block_name(c_ctl)              &
     &          bind(C, NAME = 'c_VIZ_PSF_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(psf_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PSF_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_VIZ_PSF_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PSF_ctl_iflag(c_ctl)                   &
     &          bind(C, NAME = 'c_VIZ_PSF_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(psf_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PSF_ctl_iflag = C_loc(f_ctl%i_psf_ctl)
      end function c_VIZ_PSF_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PSF_fname_section_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_VIZ_PSF_fname_section_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(psf_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PSF_fname_section_ctl = C_loc(f_ctl%fname_section_ctl)
      end function c_VIZ_PSF_fname_section_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PSF_psf_def_c(c_ctl)                   &
     &          bind(C, NAME = 'c_VIZ_PSF_psf_def_c')
      type(c_ptr), value, intent(in) :: c_ctl
      type(psf_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PSF_psf_def_c = C_loc(f_ctl%psf_def_c)
      end function c_VIZ_PSF_psf_def_c
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PSF_fname_fld_on_psf(c_ctl)            &
     &          bind(C, NAME = 'c_VIZ_PSF_fname_fld_on_psf')
      type(c_ptr), value, intent(in) :: c_ctl
      type(psf_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PSF_fname_fld_on_psf = C_loc(f_ctl%fname_fld_on_psf)
      end function c_VIZ_PSF_fname_fld_on_psf
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PSF_fld_on_psf_c(c_ctl)                &
     &          bind(C, NAME = 'c_VIZ_PSF_fld_on_psf_c')
      type(c_ptr), value, intent(in) :: c_ctl
      type(psf_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PSF_fld_on_psf_c = C_loc(f_ctl%fld_on_psf_c)
      end function c_VIZ_PSF_fld_on_psf_c
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PSF_file_head_ctl(c_ctl)               &
     &          bind(C, NAME = 'c_VIZ_PSF_file_head_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(psf_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PSF_file_head_ctl = C_loc(f_ctl%psf_file_head_ctl)
      end function c_VIZ_PSF_file_head_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_PSF_output_type_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_VIZ_PSF_output_type_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(psf_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_PSF_output_type_ctl = C_loc(f_ctl%psf_output_type_ctl)
      end function c_VIZ_PSF_output_type_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_psf_define_ctl_block_name(c_ctl)       &
     &          bind(C, NAME = 'c_VIZ_psf_define_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(psf_define_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_psf_define_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_VIZ_psf_define_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_psf_define_ctl_iflag(c_ctl)            &
     &          bind(C, NAME = 'c_VIZ_psf_define_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(psf_define_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_psf_define_ctl_iflag = C_loc(f_ctl%i_surface_define)
      end function c_VIZ_psf_define_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_psf_def_sect_method_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_VIZ_psf_def_sect_method_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(psf_define_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_psf_def_sect_method_ctl = C_loc(f_ctl%section_method_ctl)
      end function c_VIZ_psf_def_sect_method_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_psf_define_coefs_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_VIZ_psf_define_coefs_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(psf_define_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_psf_define_coefs_ctl = C_loc(f_ctl%psf_coefs_ctl)
      end function c_VIZ_psf_define_coefs_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_psf_define_center_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_VIZ_psf_define_center_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(psf_define_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_psf_define_center_ctl = C_loc(f_ctl%psf_center_ctl)
      end function c_VIZ_psf_define_center_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_psf_define_normal_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_VIZ_psf_define_normal_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(psf_define_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_psf_define_normal_ctl = C_loc(f_ctl%psf_normal_ctl)
      end function c_VIZ_psf_define_normal_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_psf_define_axis_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_VIZ_psf_define_axis_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(psf_define_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_psf_define_axis_ctl = C_loc(f_ctl%psf_axis_ctl)
      end function c_VIZ_psf_define_axis_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_psf_define_radius_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_VIZ_psf_define_radius_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(psf_define_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_psf_define_radius_ctl = C_loc(f_ctl%radius_psf_ctl)
      end function c_VIZ_psf_define_radius_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_psf_define_grp_name_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_VIZ_psf_define_grp_name_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(psf_define_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_psf_define_grp_name_ctl = C_loc(f_ctl%psf_group_name_ctl)
      end function c_VIZ_psf_define_grp_name_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_VIZ_psf_define_area_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_VIZ_psf_define_area_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(psf_define_ctl), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_VIZ_psf_define_area_ctl = C_loc(f_ctl%psf_area_ctl)
      end function c_VIZ_psf_define_area_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_VIZ_PSF_ctl

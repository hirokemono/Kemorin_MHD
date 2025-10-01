!>@file   c_link_SGS_filter_file_ctl.f90
!!@brief  module c_link_SGS_filter_file_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for filter_file_control structure
!!@verbatim
!!      type(c_ptr) function c_SGS_filter_file_block_name(c_ctl)        &
!!     &          bind(C, NAME = 'c_SGS_filter_file_block_name')
!!      type(c_ptr) function c_SGS_filter_file_iflag(c_ctl)             &
!!     &          bind(C, NAME = 'c_SGS_filter_file_iflag')
!!      type(c_ptr) function c_SGS_filter_head_ctl(c_ctl)               &
!!     &          bind(C, NAME = 'c_SGS_filter_head_ctl')
!!      type(c_ptr) function c_SGS_filter_coef_head_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_SGS_filter_coef_head_ctl')
!!      type(c_ptr) function c_SGS_filter_elen_head_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_SGS_filter_elen_head_ctl')
!!      type(c_ptr) function c_SGS_filter_moms_head_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_SGS_filter_moms_head_ctl')
!!      type(c_ptr) function c_SGS_filter_wide_head_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_SGS_filter_wide_head_ctl')
!!      type(c_ptr) function c_SGS_model_coef_ini_head(c_ctl)           &
!!     &          bind(C, NAME = 'c_SGS_model_coef_ini_head')
!!      type(c_ptr) function c_SGS_commute_coef_ini_head(c_ctl)         &
!!     &          bind(C, NAME = 'c_SGS_commute_coef_ini_head')
!!      type(c_ptr) function c_SGS_filter_elen_format(c_ctl)            &
!!     &          bind(C, NAME = 'c_SGS_filter_elen_format')
!!      type(c_ptr) function c_SGS_filter_3d_format(c_ctl)              &
!!     &          bind(C, NAME = 'c_SGS_filter_3d_format')
!!      type(c_ptr) function c_SGS_filter_wide_format(c_ctl)            &
!!     &          bind(C, NAME = 'c_SGS_filter_wide_format')
!!      type(c_ptr) function c_SGS_model_coef_rst_format(c_ctl)         &
!!     &          bind(C, NAME = 'c_SGS_model_coef_rst_format')
!!      type(c_ptr) function c_SGS_commute_coef_rst_format(c_ctl)       &
!!     &          bind(C, NAME = 'c_SGS_commute_coef_rst_format')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_SGS_filter_file_ctl
!
      use iso_c_binding
      use t_ctl_data_filter_files
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_filter_file_block_name(c_ctl)          &
     &          bind(C, NAME = 'c_SGS_filter_file_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(filter_file_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_filter_file_block_name = C_loc(f_ctl%block_name)
      end function c_SGS_filter_file_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_filter_file_iflag(c_ctl)               &
     &          bind(C, NAME = 'c_SGS_filter_file_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(filter_file_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_filter_file_iflag = C_loc(f_ctl%i_filter_fnames)
      end function c_SGS_filter_file_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_filter_head_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_SGS_filter_head_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(filter_file_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_filter_head_ctl = C_loc(f_ctl%filter_head_ctl)
      end function c_SGS_filter_head_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_filter_coef_head_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_SGS_filter_coef_head_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(filter_file_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_filter_coef_head_ctl = C_loc(f_ctl%filter_coef_head_ctl)
      end function c_SGS_filter_coef_head_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_filter_elen_head_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_SGS_filter_elen_head_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(filter_file_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_filter_elen_head_ctl = C_loc(f_ctl%filter_elen_head_ctl)
      end function c_SGS_filter_elen_head_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_filter_moms_head_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_SGS_filter_moms_head_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(filter_file_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_filter_moms_head_ctl = C_loc(f_ctl%filter_moms_head_ctl)
      end function c_SGS_filter_moms_head_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_filter_wide_head_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_SGS_filter_wide_head_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(filter_file_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_filter_wide_head_ctl = C_loc(f_ctl%filter_wide_head_ctl)
      end function c_SGS_filter_wide_head_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_coef_ini_head(c_ctl)             &
     &          bind(C, NAME = 'c_SGS_model_coef_ini_head')
      type(c_ptr), value, intent(in) :: c_ctl
      type(filter_file_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_coef_ini_head = C_loc(f_ctl%model_coef_ini_head_ctl)
      end function c_SGS_model_coef_ini_head
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_commute_coef_ini_head(c_ctl)           &
     &          bind(C, NAME = 'c_SGS_commute_coef_ini_head')
      type(c_ptr), value, intent(in) :: c_ctl
      type(filter_file_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_commute_coef_ini_head                                       &
     &            = C_loc(f_ctl%commute_coef_ini_head_ctl)
      end function c_SGS_commute_coef_ini_head
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_filter_elen_format(c_ctl)              &
     &          bind(C, NAME = 'c_SGS_filter_elen_format')
      type(c_ptr), value, intent(in) :: c_ctl
      type(filter_file_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_filter_elen_format = C_loc(f_ctl%filter_elen_format)
      end function c_SGS_filter_elen_format
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_filter_3d_format(c_ctl)                &
     &          bind(C, NAME = 'c_SGS_filter_3d_format')
      type(c_ptr), value, intent(in) :: c_ctl
      type(filter_file_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_filter_3d_format = C_loc(f_ctl%filter_3d_format)
      end function c_SGS_filter_3d_format
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_filter_wide_format(c_ctl)              &
     &          bind(C, NAME = 'c_SGS_filter_wide_format')
      type(c_ptr), value, intent(in) :: c_ctl
      type(filter_file_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_filter_wide_format = C_loc(f_ctl%filter_wide_format)
      end function c_SGS_filter_wide_format
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_model_coef_rst_format(c_ctl)           &
     &          bind(C, NAME = 'c_SGS_model_coef_rst_format')
      type(c_ptr), value, intent(in) :: c_ctl
      type(filter_file_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_model_coef_rst_format = C_loc(f_ctl%model_coef_rst_format)
      end function c_SGS_model_coef_rst_format
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_commute_coef_rst_format(c_ctl)         &
     &          bind(C, NAME = 'c_SGS_commute_coef_rst_format')
      type(c_ptr), value, intent(in) :: c_ctl
      type(filter_file_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_commute_coef_rst_format                                     &
     &              = C_loc(f_ctl%commute_coef_rst_format)
      end function c_SGS_commute_coef_rst_format
!
!  ---------------------------------------------------------------------
!
      end module c_link_SGS_filter_file_ctl

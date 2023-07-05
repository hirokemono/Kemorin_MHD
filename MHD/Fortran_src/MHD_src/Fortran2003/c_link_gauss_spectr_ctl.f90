!>@file   c_link_gauss_spectr_ctl.f90
!!@brief  module c_link_gauss_spectr_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!!
!>@brief C binding routines for sphere_data_control structure
!!@verbatim
!!      type(c_ptr) function c_sph_gauss_c_ctl_block_name(c_ctl)        &
!!     &          bind(C, NAME = 'c_sph_gauss_c_ctl_block_name')
!!      type(c_ptr) function c_sph_gauss_c_ctl_iflag(c_ctl)             &
!!     &          bind(C, NAME = 'c_sph_gauss_c_ctl_iflag')
!!
!!      type(c_ptr) function c_sph_gauss_coefs_prefix(c_ctl)            &
!!     &          bind(C, NAME = 'c_sph_gauss_coefs_prefix')
!!      type(c_ptr) function c_sph_gauss_coefs_format(c_ctl)            &
!!     &          bind(C, NAME = 'c_sph_gauss_coefs_format')
!!      type(c_ptr) function c_sph_gauss_coefs_radius_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_sph_gauss_coefs_radius_ctl')
!!      type(c_ptr) function c_sph_idx_gauss_ctl(c_ctl)                 &
!!     &          bind(C, NAME = 'c_sph_idx_gauss_ctl')
!!      type(c_ptr) function c_sph_idx_gauss_l_ctl(c_ctl)               &
!!     &          bind(C, NAME = 'c_sph_idx_gauss_l_ctl')
!!      type(c_ptr) function c_sph_idx_gauss_m_ctl(c_ctl)               &
!!     &          bind(C, NAME = 'c_sph_idx_gauss_m_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_gauss_spectr_ctl
!
      use iso_c_binding
      use t_ctl_data_gauss_coefs
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_gauss_c_ctl_block_name(c_ctl)          &
     &          bind(C, NAME = 'c_sph_gauss_c_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(gauss_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_gauss_c_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_sph_gauss_c_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_gauss_c_ctl_iflag(c_ctl)               &
     &          bind(C, NAME = 'c_sph_gauss_c_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(gauss_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_gauss_c_ctl_iflag = C_loc(f_ctl%i_gauss_coef_ctl)
      end function c_sph_gauss_c_ctl_iflag
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_gauss_coefs_prefix(c_ctl)              &
     &          bind(C, NAME = 'c_sph_gauss_coefs_prefix')
      type(c_ptr), value, intent(in) :: c_ctl
      type(gauss_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_gauss_coefs_prefix = C_loc(f_ctl%gauss_coefs_prefix)
      end function c_sph_gauss_coefs_prefix
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_gauss_coefs_format(c_ctl)              &
     &          bind(C, NAME = 'c_sph_gauss_coefs_format')
      type(c_ptr), value, intent(in) :: c_ctl
      type(gauss_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_gauss_coefs_format = C_loc(f_ctl%gauss_coefs_format)
      end function c_sph_gauss_coefs_format
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_gauss_coefs_radius_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_sph_gauss_coefs_radius_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(gauss_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_gauss_coefs_radius_ctl= C_loc(f_ctl%gauss_coefs_radius_ctl)
      end function c_sph_gauss_coefs_radius_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_idx_gauss_ctl(c_ctl)                   &
     &          bind(C, NAME = 'c_sph_idx_gauss_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(gauss_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_idx_gauss_ctl = C_loc(f_ctl%idx_gauss_ctl)
      end function c_sph_idx_gauss_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_idx_gauss_l_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_sph_idx_gauss_l_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(gauss_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_idx_gauss_l_ctl = C_loc(f_ctl%idx_gauss_l_ctl)
      end function c_sph_idx_gauss_l_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_sph_idx_gauss_m_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_sph_idx_gauss_m_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(gauss_spectr_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_sph_idx_gauss_m_ctl = C_loc(f_ctl%idx_gauss_m_ctl)
      end function c_sph_idx_gauss_m_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_gauss_spectr_ctl

!>@file   c_link_MHD_dimless_ctl.f90
!!@brief  module c_link_MHD_dimless_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for forces control structure
!!@verbatim
!!      type(c_ptr) function c_MHD_dimless_block_name(c_ctl)            &
!!     &          bind(C, NAME = 'c_MHD_dimless_block_name')
!!      type(c_ptr) function c_MHD_dimless_iflag(c_ctl)                 &
!!     &          bind(C, NAME = 'c_MHD_dimless_iflag')
!!      type(c_ptr) function c_MHD_dimless_array(c_ctl)                 &
!!     &          bind(C, NAME = 'c_MHD_dimless_array')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_MHD_eqs_block_name(c_ctl)                &
!!     &          bind(C, NAME = 'c_MHD_eqs_block_name')
!!      type(c_ptr) function c_MHD_eqs_iflag(c_ctl)                     &
!!     &          bind(C, NAME = 'c_MHD_eqs_iflag')
!!      type(c_ptr) function c_MHD_eqs_mom_ctl(c_ctl)                   &
!!     &          bind(C, NAME = 'c_MHD_eqs_mom_ctl')
!!      type(c_ptr) function c_MHD_eqs_induct_ctl(c_ctl)                &
!!     &          bind(C, NAME = 'c_MHD_eqs_induct_ctl')
!!      type(c_ptr) function c_MHD_eqs_heat_ctl(c_ctl)                  &
!!     &          bind(C, NAME = 'c_MHD_eqs_heat_ctl')
!!      type(c_ptr) function c_MHD_eqs_comp_ctl(c_ctl)                  &
!!     &          bind(C, NAME = 'c_MHD_eqs_comp_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_MHD_dimless_ctl
!
      use iso_c_binding
      use t_ctl_data_dimless_numbers
      use t_ctl_data_mhd_normalize
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_dimless_block_name(c_ctl)              &
     &          bind(C, NAME = 'c_MHD_dimless_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(dimless_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_dimless_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_dimless_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_dimless_iflag(c_ctl)                   &
     &          bind(C, NAME = 'c_MHD_dimless_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(dimless_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_dimless_iflag = C_loc(f_ctl%i_dimless_ctl)
      end function c_MHD_dimless_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_dimless_array(c_ctl)                   &
     &          bind(C, NAME = 'c_MHD_dimless_array')
      type(c_ptr), value, intent(in) :: c_ctl
      type(dimless_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_dimless_array = C_loc(f_ctl%dimless)
      end function c_MHD_dimless_array
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_eqs_block_name(c_ctl)                  &
     &          bind(C, NAME = 'c_MHD_eqs_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(equations_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_eqs_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_eqs_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_eqs_iflag(c_ctl)                       &
     &          bind(C, NAME = 'c_MHD_eqs_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(equations_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_eqs_iflag = C_loc(f_ctl%i_coef_term_ctl)
      end function c_MHD_eqs_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_eqs_mom_ctl(c_ctl)                     &
     &          bind(C, NAME = 'c_MHD_eqs_mom_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(equations_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_eqs_mom_ctl = C_loc(f_ctl%mom_ctl)
      end function c_MHD_eqs_mom_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_eqs_induct_ctl(c_ctl)                  &
     &          bind(C, NAME = 'c_MHD_eqs_induct_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(equations_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_eqs_induct_ctl = C_loc(f_ctl%induct_ctl)
      end function c_MHD_eqs_induct_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_eqs_heat_ctl(c_ctl)                    &
     &          bind(C, NAME = 'c_MHD_eqs_heat_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(equations_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_eqs_heat_ctl = C_loc(f_ctl%heat_ctl)
      end function c_MHD_eqs_heat_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_eqs_comp_ctl(c_ctl)                    &
     &          bind(C, NAME = 'c_MHD_eqs_comp_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(equations_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_eqs_comp_ctl = C_loc(f_ctl%comp_ctl)
      end function c_MHD_eqs_comp_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_MHD_dimless_ctl

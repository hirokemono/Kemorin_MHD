!>@file   c_link_MHD_induction_eq_ctl.f90
!!@brief  module c_link_MHD_induction_eq_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for forces control structure
!!@verbatim
!!      type(c_ptr) function c_MHD_induction_block_name(c_ctl)          &
!!     &          bind(C, NAME = 'c_MHD_induction_block_name')
!!      type(c_ptr) function c_MHD_induction_iflag(c_ctl)               &
!!     &          bind(C, NAME = 'c_MHD_induction_iflag')
!!      type(c_ptr) function c_MHD_induction_evo(c_ctl)                 &
!!     &          bind(C, NAME = 'c_MHD_induction_evo')
!!      type(c_ptr) function c_MHD_induction_diffuse(c_ctl)             &
!!     &          bind(C, NAME = 'c_MHD_induction_diffuse')
!!      type(c_ptr) function c_MHD_induction_potential(c_ctl)           &
!!     &          bind(C, NAME = 'c_MHD_induction_potential')
!!      type(c_ptr) function c_MHD_induction_uxb(c_ctl)                 &
!!     &          bind(C, NAME = 'c_MHD_induction_uxb')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_MHD_induction_eq_ctl
!
      use iso_c_binding
      use t_ctl_data_induct_norm
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_induction_block_name(c_ctl)            &
     &          bind(C, NAME = 'c_MHD_induction_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(induction_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_induction_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_induction_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_induction_iflag(c_ctl)                 &
     &          bind(C, NAME = 'c_MHD_induction_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(induction_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_induction_iflag = C_loc(f_ctl%i_induct_ctl)
      end function c_MHD_induction_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_induction_evo(c_ctl)                   &
     &          bind(C, NAME = 'c_MHD_induction_evo')
      type(c_ptr), value, intent(in) :: c_ctl
      type(induction_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_induction_evo = C_loc(f_ctl%coef_4_magne_evo)
      end function c_MHD_induction_evo
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_induction_diffuse(c_ctl)               &
     &          bind(C, NAME = 'c_MHD_induction_diffuse')
      type(c_ptr), value, intent(in) :: c_ctl
      type(induction_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_induction_diffuse = C_loc(f_ctl%coef_4_mag_diffuse)
      end function c_MHD_induction_diffuse
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_induction_potential(c_ctl)             &
     &          bind(C, NAME = 'c_MHD_induction_potential')
      type(c_ptr), value, intent(in) :: c_ctl
      type(induction_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_induction_potential = C_loc(f_ctl%coef_4_mag_potential)
      end function c_MHD_induction_potential
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_induction_uxb(c_ctl)                   &
     &          bind(C, NAME = 'c_MHD_induction_uxb')
      type(c_ptr), value, intent(in) :: c_ctl
      type(induction_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_induction_uxb = C_loc(f_ctl%coef_4_induction)
      end function c_MHD_induction_uxb
!
!  ---------------------------------------------------------------------
!
      end module c_link_MHD_induction_eq_ctl

!>@file   c_link_MHD_heat_eq_ctl.f90
!!@brief  module c_link_MHD_heat_eq_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for forces control structure
!!@verbatim
!!      type(c_ptr) function c_MHD_heat_block_name(c_ctl)               &
!!     &          bind(C, NAME = 'c_MHD_heat_block_name')
!!      type(c_ptr) function c_MHD_heat_iflag(c_ctl)                    &
!!     &          bind(C, NAME = 'c_MHD_heat_iflag')
!!      type(c_ptr) function c_MHD_heat_advect(c_ctl)                   &
!!     &          bind(C, NAME = 'c_MHD_heat_advect')
!!      type(c_ptr) function c_MHD_heat_diffuse(c_ctl)                  &
!!     &          bind(C, NAME = 'c_MHD_heat_diffuse')
!!      type(c_ptr) function c_MHD_heat_source(c_ctl)                   &
!!     &          bind(C, NAME = 'c_MHD_heat_source')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_MHD_heat_eq_ctl
!
      use iso_c_binding
      use t_ctl_data_termal_norm
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_heat_block_name(c_ctl)                 &
     &          bind(C, NAME = 'c_MHD_heat_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(heat_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_heat_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_heat_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_heat_iflag(c_ctl)                      &
     &          bind(C, NAME = 'c_MHD_heat_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(heat_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_heat_iflag = C_loc(f_ctl%i_diff_adv)
      end function c_MHD_heat_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_heat_advect(c_ctl)                     &
     &          bind(C, NAME = 'c_MHD_heat_advect')
      type(c_ptr), value, intent(in) :: c_ctl
      type(heat_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_heat_advect = C_loc(f_ctl%coef_4_adv_flux)
      end function c_MHD_heat_advect
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_heat_diffuse(c_ctl)                    &
     &          bind(C, NAME = 'c_MHD_heat_diffuse')
      type(c_ptr), value, intent(in) :: c_ctl
      type(heat_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_heat_diffuse = C_loc(f_ctl%coef_4_diffuse)
      end function c_MHD_heat_diffuse
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_heat_source(c_ctl)                     &
     &          bind(C, NAME = 'c_MHD_heat_source')
      type(c_ptr), value, intent(in) :: c_ctl
      type(heat_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_heat_source = C_loc(f_ctl%coef_4_source)
      end function c_MHD_heat_source
!
!  ---------------------------------------------------------------------
!
      end module c_link_MHD_heat_eq_ctl

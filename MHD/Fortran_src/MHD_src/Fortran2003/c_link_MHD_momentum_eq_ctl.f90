!>@file   c_link_MHD_momentum_eq_ctl.f90
!!@brief  module c_link_MHD_momentum_eq_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for forces control structure
!!@verbatim
!!      type(c_ptr) function c_MHD_momentum_eq_block_name(c_ctl)        &
!!     &          bind(C, NAME = 'c_MHD_momentum_eq_block_name')
!!      type(c_ptr) function c_MHD_momentum_eq_iflag(c_ctl)             &
!!     &          bind(C, NAME = 'c_MHD_momentum_eq_iflag')
!!      type(c_ptr) function c_MHD_momentum_eq_viscous(c_ctl)           &
!!     &          bind(C, NAME = 'c_MHD_momentum_eq_viscous')
!!      type(c_ptr) function c_MHD_momentum_eq_inertia(c_ctl)           &
!!     &          bind(C, NAME = 'c_MHD_momentum_eq_inertia')
!!      type(c_ptr) function c_MHD_momentum_eq_grad_p(c_ctl)            &
!!     &          bind(C, NAME = 'c_MHD_momentum_eq_grad_p')
!!      type(c_ptr) function c_MHD_momentum_eq_t_buoyancy(c_ctl)        &
!!     &          bind(C, NAME = 'c_MHD_momentum_eq_t_buoyancy')
!!      type(c_ptr) function c_MHD_momentum_eq_c_buoyancy(c_ctl)        &
!!     &          bind(C, NAME = 'c_MHD_momentum_eq_c_buoyancy')
!!      type(c_ptr) function c_MHD_momentum_eq_coriolis(c_ctl)          &
!!     &          bind(C, NAME = 'c_MHD_momentum_eq_coriolis')
!!      type(c_ptr) function c_MHD_momentum_eq_lorentz(c_ctl)           &
!!     &          bind(C, NAME = 'c_MHD_momentum_eq_lorentz')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_MHD_momentum_eq_ctl
!
      use iso_c_binding
      use t_ctl_data_momentum_norm
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_momentum_eq_block_name(c_ctl)          &
     &          bind(C, NAME = 'c_MHD_momentum_eq_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(momentum_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_momentum_eq_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_momentum_eq_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_momentum_eq_iflag(c_ctl)               &
     &          bind(C, NAME = 'c_MHD_momentum_eq_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(momentum_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_momentum_eq_iflag = C_loc(f_ctl%i_momentum)
      end function c_MHD_momentum_eq_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_momentum_eq_viscous(c_ctl)             &
     &          bind(C, NAME = 'c_MHD_momentum_eq_viscous')
      type(c_ptr), value, intent(in) :: c_ctl
      type(momentum_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_momentum_eq_viscous = C_loc(f_ctl%coef_4_viscous)
      end function c_MHD_momentum_eq_viscous
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_momentum_eq_inertia(c_ctl)             &
     &          bind(C, NAME = 'c_MHD_momentum_eq_inertia')
      type(c_ptr), value, intent(in) :: c_ctl
      type(momentum_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_momentum_eq_inertia = C_loc(f_ctl%coef_4_intertia)
      end function c_MHD_momentum_eq_inertia
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_momentum_eq_grad_p(c_ctl)              &
     &          bind(C, NAME = 'c_MHD_momentum_eq_grad_p')
      type(c_ptr), value, intent(in) :: c_ctl
      type(momentum_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_momentum_eq_grad_p = C_loc(f_ctl%coef_4_grad_p)
      end function c_MHD_momentum_eq_grad_p
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_momentum_eq_t_buoyancy(c_ctl)          &
     &          bind(C, NAME = 'c_MHD_momentum_eq_t_buoyancy')
      type(c_ptr), value, intent(in) :: c_ctl
      type(momentum_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_momentum_eq_t_buoyancy = C_loc(f_ctl%coef_4_termal_buo)
      end function c_MHD_momentum_eq_t_buoyancy
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_momentum_eq_c_buoyancy(c_ctl)          &
     &          bind(C, NAME = 'c_MHD_momentum_eq_c_buoyancy')
      type(c_ptr), value, intent(in) :: c_ctl
      type(momentum_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_momentum_eq_c_buoyancy = C_loc(f_ctl%coef_4_comp_buo)
      end function c_MHD_momentum_eq_c_buoyancy
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_momentum_eq_coriolis(c_ctl)            &
     &          bind(C, NAME = 'c_MHD_momentum_eq_coriolis')
      type(c_ptr), value, intent(in) :: c_ctl
      type(momentum_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_momentum_eq_coriolis = C_loc(f_ctl%coef_4_Coriolis)
      end function c_MHD_momentum_eq_coriolis
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_momentum_eq_lorentz(c_ctl)             &
     &          bind(C, NAME = 'c_MHD_momentum_eq_lorentz')
      type(c_ptr), value, intent(in) :: c_ctl
      type(momentum_equation_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_momentum_eq_lorentz = C_loc(f_ctl%coef_4_Lorentz)
      end function c_MHD_momentum_eq_lorentz
!
!  ---------------------------------------------------------------------
!
      end module c_link_MHD_momentum_eq_ctl

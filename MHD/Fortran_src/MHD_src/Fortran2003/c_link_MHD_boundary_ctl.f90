!>@file   c_link_MHD_boundary_ctl.f90
!!@brief  module c_link_MHD_boundary_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for sphere_data_control structure
!!@verbatim
!!      type(c_ptr) function c_MHD_node_bc_ctl_block_name(c_ctl)        &
!!     &          bind(C, NAME = 'c_MHD_node_bc_ctl_block_name')
!!      type(c_ptr) function c_MHD_node_bc_ctl_iflag(c_ctl)             &
!!     &          bind(C, NAME = 'c_MHD_node_bc_ctl_iflag')
!!      type(c_ptr) function c_MHD_node_bc_node_bc_T_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_MHD_node_bc_node_bc_T_ctl')
!!      type(c_ptr) function c_MHD_node_bc_node_bc_U_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_MHD_node_bc_node_bc_U_ctl')
!!      type(c_ptr) function c_MHD_node_bc_node_bc_P_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_MHD_node_bc_node_bc_P_ctl')
!!      type(c_ptr) function c_MHD_node_bc_node_bc_C_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_MHD_node_bc_node_bc_C_ctl')
!!      type(c_ptr) function c_MHD_node_bc_node_bc_B_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_MHD_node_bc_node_bc_B_ctl')
!!      type(c_ptr) function c_MHD_node_bc_node_bc_MP_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_MHD_node_bc_node_bc_MP_ctl')
!!      type(c_ptr) function c_MHD_node_bc_node_bc_A_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_MHD_node_bc_node_bc_A_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_MHD_surf_bc_ctl_block_name(c_ctl)        &
!!     &          bind(C, NAME = 'c_MHD_surf_bc_ctl_block_name')
!!      type(c_ptr) function c_MHD_surf_bc_ctl_iflag(c_ctl)             &
!!     &          bind(C, NAME = 'c_MHD_surf_bc_ctl_iflag')
!!      type(c_ptr) function c_MHD_surf_bc_surf_bc_HF_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_MHD_surf_bc_surf_bc_HF_ctl')
!!      type(c_ptr) function c_MHD_surf_bc_surf_bc_ST_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_MHD_surf_bc_surf_bc_ST_ctl')
!!      type(c_ptr) function c_MHD_surf_bc_surf_bc_PN_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_MHD_surf_bc_surf_bc_PN_ctl')
!!      type(c_ptr) function c_MHD_surf_bc_surf_bc_BN_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_MHD_surf_bc_surf_bc_BN_ctl')
!!      type(c_ptr) function c_MHD_surf_bc_surf_bc_JN_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_MHD_surf_bc_surf_bc_JN_ctl')
!!      type(c_ptr) function c_MHD_surf_bc_surf_bc_AN_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_MHD_surf_bc_surf_bc_AN_ctl')
!!      type(c_ptr) function c_MHD_surf_bc_surf_bc_MPN_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_MHD_surf_bc_surf_bc_MPN_ctl')
!!      type(c_ptr) function c_MHD_surf_bc_surf_bc_CF_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_MHD_surf_bc_surf_bc_CF_ctl')
!!      type(c_ptr) function c_MHD_surf_bc_surf_bc_INF_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_MHD_surf_bc_surf_bc_INF_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function set_label_thermal_bc_f(c_ctl)  bind(c)
!!      type(c_ptr) function set_label_momentum_bc_f(c_ctl)  bind(c)
!!      type(c_ptr) function set_label_induction_bc_f(c_ctl)  bind(c)
!!@endverbatim
      module c_link_MHD_boundary_ctl
!
      use iso_c_binding
      use t_control_array_character
      use t_ctl_data_node_boundary
      use t_ctl_data_surf_boundary
!
      implicit none
!
      type(ctl_array_chara), save, target, private :: momentum_bc_type
      type(ctl_array_chara), save, target, private :: induction_bc_type
      type(ctl_array_chara), save, target, private :: thermal_bc_type
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_node_bc_ctl_block_name(c_ctl)          &
     &          bind(C, NAME = 'c_MHD_node_bc_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(node_bc_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_node_bc_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_node_bc_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_node_bc_ctl_iflag(c_ctl)               &
     &          bind(C, NAME = 'c_MHD_node_bc_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(node_bc_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_node_bc_ctl_iflag = C_loc(f_ctl%i_bc_4_node)
      end function c_MHD_node_bc_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_node_bc_node_bc_T_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_MHD_node_bc_node_bc_T_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(node_bc_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_node_bc_node_bc_T_ctl = C_loc(f_ctl%node_bc_T_ctl)
      end function c_MHD_node_bc_node_bc_T_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_node_bc_node_bc_U_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_MHD_node_bc_node_bc_U_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(node_bc_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_node_bc_node_bc_U_ctl = C_loc(f_ctl%node_bc_U_ctl)
      end function c_MHD_node_bc_node_bc_U_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_node_bc_node_bc_P_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_MHD_node_bc_node_bc_P_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(node_bc_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_node_bc_node_bc_P_ctl = C_loc(f_ctl%node_bc_P_ctl)
      end function c_MHD_node_bc_node_bc_P_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_node_bc_node_bc_C_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_MHD_node_bc_node_bc_C_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(node_bc_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_node_bc_node_bc_C_ctl = C_loc(f_ctl%node_bc_C_ctl)
      end function c_MHD_node_bc_node_bc_C_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_node_bc_node_bc_B_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_MHD_node_bc_node_bc_B_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(node_bc_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_node_bc_node_bc_B_ctl = C_loc(f_ctl%node_bc_B_ctl)
      end function c_MHD_node_bc_node_bc_B_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_node_bc_node_bc_MP_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_MHD_node_bc_node_bc_MP_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(node_bc_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_node_bc_node_bc_MP_ctl = C_loc(f_ctl%node_bc_MP_ctl)
      end function c_MHD_node_bc_node_bc_MP_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_node_bc_node_bc_A_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_MHD_node_bc_node_bc_A_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(node_bc_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_node_bc_node_bc_A_ctl = C_loc(f_ctl%node_bc_A_ctl)
      end function c_MHD_node_bc_node_bc_A_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_node_bc_node_bc_J_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_MHD_node_bc_node_bc_J_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(node_bc_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_node_bc_node_bc_J_ctl = C_loc(f_ctl%node_bc_J_ctl)
      end function c_MHD_node_bc_node_bc_J_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_surf_bc_ctl_block_name(c_ctl)          &
     &          bind(C, NAME = 'c_MHD_surf_bc_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(surf_bc_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_surf_bc_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_surf_bc_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_surf_bc_ctl_iflag(c_ctl)               &
     &          bind(C, NAME = 'c_MHD_surf_bc_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(surf_bc_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_surf_bc_ctl_iflag = C_loc(f_ctl%i_bc_4_surf)
      end function c_MHD_surf_bc_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_surf_bc_surf_bc_HF_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_MHD_surf_bc_surf_bc_HF_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(surf_bc_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_surf_bc_surf_bc_HF_ctl = C_loc(f_ctl%surf_bc_HF_ctl)
      end function c_MHD_surf_bc_surf_bc_HF_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_surf_bc_surf_bc_ST_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_MHD_surf_bc_surf_bc_ST_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(surf_bc_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_surf_bc_surf_bc_ST_ctl = C_loc(f_ctl%surf_bc_ST_ctl)
      end function c_MHD_surf_bc_surf_bc_ST_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_surf_bc_surf_bc_PN_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_MHD_surf_bc_surf_bc_PN_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(surf_bc_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_surf_bc_surf_bc_PN_ctl = C_loc(f_ctl%surf_bc_PN_ctl)
      end function c_MHD_surf_bc_surf_bc_PN_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_surf_bc_surf_bc_BN_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_MHD_surf_bc_surf_bc_BN_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(surf_bc_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_surf_bc_surf_bc_BN_ctl = C_loc(f_ctl%surf_bc_BN_ctl)
      end function c_MHD_surf_bc_surf_bc_BN_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_surf_bc_surf_bc_JN_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_MHD_surf_bc_surf_bc_JN_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(surf_bc_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_surf_bc_surf_bc_JN_ctl = C_loc(f_ctl%surf_bc_JN_ctl)
      end function c_MHD_surf_bc_surf_bc_JN_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_surf_bc_surf_bc_AN_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_MHD_surf_bc_surf_bc_AN_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(surf_bc_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_surf_bc_surf_bc_AN_ctl = C_loc(f_ctl%surf_bc_AN_ctl)
      end function c_MHD_surf_bc_surf_bc_AN_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_surf_bc_surf_bc_MPN_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_MHD_surf_bc_surf_bc_MPN_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(surf_bc_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_surf_bc_surf_bc_MPN_ctl = C_loc(f_ctl%surf_bc_MPN_ctl)
      end function c_MHD_surf_bc_surf_bc_MPN_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_surf_bc_surf_bc_CF_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_MHD_surf_bc_surf_bc_CF_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(surf_bc_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_surf_bc_surf_bc_CF_ctl = C_loc(f_ctl%surf_bc_CF_ctl)
      end function c_MHD_surf_bc_surf_bc_CF_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_surf_bc_surf_bc_INF_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_MHD_surf_bc_surf_bc_INF_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(surf_bc_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_surf_bc_surf_bc_INF_ctl = C_loc(f_ctl%surf_bc_INF_ctl)
      end function c_MHD_surf_bc_surf_bc_INF_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function set_label_thermal_bc_f(c_ctl)  bind(c)
      use set_node_group_types
      type(c_ptr), value, intent(in) :: c_ctl
!
      call set_label_thermal_bc(thermal_bc_type)
      set_label_thermal_bc_f = C_loc(thermal_bc_type)
      end function set_label_thermal_bc_f
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function set_label_momentum_bc_f(c_ctl)  bind(c)
      use set_node_group_types
      type(c_ptr), value, intent(in) :: c_ctl
!
      call set_label_momentum_bc(momentum_bc_type)
      set_label_momentum_bc_f = C_loc(momentum_bc_type)
      end function set_label_momentum_bc_f
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function set_label_induction_bc_f(c_ctl)  bind(c)
      use set_node_group_types
      type(c_ptr), value, intent(in) :: c_ctl
!
      call set_label_induction_bc(induction_bc_type)
      set_label_induction_bc_f = C_loc(induction_bc_type)
      end function set_label_induction_bc_f
!
!  ---------------------------------------------------------------------
!
      end module c_link_MHD_boundary_ctl

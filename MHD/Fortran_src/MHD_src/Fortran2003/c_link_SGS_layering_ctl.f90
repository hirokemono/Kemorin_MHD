!>@file   c_link_SGS_layering_ctl.f90
!!@brief  module c_link_SGS_layering_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for layering_control structure
!!@verbatim
!!      type(c_ptr) function c_SGS_layering_ctl_block_name(c_ctl)       &
!!     &          bind(C, NAME = 'c_SGS_layering_ctl_block_name')
!!      type(c_ptr) function c_SGS_layering_ctl_iflag(c_ctl)            &
!!     &          bind(C, NAME = 'c_SGS_layering_ctl_iflag')
!!      type(c_ptr) function c_SGS_layering_grp_type_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_SGS_layering_grp_type_ctl')
!!      type(c_ptr) function c_SGS_layer_grp_name_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_SGS_layer_grp_name_ctl')
!!      type(c_ptr) function c_SGS_igrp_stack_layer_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_SGS_igrp_stack_layer_ctl')
!!      type(c_ptr) function c_SGS_num_layering_grp_ctl(c_ctl)          &
!!     &          bind(C, NAME = 'c_SGS_num_layering_grp_ctl')
!!      type(c_ptr) function c_SGS_num_fluid_layer_grp_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_SGS_num_fluid_layer_grp_ctl')
!!      type(c_ptr) function c_SGS_start_layer_grp_name_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_SGS_start_layer_grp_name_ctl')
!!      type(c_ptr) function c_SGS_start_fluid_grp_name_ctl(c_ctl)      &
!!     &          bind(C, NAME = 'c_SGS_start_fluid_grp_name_ctl')
!!      type(c_ptr) function c_SGS_ngrp_on_sphere_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_SGS_ngrp_on_sphere_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!
!!      type(c_ptr) function c_SGS_3d_filter_ctl_block_name(c_ctl)      &
!!     &          bind(C, NAME = 'c_SGS_3d_filter_ctl_block_name')
!!      type(c_ptr) function c_SGS_3d_filter_ctl_iflag(c_ctl)           &
!!     &          bind(C, NAME = 'c_SGS_3d_filter_ctl_iflag')
!!      type(c_ptr) function c_SGS_3d_whole_filter_grp_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_SGS_3d_whole_filter_grp_ctl')
!!      type(c_ptr) function c_SGS_3d_fluid_filter_grp_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_SGS_3d_fluid_filter_grp_ctl')
!!      type(c_ptr) function c_SGS_3d_momentum_filter_ctl(c_ctl)        &
!!     &          bind(C, NAME = 'c_SGS_3d_momentum_filter_ctl')
!!      type(c_ptr) function c_SGS_3d_heat_filter_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_SGS_3d_heat_filter_ctl')
!!      type(c_ptr) function c_SGS_3d_induction_filter_ctl(c_ctl)       &
!!     &          bind(C, NAME = 'c_SGS_3d_induction_filter_ctl')
!!      type(c_ptr) function c_SGS_3d_comp_filter_ctl(c_ctl)            &
!!     &          bind(C, NAME = 'c_SGS_3d_comp_filter_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_SGS_layering_ctl
!
      use iso_c_binding
      use t_ctl_data_ele_layering
      use t_ctl_SGS_3d_filter
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_layering_ctl_block_name(c_ctl)         &
     &          bind(C, NAME = 'c_SGS_layering_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layering_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_layering_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_SGS_layering_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_layering_ctl_iflag(c_ctl)              &
     &          bind(C, NAME = 'c_SGS_layering_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layering_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_layering_ctl_iflag = C_loc(f_ctl%i_dynamic_layers)
      end function c_SGS_layering_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_layering_grp_type_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_SGS_layering_grp_type_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layering_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_layering_grp_type_ctl = C_loc(f_ctl%layering_grp_type_ctl)
      end function c_SGS_layering_grp_type_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_layer_grp_name_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_SGS_layer_grp_name_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layering_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_layer_grp_name_ctl = C_loc(f_ctl%layer_grp_name_ctl)
      end function c_SGS_layer_grp_name_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_igrp_stack_layer_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_SGS_igrp_stack_layer_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layering_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_igrp_stack_layer_ctl = C_loc(f_ctl%igrp_stack_layer_ctl)
      end function c_SGS_igrp_stack_layer_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_num_layering_grp_ctl(c_ctl)            &
     &          bind(C, NAME = 'c_SGS_num_layering_grp_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layering_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_num_layering_grp_ctl = C_loc(f_ctl%num_layering_grp_ctl)
      end function c_SGS_num_layering_grp_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_num_fluid_layer_grp_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_SGS_num_fluid_layer_grp_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layering_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_num_fluid_layer_grp_ctl = C_loc(f_ctl%num_fl_layer_grp_ctl)
      end function c_SGS_num_fluid_layer_grp_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_start_layer_grp_name_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_SGS_start_layer_grp_name_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layering_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_start_layer_grp_name_ctl                                    &
     &          = C_loc(f_ctl%start_layering_grp_name_ctl)
      end function c_SGS_start_layer_grp_name_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_start_fluid_grp_name_ctl(c_ctl)        &
     &          bind(C, NAME = 'c_SGS_start_fluid_grp_name_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layering_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_start_fluid_grp_name_ctl                                    &
     &          = C_loc(f_ctl%start_fl_layer_grp_name_ctl)
      end function c_SGS_start_fluid_grp_name_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_ngrp_on_sphere_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_SGS_ngrp_on_sphere_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(layering_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_ngrp_on_sphere_ctl = C_loc(f_ctl%ngrp_SGS_on_sphere_ctl)
      end function c_SGS_ngrp_on_sphere_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_3d_filter_ctl_block_name(c_ctl)        &
     &          bind(C, NAME = 'c_SGS_3d_filter_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_3d_filter_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_3d_filter_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_SGS_3d_filter_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_3d_filter_ctl_iflag(c_ctl)             &
     &          bind(C, NAME = 'c_SGS_3d_filter_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_3d_filter_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_3d_filter_ctl_iflag = C_loc(f_ctl%i_SGS_3d_filter_ctl)
      end function c_SGS_3d_filter_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_3d_whole_filter_grp_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_SGS_3d_whole_filter_grp_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_3d_filter_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_3d_whole_filter_grp_ctl = C_loc(f_ctl%whole_filter_grp_ctl)
      end function c_SGS_3d_whole_filter_grp_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_3d_fluid_filter_grp_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_SGS_3d_fluid_filter_grp_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_3d_filter_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_3d_fluid_filter_grp_ctl = C_loc(f_ctl%fluid_filter_grp_ctl)
      end function c_SGS_3d_fluid_filter_grp_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_3d_momentum_filter_ctl(c_ctl)          &
     &          bind(C, NAME = 'c_SGS_3d_momentum_filter_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_3d_filter_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_3d_momentum_filter_ctl = C_loc(f_ctl%momentum_filter_ctl)
      end function c_SGS_3d_momentum_filter_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_3d_heat_filter_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_SGS_3d_heat_filter_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_3d_filter_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_3d_heat_filter_ctl = C_loc(f_ctl%heat_filter_ctl)
      end function c_SGS_3d_heat_filter_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_3d_induction_filter_ctl(c_ctl)         &
     &          bind(C, NAME = 'c_SGS_3d_induction_filter_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_3d_filter_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_3d_induction_filter_ctl = C_loc(f_ctl%induction_filter_ctl)
      end function c_SGS_3d_induction_filter_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_SGS_3d_comp_filter_ctl(c_ctl)              &
     &          bind(C, NAME = 'c_SGS_3d_comp_filter_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(SGS_3d_filter_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_SGS_3d_comp_filter_ctl = C_loc(f_ctl%compostion_filter_ctl)
      end function c_SGS_3d_comp_filter_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_SGS_layering_ctl

!>@file   c_link_MHD_field_evolve_ctl.f90
!!@brief  module c_link_MHD_field_evolve_ctl
!!
!!@author H. Matsui
!!@date Programmed in June., 2023
!
!>@brief C binding routines for forces control structure
!!@verbatim
!!      type(c_ptr) function c_MHD_field_ctl_block_name(c_ctl)          &
!!     &          bind(C, NAME = 'c_MHD_field_ctl_block_name')
!!      type(c_ptr) function c_MHD_field_ctl_iflag(c_ctl)               &
!!     &          bind(C, NAME = 'c_MHD_field_ctl_iflag')
!!      type(c_ptr) function c_MHD_field_ctl_field_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_MHD_field_ctl_field_ctl')
!!      type(c_ptr) function c_MHD_field_quad_phys_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_MHD_field_quad_phys_ctl')
!!      type(c_ptr) function c_MHD_scalar_phys_ctl(c_ctl)               &
!!     &          bind(C, NAME = 'c_MHD_scalar_phys_ctl')
!!      type(c_ptr) function c_MHD_vector_phys_ctl(c_ctl)               &
!!     &          bind(C, NAME = 'c_MHD_vector_phys_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!!!
!!      type(c_ptr) function c_MHD_evolution_ctl_block_name(c_ctl)      &
!!     &          bind(C, NAME = 'c_MHD_evolution_ctl_block_name')
!!      type(c_ptr) function c_MHD_evolution_ctl_iflag(c_ctl)           &
!!     &          bind(C, NAME = 'c_MHD_evolution_ctl_iflag')
!!      type(c_ptr) function c_MHD_t_evo_field_ctl(c_ctl)               &
!!     &          bind(C, NAME = 'c_MHD_t_evo_field_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!!!
!!      type(c_ptr) function c_MHD_forces_block_name(c_ctl)             &
!!     &          bind(C, NAME = 'c_MHD_forces_block_name')
!!      type(c_ptr) function c_MHD_forces_iflag(c_ctl)                  &
!!     &          bind(C, NAME = 'c_MHD_forces_iflag')
!!      type(c_ptr) function c_MHD_forces_array(c_ctl)                  &
!!     &          bind(C, NAME = 'c_MHD_forces_array')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!!!
!!      type(c_ptr) function c_MHD_evo_area_ctl_block_name(c_ctl)       &
!!     &          bind(C, NAME = 'c_MHD_evo_area_ctl_block_name')
!!      type(c_ptr) function c_MHD_evo_area_ctl_iflag(c_ctl)            &
!!     &          bind(C, NAME = 'c_MHD_evo_area_ctl_iflag')
!!      type(c_ptr) function c_MHD_evo_fluid_group_ctl(c_ctl)           &
!!     &          bind(C, NAME = 'c_MHD_evo_fluid_group_ctl')
!!      type(c_ptr) function c_MHD_evo_conduct_group_ctl(c_ctl)         &
!!     &          bind(C, NAME = 'c_MHD_evo_conduct_group_ctl')
!!        type(c_ptr), value, intent(in) :: c_ctl
!!@endverbatim
      module c_link_MHD_field_evolve_ctl
!
      use iso_c_binding
      use t_ctl_data_4_fields
      use t_ctl_data_mhd_evolution
      use t_ctl_data_mhd_forces
      use t_ctl_data_mhd_evo_area
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_field_ctl_block_name(c_ctl)            &
     &          bind(C, NAME = 'c_MHD_field_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(field_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_field_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_field_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_field_ctl_iflag(c_ctl)               &
     &          bind(C, NAME = 'c_MHD_field_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(field_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_field_ctl_iflag = C_loc(f_ctl%i_phys_values)
      end function c_MHD_field_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_field_ctl_field_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_MHD_field_ctl_field_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(field_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_field_ctl_field_ctl = C_loc(f_ctl%field_ctl)
      end function c_MHD_field_ctl_field_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_field_quad_phys_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_MHD_field_quad_phys_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(field_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_field_quad_phys_ctl = C_loc(f_ctl%quad_phys)
      end function c_MHD_field_quad_phys_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_scalar_phys_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_MHD_scalar_phys_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(field_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_scalar_phys_ctl = C_loc(f_ctl%scalar_phys)
      end function c_MHD_scalar_phys_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_vector_phys_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_MHD_vector_phys_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(field_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_vector_phys_ctl = C_loc(f_ctl%vector_phys)
      end function c_MHD_vector_phys_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_evolution_ctl_block_name(c_ctl)        &
     &          bind(C, NAME = 'c_MHD_evolution_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evolution_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_evolution_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_evolution_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_evolution_ctl_iflag(c_ctl)             &
     &          bind(C, NAME = 'c_MHD_evolution_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evolution_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_evolution_ctl_iflag = C_loc(f_ctl%i_time_evo)
      end function c_MHD_evolution_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_t_evo_field_ctl(c_ctl)                 &
     &          bind(C, NAME = 'c_MHD_t_evo_field_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evolution_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_t_evo_field_ctl = C_loc(f_ctl%t_evo_field_ctl)
      end function c_MHD_t_evo_field_ctl
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_forces_block_name(c_ctl)               &
     &          bind(C, NAME = 'c_MHD_forces_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(forces_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_forces_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_forces_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_forces_iflag(c_ctl)                    &
     &          bind(C, NAME = 'c_MHD_forces_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(forces_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_forces_iflag = C_loc(f_ctl%i_forces_ctl)
      end function c_MHD_forces_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_forces_array(c_ctl)                    &
     &          bind(C, NAME = 'c_MHD_forces_array')
      type(c_ptr), value, intent(in) :: c_ctl
      type(forces_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_forces_array = C_loc(f_ctl%force_names)
      end function c_MHD_forces_array
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_evo_area_ctl_block_name(c_ctl)         &
     &          bind(C, NAME = 'c_MHD_evo_area_ctl_block_name')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_area_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_evo_area_ctl_block_name = C_loc(f_ctl%block_name)
      end function c_MHD_evo_area_ctl_block_name
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_evo_area_ctl_iflag(c_ctl)              &
     &          bind(C, NAME = 'c_MHD_evo_area_ctl_iflag')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_area_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_evo_area_ctl_iflag = C_loc(f_ctl%i_layers_ctl)
      end function c_MHD_evo_area_ctl_iflag
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_evo_fluid_group_ctl(c_ctl)             &
     &          bind(C, NAME = 'c_MHD_evo_fluid_group_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_area_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_evo_fluid_group_ctl = C_loc(f_ctl%evo_fluid_group_ctl)
      end function c_MHD_evo_fluid_group_ctl
!
!  ---------------------------------------------------------------------
!
      type(c_ptr) function c_MHD_evo_conduct_group_ctl(c_ctl)           &
     &          bind(C, NAME = 'c_MHD_evo_conduct_group_ctl')
      type(c_ptr), value, intent(in) :: c_ctl
      type(mhd_evo_area_control), pointer :: f_ctl
      call c_f_pointer(c_ctl, f_ctl)
      c_MHD_evo_conduct_group_ctl = C_loc(f_ctl%evo_conduct_group_ctl)
      end function c_MHD_evo_conduct_group_ctl
!
!  ---------------------------------------------------------------------
!
      end module c_link_MHD_field_evolve_ctl
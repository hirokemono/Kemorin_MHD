!>@file   check_diffusion_w_symmetry.f90
!!        module check_diffusion_w_symmetry
!!
!! @author T. Kera
!! @date   Programmed in Jan., 2026
!!
!!
!> @brief Check Dependecies for diffusion with symmetry
!!
!!@verbatim
!!      subroutine add_diffusion_w_symmetry_ctl(field_ctl)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!@endverbatim
!!
      module check_diffusion_w_symmetry
!
      use m_precision
      use m_constants
!
      use t_base_field_labels
      use m_diffusion_term_w_sym_labels
!
      implicit  none
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine add_diffusion_w_symmetry_ctl(field_ctl)
!
      use t_control_array_character3
      use m_diffusion_term_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(      check_field_list_ctl(sym_viscous_diffusion, field_ctl)            &
     &    .or. check_field_list_ctl(asym_viscous_diffusion, field_ctl)) then
        call add_phys_name_ctl(viscous_diffusion, field_ctl)
      end if
      if( check_field_list_ctl(sym_vorticity_diffusion, field_ctl)                &
     &    .or. check_field_list_ctl(asym_vorticity_diffusion, field_ctl)) then
        call add_phys_name_ctl(vorticity_diffusion, field_ctl)
      end if
      if( check_field_list_ctl(sym_magnetic_diffusion, field_ctl)           &
     &    .or. check_field_list_ctl(asym_magnetic_diffusion,                &
     &                              field_ctl)) then
        call add_phys_name_ctl(magnetic_diffusion, field_ctl)
      end if
      if( check_field_list_ctl(sym_vector_potential_diffusion, field_ctl)         &
     &    .or. check_field_list_ctl(asym_vector_potential_diffusion,              &
     &                              field_ctl)) then
        call add_phys_name_ctl(vector_potential_diffusion, field_ctl)
      end if
      if( check_field_list_ctl(sym_thermal_diffusion, field_ctl)          &
     &    .or. check_field_list_ctl(asym_thermal_diffusion,               &
     &                              field_ctl)) then
        call add_phys_name_ctl(thermal_diffusion, field_ctl)
      end if
!
      if( check_field_list_ctl(sym_composition_diffusion, field_ctl)                 &
     &    .or. check_field_list_ctl(asym_composition_diffusion, field_ctl)) then
        call add_phys_name_ctl(composition_diffusion, field_ctl)
      end if
      if( check_field_list_ctl(sym_div_viscousity, field_ctl)       &
     &    .or. check_field_list_ctl(asym_div_viscousity, field_ctl) &
     &   ) then
        call add_phys_name_ctl(div_viscousity, field_ctl)
      end if
!
      end subroutine add_diffusion_w_symmetry_ctl
!
! -----------------------------------------------------------------------
!
      end module check_diffusion_w_symmetry
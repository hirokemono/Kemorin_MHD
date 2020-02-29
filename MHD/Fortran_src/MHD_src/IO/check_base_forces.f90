!>@file   check_base_forces.f90
!!        module check_base_forces
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields
!!
!!@verbatim
!!      subroutine add_field_ctl_4_forces(field_ctl)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!@endverbatim
!!
      module check_base_forces
!
      use m_precision
      use m_constants
!
      use t_base_field_labels
      use t_base_force_labels
!
      implicit  none
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_forces(field_ctl)
!
      use t_control_array_character3
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(magnetic_induction%name, field_ctl)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_magne, field_ctl)
        call add_phys_name_ctl(vecp_induction%name, field_ctl)
      end if
!
      if(check_field_list_ctl(pressure_gradient%name, field_ctl))       &
        call add_phys_name_ctl(fhd_press, field_ctl)
      if(check_field_list_ctl(inertia%name, field_ctl)) then
        call add_phys_name_ctl(fhd_vort, field_ctl)
        call add_phys_name_ctl(fhd_press, field_ctl)
      end if
      if(check_field_list_ctl(Lorentz_force%name, field_ctl)) then
        call add_phys_name_ctl(fhd_current, field_ctl)
        call add_phys_name_ctl(fhd_press, field_ctl)
      end if
!
      if(check_field_list_ctl(buoyancy%name, field_ctl))                &
        call add_phys_name_ctl(fhd_temp, field_ctl)
      if(check_field_list_ctl(composite_buoyancy%name, field_ctl))      &
        call add_phys_name_ctl(fhd_light, field_ctl)
!
      if(      check_field_list_ctl(Coriolis_force%name, field_ctl)     &
     &    .or. check_field_list_ctl(momentum_flux%name, field_ctl))     &
        call add_phys_name_ctl(fhd_velo, field_ctl)
      if(      check_field_list_ctl(magnetic_tension%name, field_ctl)   &
      &    .or. check_field_list_ctl(maxwell_tensor%name, field_ctl))   &
        call add_phys_name_ctl(fhd_magne, field_ctl)
!
      if(      check_field_list_ctl(vecp_induction%name, field_ctl)     &
     &    .or. check_field_list_ctl(magnetic_stretch%name, field_ctl)   &
     &    .or. check_field_list_ctl(induction_tensor%name, field_ctl)   &
     &      ) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_magne, field_ctl)
      end if
!
      if(      check_field_list_ctl(heat_advect%name, field_ctl)        &
     &    .or. check_field_list_ctl(heat_flux%name, field_ctl)          &
     &      ) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_temp, field_ctl)
      end if
      if(      check_field_list_ctl(pert_heat_advect%name, field_ctl)   &
     &    .or. check_field_list_ctl(pert_heat_flux%name, field_ctl)     &
     &      ) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_part_temp, field_ctl)
      end if
      if(      check_field_list_ctl(composition_advect%name, field_ctl) &
     &    .or. check_field_list_ctl(composite_flux%name, field_ctl)     &
     &      ) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_light, field_ctl)
      end if
      if(      check_field_list_ctl(pert_comp_advect%name, field_ctl)   &
     &    .or. check_field_list_ctl(pert_comp_flux%name, field_ctl)     &
     &      ) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_part_light, field_ctl)
      end if
!
      end subroutine add_field_ctl_4_forces
!
! -----------------------------------------------------------------------
!
      end module check_base_forces

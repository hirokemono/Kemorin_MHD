!>@file   check_filtered_forces.f90
!!        module check_filtered_forces
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for filtered forces
!!
!!@verbatim
!!      subroutine add_field_ctl_4_filter_forces(field_ctl)
!!      subroutine add_field_ctl_4_rot_fil_forces(field_ctl)
!!      subroutine add_field_ctl_4_div_fil_forces(field_ctl)
!!      subroutine add_field_ctl_4_fil_ene_flux(field_ctl)
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!@endverbatim
!!
      module check_filtered_forces
!
      use m_precision
      use m_constants
!
      use t_control_array_character3
      use m_filtered_field_labels
      use m_filtered_force_labels
!
      implicit  none
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_filter_forces(field_ctl)
!
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(inertia_by_filtered%name,                 &
     &                        field_ctl)) then
         call add_phys_name_ctl(filter_velocity%name, field_ctl)
         call add_phys_name_ctl(filter_vorticity%name, field_ctl)
      end if
      if(check_field_list_ctl(momentum_flux_by_filtered%name,           &
     &        field_ctl))                                               &
     &   call add_phys_name_ctl(filter_velocity%name, field_ctl)
!
      if(check_field_list_ctl(Lorentz_force_by_filtered%name,           &
     &                        field_ctl)) then
         call add_phys_name_ctl(filter_magne%name, field_ctl)
         call add_phys_name_ctl(filter_current%name, field_ctl)
      end if
      if(     check_field_list_ctl(magnetic_tension_by_filtered%name,   &
     &                             field_ctl)                           &
     &   .or. check_field_list_ctl(maxwell_tensor_by_filtered%name,     &
     &                             field_ctl)) then
         call add_phys_name_ctl(filter_magne%name, field_ctl)
      end if
!
      if(check_field_list_ctl(filtered_buoyancy%name, field_ctl))       &
     &   call add_phys_name_ctl(filter_temperature%name, field_ctl)
      if(check_field_list_ctl(filtered_comp_buoyancy%name, field_ctl))  &
     &   call add_phys_name_ctl(filter_composition%name, field_ctl)
!
      if(     check_field_list_ctl(vecp_induction_by_filtered%name,     &
     &                             field_ctl)                           &
     &   .or. check_field_list_ctl(magnetic_stretch_by_filtered%name,   &
     &                             field_ctl)                           &
     &   .or. check_field_list_ctl(induction_tensor_by_filtered%name,   &
     &                             field_ctl)) then
         call add_phys_name_ctl(filter_velocity%name, field_ctl)
         call add_phys_name_ctl(filter_magne%name, field_ctl)
      end if
!
      if(check_field_list_ctl(heat_advect_by_filtered%name, field_ctl)  &
     &   .or. check_field_list_ctl(heat_flux_by_filtered%name,          &
     &                             field_ctl)) then
         call add_phys_name_ctl(filter_velocity%name, field_ctl)
         call add_phys_name_ctl(filter_temperature%name, field_ctl)
      end if
      if(     check_field_list_ctl(pert_h_advect_by_filtered%name,      &
     &                             field_ctl)                           &
     &   .or. check_field_list_ctl(pert_h_flux_by_filtered%name,        &
     &                             field_ctl)) then
         call add_phys_name_ctl(filter_velocity%name, field_ctl)
         call add_phys_name_ctl(filter_pert_temperature%name,           &
     &                          field_ctl)
      end if
!
      if(check_field_list_ctl(comp_advect_by_filtered%name, field_ctl)  &
     &   .or. check_field_list_ctl(composite_flux_by_filtered%name,     &
     &                             field_ctl)) then
         call add_phys_name_ctl(filter_velocity%name, field_ctl)
         call add_phys_name_ctl(filter_composition%name, field_ctl)
      end if
      if(     check_field_list_ctl(pert_c_advect_by_filtered%name,      &
     &                             field_ctl)                           &
     &   .or. check_field_list_ctl(pert_c_flux_by_filtered%name,        &
     &                             field_ctl)) then
         call add_phys_name_ctl(filter_velocity%name, field_ctl)
         call add_phys_name_ctl                                         &
     &      (filter_pert_composition%name, field_ctl)
      end if
!
      end subroutine add_field_ctl_4_filter_forces
!
! ----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_rot_fil_forces(field_ctl)
!
      use m_rot_filtered_force_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if    (check_field_list_ctl(rot_inertia_by_filtered%name,         &
     &        field_ctl))                                               &
     &   call add_phys_name_ctl(inertia_by_filtered%name, field_ctl)
      if(check_field_list_ctl(rot_Lorentz_force_by_filtered%name,       &
     &        field_ctl))                                               &
     &   call add_phys_name_ctl                                         &
     &      (Lorentz_force_by_filtered%name, field_ctl)
!
      if(check_field_list_ctl(rot_filtered_buoyancy%name,               &
     &        field_ctl))                                               &
     &   call add_phys_name_ctl(filtered_buoyancy%name, field_ctl)
      if(check_field_list_ctl(rot_filtered_comp_buoyancy%name,          &
     &        field_ctl))                                               &
     &   call add_phys_name_ctl(filtered_comp_buoyancy%name, field_ctl)
!
      if(check_field_list_ctl(magnetic_induction_by_filtered%name,      &
     &        field_ctl))                                               &
     &   call add_phys_name_ctl                                         &
     &      (vecp_induction_by_filtered%name, field_ctl)
!
      end subroutine add_field_ctl_4_rot_fil_forces
!
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_div_fil_forces(field_ctl)
!
      use m_div_filtered_force_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(div_inertia_by_filtered%name,             &
     &        field_ctl))                                               &
     &   call add_phys_name_ctl(inertia_by_filtered%name, field_ctl)
      if(check_field_list_ctl(div_Lorentz_force_by_filtered%name,       &
     &        field_ctl))                                               &
     &   call add_phys_name_ctl                                         &
     &      (Lorentz_force_by_filtered%name, field_ctl)
!
      if(check_field_list_ctl(div_filtered_buoyancy%name,               &
     &        field_ctl))                                               &
     &   call add_phys_name_ctl(filtered_buoyancy%name, field_ctl)
      if(check_field_list_ctl(div_filtered_comp_buoyancy%name,          &
     &        field_ctl))                                               &
     &   call add_phys_name_ctl(filtered_comp_buoyancy%name, field_ctl)
!
      if(check_field_list_ctl(div_vecp_induction_by_filtered%name,      &
     &        field_ctl))                                               &
     &   call add_phys_name_ctl                                         &
     &      (vecp_induction_by_filtered%name, field_ctl)
!
      if(check_field_list_ctl(div_m_flux_by_filtered%name,              &
     &        field_ctl))                                               &
     &   call add_phys_name_ctl                                         &
     &      (momentum_flux_by_filtered%name, field_ctl)
      if(check_field_list_ctl(div_maxwell_t_by_filtered%name,           &
     &        field_ctl))                                               &
     &   call add_phys_name_ctl                                         &
     &      (maxwell_tensor_by_filtered%name, field_ctl)
      if(check_field_list_ctl(div_induct_t_by_filtered%name,            &
     &        field_ctl))                                               &
     &   call add_phys_name_ctl                                         &
     &      (induction_tensor_by_filtered%name, field_ctl)
!
      if(check_field_list_ctl(div_h_flux_by_filtered%name,              &
     &        field_ctl))                                               &
     &   call add_phys_name_ctl(heat_flux_by_filtered%name, field_ctl)
      if(check_field_list_ctl(div_pert_h_flux_by_filtered%name,         &
     &        field_ctl))                                               &
     &   call add_phys_name_ctl                                         &
     &      (pert_h_flux_by_filtered%name, field_ctl)
      if(check_field_list_ctl(div_c_flux_by_filtered%name,              &
     &        field_ctl))                                               &
     &   call add_phys_name_ctl                                         &
     &      (composite_flux_by_filtered%name, field_ctl)
      if(check_field_list_ctl(div_pert_c_flux_by_filtered%name,         &
     &        field_ctl))                                               &
     &   call add_phys_name_ctl                                         &
     &      (pert_c_flux_by_filtered%name, field_ctl)
!
      end subroutine add_field_ctl_4_div_fil_forces
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_fil_ene_flux(field_ctl)
!
      use m_filtered_ene_flux_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(inertia_work_by_filtered%name,            &
     &                        field_ctl)) then
         call add_phys_name_ctl(velocity%name, field_ctl)
         call add_phys_name_ctl(inertia_by_filtered%name, field_ctl)
      end if
!
      if(check_field_list_ctl(wk_against_Lorentz_by_filtered%name,      &
     &                        field_ctl)                                &
     &     .or. check_field_list_ctl(Lorentz_work_by_filtered%name,     &
     &                        field_ctl)) then
         call add_phys_name_ctl(velocity%name, field_ctl)
         call add_phys_name_ctl                                         &
     &      (Lorentz_force_by_filtered%name, field_ctl)
      end if
      if(check_field_list_ctl(mag_tension_work_by_filtered%name,        &
     &                        field_ctl)) then
         call add_phys_name_ctl(velocity%name, field_ctl)
         call add_phys_name_ctl                                         &
     &      (magnetic_tension_by_filtered%name, field_ctl)
      end if
!
      if(check_field_list_ctl(filtered_buoyancy_flux%name,              &
     &                        field_ctl)) then
         call add_phys_name_ctl(velocity%name, field_ctl)
         call add_phys_name_ctl(filtered_buoyancy%name, field_ctl)
      end if
      if(check_field_list_ctl(filtered_comp_buoyancy_flux%name,         &
     &                        field_ctl)) then
         call add_phys_name_ctl(velocity%name, field_ctl)
         call add_phys_name_ctl(filtered_comp_buoyancy%name, field_ctl)
      end if
!
      if(check_field_list_ctl(mag_ene_generation_by_filtered%name,      &
     &                        field_ctl)) then
         call add_phys_name_ctl(magnetic_field%name, field_ctl)
         call add_phys_name_ctl                                         &
     &      (vecp_induction_by_filtered%name, field_ctl)
      end if
      if(check_field_list_ctl(mag_stretch_flux_by_filtered%name,        &
     &                        field_ctl)) then
         call add_phys_name_ctl(magnetic_field%name, field_ctl)
         call add_phys_name_ctl                                         &
     &      (magnetic_stretch_by_filtered%name, field_ctl)
      end if
!
      if(check_field_list_ctl(temp_generation_by_filtered%name,         &
     &                        field_ctl)) then
         call add_phys_name_ctl(temperature%name, field_ctl)
         call add_phys_name_ctl                                         &
     &      (heat_advect_by_filtered%name, field_ctl)
      end if
      if(check_field_list_ctl(part_temp_gen_by_filtered%name,           &
     &                        field_ctl)) then
         call add_phys_name_ctl(fhd_part_temp, field_ctl)
         call add_phys_name_ctl                                         &
     &      (pert_h_advect_by_filtered%name, field_ctl)
      end if
!
      if(check_field_list_ctl(comp_generation_by_filtered%name,         &
     &                        field_ctl)) then
         call add_phys_name_ctl(fhd_light, field_ctl)
         call add_phys_name_ctl                                         &
     &      (comp_advect_by_filtered%name, field_ctl)
      end if
      if(check_field_list_ctl(part_comp_gen_by_filtered%name,           &
     &                        field_ctl)) then
         call add_phys_name_ctl(fhd_part_light, field_ctl)
         call add_phys_name_ctl                                         &
     &      (pert_c_advect_by_filtered%name, field_ctl)
      end if
!
      end subroutine add_field_ctl_4_fil_ene_flux
!
! ----------------------------------------------------------------------
!
      end module check_filtered_forces

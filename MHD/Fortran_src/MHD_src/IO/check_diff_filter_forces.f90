!>@file   check_diff_filter_forces.f90
!!        module check_diff_filter_forces
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields
!!
!!@verbatim
!!      subroutine add_field_ctl_4_rot_fil_forces(field_ctl)
!!      subroutine add_field_ctl_4_div_fil_forces(field_ctl)
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!@endverbatim
!!
      module check_diff_filter_forces
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
     &   call add_phys_name_ctl(fhd_filter_temp, field_ctl)
      if(check_field_list_ctl(rot_filtered_comp_buoyancy%name,          &
     &        field_ctl))                                               &
     &   call add_phys_name_ctl(fhd_filter_comp, field_ctl)
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
     &   call add_phys_name_ctl(fhd_filter_temp, field_ctl)
      if(check_field_list_ctl(div_filtered_comp_buoyancy%name,          &
     &        field_ctl))                                               &
     &   call add_phys_name_ctl(fhd_filter_comp, field_ctl)
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
      if(check_field_list_ctl(div_part_h_flux_by_filtered%name,         &
     &        field_ctl))                                               &
     &   call add_phys_name_ctl                                         &
     &      (part_h_flux_by_filtered%name, field_ctl)
      if(check_field_list_ctl(div_c_flux_by_filtered%name,              &
     &        field_ctl))                                               &
     &   call add_phys_name_ctl                                         &
     &      (composite_flux_by_filtered%name, field_ctl)
      if(check_field_list_ctl(div_part_c_flux_by_filtered%name,         &
     &        field_ctl))                                               &
     &   call add_phys_name_ctl                                         &
     &      (part_c_flux_by_filtered%name, field_ctl)
!
      end subroutine add_field_ctl_4_div_fil_forces
!
! ----------------------------------------------------------------------
!
      end module check_diff_filter_forces

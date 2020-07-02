!>@file   check_double_filter_field.f90
!!        module check_double_filter_field
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields
!!
!!@verbatim
!!      subroutine add_field_ctl_4_wide_fil_field(field_ctl)
!!      subroutine add_field_ctl_4_dbl_fil_field(field_ctl)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!@endverbatim
!!
      module check_double_filter_field
!
      use m_precision
      use m_constants
      use t_base_field_labels
      use m_filtered_field_labels
!
      implicit  none
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_wide_fil_field(field_ctl)
!
      use t_control_array_character3
      use m_wide_filter_field_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(wide_filter_vorticity%name, field_ctl))   &
     &  call add_phys_name_ctl(wide_filter_velocity, field_ctl)
      if(check_field_list_ctl(wide_filter_current%name, field_ctl))     &
     &  call add_phys_name_ctl(wide_filter_magne, field_ctl)
!
      if(check_field_list_ctl(wide_filter_grad_temp%name, field_ctl))   &
     &  call add_phys_name_ctl(wide_filter_temp, field_ctl)
      if(check_field_list_ctl(wide_filter_grad_composition%name,        &
     &                        field_ctl))                               &
     &  call add_phys_name_ctl(wide_filter_composition, field_ctl)
!
      if(check_field_list_ctl(wide_filter_velocity%name, field_ctl))    &
     &  call add_phys_name_ctl(filter_velocity, field_ctl)
      if(check_field_list_ctl(wide_filter_magne%name, field_ctl))       &
     &  call add_phys_name_ctl(filter_magne, field_ctl)
      if(check_field_list_ctl(wide_filter_vector_potential%name,        &
     &                        field_ctl))                               &
     &  call add_phys_name_ctl(filter_vector_potential, field_ctl)
!
      if(check_field_list_ctl(wide_filter_temp%name, field_ctl))        &
     &  call add_phys_name_ctl(filter_temperature, field_ctl)
      if(check_field_list_ctl(wide_filter_composition%name, field_ctl)) &
     &  call add_phys_name_ctl(filter_composition, field_ctl)
      if(check_field_list_ctl(wide_filter_density%name, field_ctl))     &
     &  call add_phys_name_ctl(filter_density, field_ctl)
      if(check_field_list_ctl(wide_filter_entropy%name, field_ctl))     &
     &  call add_phys_name_ctl(filter_entropy, field_ctl)
!
      if(check_field_list_ctl(wide_filter_pert_temp%name, field_ctl))   &
     &  call add_phys_name_ctl(filter_pert_temperature, field_ctl)
      if(check_field_list_ctl(wide_filter_pert_comp%name, field_ctl))   &
     &  call add_phys_name_ctl(filter_pert_composition, field_ctl)
      if(check_field_list_ctl(wide_filter_pert_density%name,            &
     &                        field_ctl))                               &
     &  call add_phys_name_ctl(filter_pert_density, field_ctl)
      if(check_field_list_ctl(wide_filter_pert_entropy%name,            &
     &                        field_ctl))                               &
     &  call add_phys_name_ctl(filter_pert_entropy, field_ctl)
!
      end subroutine add_field_ctl_4_wide_fil_field
!
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_dbl_fil_field(field_ctl)
!
      use t_control_array_character3
      use m_dble_filter_field_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(double_filter_vorticity%name, field_ctl)) &
     &  call add_phys_name_ctl(double_filter_velocity, field_ctl)
      if(check_field_list_ctl(double_filter_current%name, field_ctl))   &
     &  call add_phys_name_ctl(double_filter_magne, field_ctl)
!
      if(check_field_list_ctl(double_filter_grad_temp%name, field_ctl)) &
     &  call add_phys_name_ctl(double_filter_temp, field_ctl)
      if(check_field_list_ctl(double_filter_grad_comp%name, field_ctl)) &
     &  call add_phys_name_ctl(double_filter_composition, field_ctl)
!
      if(check_field_list_ctl(double_filter_velocity%name, field_ctl))  &
     &  call add_phys_name_ctl(filter_velocity, field_ctl)
      if(check_field_list_ctl(double_filter_magne%name, field_ctl))     &
     &  call add_phys_name_ctl(filter_magne, field_ctl)
      if(check_field_list_ctl(double_filter_vector_potential%name,      &
     &                        field_ctl))                               &
     &  call add_phys_name_ctl(filter_vector_potential, field_ctl)
!
      if(check_field_list_ctl(double_filter_temp%name, field_ctl))      &
     &  call add_phys_name_ctl(filter_temperature, field_ctl)
      if(check_field_list_ctl(double_filter_composition%name,           &
     &                        field_ctl))                               &
     &  call add_phys_name_ctl(filter_composition, field_ctl)
      if(check_field_list_ctl(double_filter_density%name, field_ctl))   &
     &  call add_phys_name_ctl(filter_density, field_ctl)
      if(check_field_list_ctl(double_filter_entropy%name, field_ctl))   &
     &  call add_phys_name_ctl(filter_entropy, field_ctl)
!
      if(check_field_list_ctl(double_filter_pert_temp%name, field_ctl)) &
     &  call add_phys_name_ctl(filter_pert_temperature, field_ctl)
      if(check_field_list_ctl(double_filter_pert_comp%name, field_ctl)) &
     &  call add_phys_name_ctl(filter_pert_composition, field_ctl)
      if(check_field_list_ctl(double_filter_pert_density%name,          &
     &                        field_ctl))                               &
     &  call add_phys_name_ctl(filter_pert_density, field_ctl)
      if(check_field_list_ctl(double_filter_pert_entropy%name,          &
     &                        field_ctl))                               &
     &  call add_phys_name_ctl(filter_pert_entropy, field_ctl)
!
      end subroutine add_field_ctl_4_dbl_fil_field
!
! -----------------------------------------------------------------------
!
      end module check_double_filter_field

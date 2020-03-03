!>@file   check_differenceate_fields.f90
!!        module check_differenceate_fields
!!
!! @author H. Matsui
!! @date   Programmed in Feb., 2020
!!
!!
!> @brief Labels and addresses for differenciate of fields
!!
!!@verbatim
!!      subroutine add_field_ctl_4_grad_field(field_ctl)
!!      subroutine add_field_ctl_4_diff_vector(field_ctl)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!@endverbatim
!!
      module check_differenceate_fields
!
      use m_precision
      use m_constants
!
      use t_control_array_character3
!
      implicit  none
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_grad_field(field_ctl)
!
      use t_grad_field_labels
      use t_base_field_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(div_velocity%name, field_ctl))            &
     &   call add_phys_name_ctl(fhd_velo, field_ctl)
      if(check_field_list_ctl(div_magnetic%name, field_ctl))            &
     &   call add_phys_name_ctl(fhd_magne, field_ctl)
      if(check_field_list_ctl(div_vector_potential%name, field_ctl))    &
     &   call add_phys_name_ctl(fhd_vecp, field_ctl)
!
      if(check_field_list_ctl(grad_temp%name, field_ctl))               &
     &   call add_phys_name_ctl(fhd_temp, field_ctl)
      if(check_field_list_ctl(grad_pert_temp%name, field_ctl))          &
     &   call add_phys_name_ctl(fhd_part_temp, field_ctl)
      if(check_field_list_ctl(grad_reference_temp%name, field_ctl))    &
     &   call add_phys_name_ctl(fhd_ref_temp, field_ctl)
!
      if(check_field_list_ctl(grad_composition%name, field_ctl))        &
     &   call add_phys_name_ctl(fhd_light, field_ctl)
      if(check_field_list_ctl(grad_pert_composition%name, field_ctl))   &
     &   call add_phys_name_ctl(fhd_part_light, field_ctl)
      if(check_field_list_ctl(grad_reference_composition%name,          &
     &                        field_ctl))                               &
     &   call add_phys_name_ctl(fhd_ref_light, field_ctl)
!
      if(check_field_list_ctl(grad_density%name, field_ctl))            &
     &   call add_phys_name_ctl(fhd_density, field_ctl)
      if(check_field_list_ctl(grad_pert_density%name, field_ctl))       &
     &   call add_phys_name_ctl(fhd_per_density, field_ctl)
      if(check_field_list_ctl(grad_reference_density%name, field_ctl))  &
     &   call add_phys_name_ctl(fhd_ref_density, field_ctl)
!
      if(check_field_list_ctl(grad_entropy%name, field_ctl))            &
     &   call add_phys_name_ctl(fhd_entropy, field_ctl)
      if(check_field_list_ctl(grad_pert_entropy%name, field_ctl))       &
     &   call add_phys_name_ctl(fhd_per_entropy, field_ctl)
      if(check_field_list_ctl(grad_reference_entropy%name, field_ctl))  &
     &   call add_phys_name_ctl(fhd_ref_entropy, field_ctl)
!
      end subroutine add_field_ctl_4_grad_field
!
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_diff_vector(field_ctl)
!
      use t_diff_vector_labels
      use t_base_field_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(   check_field_list_ctl(grad_v_1%name, field_ctl)              &
     & .or. check_field_list_ctl(grad_v_2%name, field_ctl)              &
     & .or. check_field_list_ctl(grad_v_3%name, field_ctl))             &
     &  call add_phys_name_ctl(fhd_velo, field_ctl)
      if(   check_field_list_ctl(grad_w_1%name, field_ctl)              &
     & .or. check_field_list_ctl(grad_w_2%name, field_ctl)              &
     & .or. check_field_list_ctl(grad_w_3%name, field_ctl))             &
     &  call add_phys_name_ctl(fhd_vort, field_ctl)
!
      if(   check_field_list_ctl(grad_b_1%name, field_ctl)              &
     & .or. check_field_list_ctl(grad_b_2%name, field_ctl)              &
     & .or. check_field_list_ctl(grad_b_3%name, field_ctl))             &
     &  call add_phys_name_ctl(fhd_magne, field_ctl)
      if(   check_field_list_ctl(grad_a_1%name, field_ctl)              &
     & .or. check_field_list_ctl(grad_a_2%name, field_ctl)              &
     & .or. check_field_list_ctl(grad_a_3%name, field_ctl))             &
     &  call add_phys_name_ctl(fhd_vecp, field_ctl)
      if(   check_field_list_ctl(grad_j_1%name, field_ctl)              &
     & .or. check_field_list_ctl(grad_j_2%name, field_ctl)              &
     & .or. check_field_list_ctl(grad_j_3%name, field_ctl))             &
     &  call add_phys_name_ctl(fhd_current, field_ctl)
!
      end subroutine add_field_ctl_4_diff_vector
!
! -----------------------------------------------------------------------
!
      end module check_differenceate_fields

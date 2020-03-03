!>@file   check_filtered_field.f90
!!        module check_filtered_field
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for basic fields
!!
!!@verbatim
!!      subroutine add_filter_field_ctl(field_ctl)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!
!!      subroutine add_field_ctl_4_grad_fil_field(field_ctl)
!!      subroutine add_field_ctl_4_diff_fil_vect(field_ctl)
!!        type(ctl_array_c3), intent(in) :: field_ctl
!!@endverbatim
!!
      module check_filtered_field
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
      subroutine add_filter_field_ctl(field_ctl)
!
      use t_control_array_character3
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(fhd_filter_vort, field_ctl)) then
        call add_phys_name_ctl(fhd_filter_velo, field_ctl)
      end if
      if(check_field_list_ctl(fhd_filter_current, field_ctl)) then
        call add_phys_name_ctl(fhd_filter_magne, field_ctl)
      end if
!
      if(     check_field_list_ctl(fhd_filter_velo, field_ctl)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
      end if
      if(check_field_list_ctl(fhd_filter_magne, field_ctl)) then
        call add_phys_name_ctl(fhd_magne, field_ctl)
      end if
      if(check_field_list_ctl(fhd_filter_vecp, field_ctl)) then
        call add_phys_name_ctl(fhd_vecp, field_ctl)
      end if
!
      if(check_field_list_ctl(fhd_filter_temp, field_ctl)) then
        call add_phys_name_ctl(fhd_temp, field_ctl)
      end if
      if(check_field_list_ctl(fhd_filter_comp, field_ctl)) then
        call add_phys_name_ctl(fhd_light, field_ctl)
      end if
      if(check_field_list_ctl(fhd_filter_density, field_ctl)) then
        call add_phys_name_ctl(fhd_density, field_ctl)
      end if
      if(check_field_list_ctl(fhd_filter_entropy, field_ctl)) then
        call add_phys_name_ctl(fhd_entropy, field_ctl)
      end if
!
      if(check_field_list_ctl(fhd_filter_pert_temp, field_ctl)) then
        call add_phys_name_ctl(fhd_part_temp, field_ctl)
      end if
      if(check_field_list_ctl(fhd_filter_pert_comp, field_ctl)) then
        call add_phys_name_ctl(fhd_part_light, field_ctl)
      end if
      if(check_field_list_ctl(fhd_filter_pert_density, field_ctl)) then
        call add_phys_name_ctl(fhd_per_density, field_ctl)
      end if
      if(check_field_list_ctl(fhd_filter_pert_entropy, field_ctl)) then
        call add_phys_name_ctl(fhd_per_entropy, field_ctl)
      end if
!
      end subroutine add_filter_field_ctl
!
! -----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_grad_fil_field(field_ctl)
!
      use m_grad_filter_field_labels
      use m_filtered_field_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(div_filtered_velo%name, field_ctl))       &
     &   call add_phys_name_ctl(fhd_filter_velo, field_ctl)
      if(check_field_list_ctl(div_filtered_magne%name, field_ctl))      &
     &   call add_phys_name_ctl(fhd_filter_magne, field_ctl)
      if(check_field_list_ctl(div_filtered_vector_potential%name,       &
     &                        field_ctl))                               &
     &   call add_phys_name_ctl(fhd_filter_vecp, field_ctl)
!
      if(check_field_list_ctl(grad_filtered_temp%name, field_ctl))      &
     &   call add_phys_name_ctl(fhd_filter_temp, field_ctl)
      if(check_field_list_ctl(grad_filtered_pert_temp%name, field_ctl)) &
     &   call add_phys_name_ctl(fhd_filter_pert_temp, field_ctl)
!
      if(check_field_list_ctl(grad_filtered_comp%name, field_ctl))      &
     &   call add_phys_name_ctl(fhd_filter_comp, field_ctl)
      if(check_field_list_ctl(grad_filtered_pert_comp%name, field_ctl)) &
     &   call add_phys_name_ctl(fhd_per_density, field_ctl)
!
      if(check_field_list_ctl(grad_filtered_density%name, field_ctl))   &
     &   call add_phys_name_ctl(fhd_filter_density, field_ctl)
      if(check_field_list_ctl(grad_filtered_pert_density%name,          &
     &                        field_ctl))                               &
     &   call add_phys_name_ctl(fhd_filter_pert_density, field_ctl)
!
      if(check_field_list_ctl(grad_filtered_entropy%name, field_ctl))   &
     &   call add_phys_name_ctl(fhd_filter_entropy, field_ctl)
      if(check_field_list_ctl(grad_filtered_pert_entropy%name,          &
     &                        field_ctl))                               &
     &   call add_phys_name_ctl(fhd_filter_pert_entropy, field_ctl)
!
      end subroutine add_field_ctl_4_grad_fil_field
!
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_diff_fil_vect(field_ctl)
!
      use m_diff_filter_vect_labels
      use m_filtered_field_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(   check_field_list_ctl(grad_filtered_v_1%name, field_ctl)     &
     & .or. check_field_list_ctl(grad_filtered_v_2%name, field_ctl)     &
     & .or. check_field_list_ctl(grad_filtered_v_3%name, field_ctl))    &
     &  call add_phys_name_ctl(fhd_filter_velo, field_ctl)
      if(   check_field_list_ctl(grad_filtered_w_1%name, field_ctl)     &
     & .or. check_field_list_ctl(grad_filtered_w_2%name, field_ctl)     &
     & .or. check_field_list_ctl(grad_filtered_w_3%name, field_ctl))    &
     &  call add_phys_name_ctl(fhd_filter_vort, field_ctl)
!
      if(   check_field_list_ctl(grad_filtered_b_1%name, field_ctl)     &
     & .or. check_field_list_ctl(grad_filtered_b_2%name, field_ctl)     &
     & .or. check_field_list_ctl(grad_filtered_b_3%name, field_ctl))    &
     &  call add_phys_name_ctl(fhd_filter_magne, field_ctl)
      if(   check_field_list_ctl(grad_filtered_a_1%name, field_ctl)     &
     & .or. check_field_list_ctl(grad_filtered_a_2%name, field_ctl)     &
     & .or. check_field_list_ctl(grad_filtered_a_3%name, field_ctl))    &
     &  call add_phys_name_ctl(fhd_filter_vecp, field_ctl)
      if(   check_field_list_ctl(grad_filtered_j_1%name, field_ctl)     &
     & .or. check_field_list_ctl(grad_filtered_j_2%name, field_ctl)     &
     & .or. check_field_list_ctl(grad_filtered_j_3%name, field_ctl))    &
     &  call add_phys_name_ctl(fhd_filter_current, field_ctl)
!
      end subroutine add_field_ctl_4_diff_fil_vect
!
! -----------------------------------------------------------------------
!
      end module check_filtered_field

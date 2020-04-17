!>@file   check_wide_SGS_terms.f90
!!        module check_wide_SGS_terms
!!
!! @author H. Matsui
!! @date   Programmed in March, 2020
!!
!!
!>@brief Add labels for SGS terms by wider filtering
!!
!!@verbatim
!!      subroutine add_field_ctl_4_grad_wide_SGS(field_ctl)
!!      subroutine add_field_ctl_4_simi_wide_SGS(field_ctl)
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!
!!      subroutine add_field_ctl_4_simi_wide_SGS(field_ctl)
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!@endverbatim
!!
      module check_wide_SGS_terms
!
      use m_precision
      use m_constants
!
      use t_base_field_labels
!
      implicit  none
! 
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_grad_wide_SGS(field_ctl)
!
      use t_control_array_character3
      use t_SGS_term_labels
      use m_filtered_field_labels
      use m_wide_SGS_term_labels
      use m_grad_filter_field_labels
      use m_diff_filter_vect_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(wide_SGS_heat_flux%name,                  &
     &                        field_ctl)) then
        call add_phys_name_ctl(filter_velocity%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_1%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_2%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_3%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_temp%name, field_ctl)
      end if
      if(check_field_list_ctl(wide_SGS_composit_flux%name,              &
     &                        field_ctl)) then
        call add_phys_name_ctl(filter_velocity%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_1%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_2%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_3%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_comp%name, field_ctl)
      end if
!
      if(check_field_list_ctl(wide_SGS_inertia%name, field_ctl)) then
        call add_phys_name_ctl(filter_velocity%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_w_1%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_w_2%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_w_3%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_1%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_2%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_3%name, field_ctl)
      end if
      if(check_field_list_ctl(wide_SGS_Lorentz%name, field_ctl)) then
        call add_phys_name_ctl(filter_magne%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_j_1%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_j_2%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_j_3%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_b_1%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_b_2%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_b_3%name, field_ctl)
      end if
!
      if(check_field_list_ctl(wide_SGS_vp_induction%name,               &
     &                        field_ctl)) then
        call add_phys_name_ctl(filter_magne%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_1%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_2%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_3%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_b_1%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_b_2%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_b_3%name, field_ctl)
      end if
!
      end subroutine add_field_ctl_4_grad_wide_SGS
!
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_simi_wide_SGS(field_ctl)
!
      use t_control_array_character3
      use t_SGS_term_labels
      use t_base_force_labels
      use m_wide_SGS_term_labels
      use m_wide_filter_field_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(wide_SGS_heat_flux%name,                  &
     &                        field_ctl)) then
        call add_phys_name_ctl(heat_flux%name, field_ctl)
        call add_phys_name_ctl(wide_filter_velocity%name, field_ctl)
        call add_phys_name_ctl(wide_filter_temp%name, field_ctl)
      end if
      if(check_field_list_ctl(wide_SGS_composit_flux%name,              &
     &                        field_ctl)) then
        call add_phys_name_ctl(composite_flux%name, field_ctl)
        call add_phys_name_ctl(wide_filter_velocity%name, field_ctl)
        call add_phys_name_ctl(wide_filter_composition%name, field_ctl)
      end if
!
      if(check_field_list_ctl(wide_SGS_inertia%name, field_ctl)) then
        call add_phys_name_ctl(inertia%name, field_ctl)
        call add_phys_name_ctl(wide_filter_vorticity%name, field_ctl)
        call add_phys_name_ctl(wide_filter_velocity%name, field_ctl)
      end if
      if(check_field_list_ctl(wide_SGS_Lorentz%name, field_ctl)) then
        call add_phys_name_ctl(Lorentz_force%name, field_ctl)
        call add_phys_name_ctl(wide_filter_current%name, field_ctl)
        call add_phys_name_ctl(wide_filter_magne%name, field_ctl)
      end if
!
      if(check_field_list_ctl(wide_SGS_vp_induction%name,               &
     &                        field_ctl)) then
        call add_phys_name_ctl(vecp_induction%name, field_ctl)
        call add_phys_name_ctl(wide_filter_velocity%name, field_ctl)
        call add_phys_name_ctl(wide_filter_magne%name, field_ctl)
      end if
!
      end subroutine add_field_ctl_4_simi_wide_SGS
!
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_dble_SGS_terms(field_ctl)
!
      use t_control_array_character3
      use t_SGS_term_labels
      use m_wide_SGS_term_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(double_SGS_heat_flux%name,                &
     &                        field_ctl))                               &
     &   call add_phys_name_ctl(SGS_heat_flux%name, field_ctl)
      if(check_field_list_ctl(double_SGS_composit_flux%name,            &
     &                        field_ctl))                               &
     &   call add_phys_name_ctl(SGS_composit_flux%name, field_ctl)
!
      if(check_field_list_ctl(double_SGS_inertia%name, field_ctl))      &
     &   call add_phys_name_ctl(SGS_inertia%name, field_ctl)
      if(check_field_list_ctl(double_SGS_Lorentz%name, field_ctl))      &
     &   call add_phys_name_ctl(SGS_Lorentz%name, field_ctl)
!
      if(check_field_list_ctl(double_SGS_vp_induction%name, field_ctl)) &
     &   call add_phys_name_ctl(SGS_vecp_induction%name, field_ctl)
!
      end subroutine add_field_ctl_4_dble_SGS_terms
!
! -----------------------------------------------------------------------
!
      end module check_wide_SGS_terms

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
      if(check_field_list_ctl(wide_SGS_heat_flux, field_ctl)) then
        call add_phys_name_ctl(filter_velocity, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_1, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_2, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_3, field_ctl)
        call add_phys_name_ctl(grad_filtered_temp, field_ctl)
      end if
      if(check_field_list_ctl(wide_SGS_composit_flux, field_ctl)) then
        call add_phys_name_ctl(filter_velocity, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_1, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_2, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_3, field_ctl)
        call add_phys_name_ctl(grad_filtered_comp, field_ctl)
      end if
!
      if(check_field_list_ctl(wide_SGS_inertia, field_ctl)) then
        call add_phys_name_ctl(filter_velocity, field_ctl)
        call add_phys_name_ctl(grad_filtered_w_1, field_ctl)
        call add_phys_name_ctl(grad_filtered_w_2, field_ctl)
        call add_phys_name_ctl(grad_filtered_w_3, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_1, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_2, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_3, field_ctl)
      end if
      if(check_field_list_ctl(wide_SGS_Lorentz, field_ctl)) then
        call add_phys_name_ctl(filter_magne, field_ctl)
        call add_phys_name_ctl(grad_filtered_j_1, field_ctl)
        call add_phys_name_ctl(grad_filtered_j_2, field_ctl)
        call add_phys_name_ctl(grad_filtered_j_3, field_ctl)
        call add_phys_name_ctl(grad_filtered_b_1, field_ctl)
        call add_phys_name_ctl(grad_filtered_b_2, field_ctl)
        call add_phys_name_ctl(grad_filtered_b_3, field_ctl)
      end if
!
      if(check_field_list_ctl(wide_SGS_vp_induction, field_ctl)) then
        call add_phys_name_ctl(filter_magne, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_1, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_2, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_3, field_ctl)
        call add_phys_name_ctl(grad_filtered_b_1, field_ctl)
        call add_phys_name_ctl(grad_filtered_b_2, field_ctl)
        call add_phys_name_ctl(grad_filtered_b_3, field_ctl)
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
      use m_base_force_labels
      use m_wide_SGS_term_labels
      use m_wide_filter_field_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(wide_SGS_heat_flux, field_ctl)) then
        call add_phys_name_ctl(heat_flux, field_ctl)
        call add_phys_name_ctl(wide_filter_velocity, field_ctl)
        call add_phys_name_ctl(wide_filter_temp, field_ctl)
      end if
      if(check_field_list_ctl(wide_SGS_composit_flux, field_ctl)) then
        call add_phys_name_ctl(composite_flux, field_ctl)
        call add_phys_name_ctl(wide_filter_velocity, field_ctl)
        call add_phys_name_ctl(wide_filter_composition, field_ctl)
      end if
!
      if(check_field_list_ctl(wide_SGS_inertia, field_ctl)) then
        call add_phys_name_ctl(inertia, field_ctl)
        call add_phys_name_ctl(wide_filter_vorticity, field_ctl)
        call add_phys_name_ctl(wide_filter_velocity, field_ctl)
      end if
      if(check_field_list_ctl(wide_SGS_Lorentz, field_ctl)) then
        call add_phys_name_ctl(Lorentz_force, field_ctl)
        call add_phys_name_ctl(wide_filter_current, field_ctl)
        call add_phys_name_ctl(wide_filter_magne, field_ctl)
      end if
!
      if(check_field_list_ctl(wide_SGS_vp_induction, field_ctl)) then
        call add_phys_name_ctl(vecp_induction, field_ctl)
        call add_phys_name_ctl(wide_filter_velocity, field_ctl)
        call add_phys_name_ctl(wide_filter_magne, field_ctl)
      end if
!
      end subroutine add_field_ctl_4_simi_wide_SGS
!
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_dble_SGS_terms(field_ctl)
!
      use t_control_array_character3
      use m_SGS_term_labels
      use m_wide_SGS_term_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(double_SGS_heat_flux, field_ctl))         &
     &   call add_phys_name_ctl(SGS_heat_flux, field_ctl)
      if(check_field_list_ctl(double_SGS_composit_flux, field_ctl))     &
     &   call add_phys_name_ctl(SGS_composit_flux, field_ctl)
!
      if(check_field_list_ctl(double_SGS_inertia, field_ctl))           &
     &   call add_phys_name_ctl(SGS_inertia, field_ctl)
      if(check_field_list_ctl(double_SGS_Lorentz, field_ctl))           &
     &   call add_phys_name_ctl(SGS_Lorentz, field_ctl)
!
      if(check_field_list_ctl(double_SGS_vp_induction, field_ctl))      &
     &   call add_phys_name_ctl(SGS_vecp_induction, field_ctl)
!
      end subroutine add_field_ctl_4_dble_SGS_terms
!
! -----------------------------------------------------------------------
!
      end module check_wide_SGS_terms

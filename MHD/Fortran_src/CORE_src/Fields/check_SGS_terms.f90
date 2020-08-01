!>@file   check_SGS_terms.f90
!!        module check_SGS_terms
!!
!! @author H. Matsui
!! @date   Programmed in Jan., 2020
!!
!!
!> @brief Labels and addresses for SGS terms
!!
!!@verbatim
!!      subroutine add_field_ctl_4_SGS_terms(field_ctl)
!!      subroutine add_field_ctl_4_diff_SGS_terms(field_ctl)
!!      subroutine add_field_ctl_4_SGS_ene_fluxes(field_ctl)
!!      subroutine add_field_ctl_4_model_coefs(field_ctl)
!!      subroutine add_field_ctl_4_force_w_SGS(field_ctl)
!!      subroutine add_field_ctl_4_true_SGS(field_ctl)
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!@endverbatim
!!
      module check_SGS_terms
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
      subroutine add_field_ctl_4_SGS_terms(field_ctl)
!
      use t_control_array_character3

!
      use m_SGS_term_labels
      use m_base_field_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(SGS_induction, field_ctl))                &
     &   call add_phys_name_ctl(SGS_vecp_induction, field_ctl)
!
      if(check_field_list_ctl(SGS_buoyancy, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(SGS_heat_flux, field_ctl)
      end if
      if(check_field_list_ctl(SGS_composit_buoyancy, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(SGS_composit_flux, field_ctl)
      end if
!
!
      if(check_field_list_ctl(SGS_inertia, field_ctl)) then
        call add_phys_name_ctl(vorticity, field_ctl)
        call add_phys_name_ctl(velocity, field_ctl)
      end if
      if(check_field_list_ctl(SGS_Lorentz, field_ctl)) then
        call add_phys_name_ctl(current_density, field_ctl)
        call add_phys_name_ctl(magnetic_field, field_ctl)
      end if
!
      if(check_field_list_ctl(SGS_momentum_flux, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
      end if
      if(check_field_list_ctl(SGS_maxwell_tensor, field_ctl)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
      end if
!
      if(     check_field_list_ctl(SGS_vecp_induction, field_ctl)       &
     &   .or. check_field_list_ctl(SGS_induct_tensor, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(magnetic_field, field_ctl)
      end if
!
      if(check_field_list_ctl(SGS_heat_flux, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(temperature, field_ctl)
      end if
      if(check_field_list_ctl(SGS_composit_flux, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(composition, field_ctl)
      end if
!
      end subroutine add_field_ctl_4_SGS_terms
!
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_diff_SGS_terms(field_ctl)
!
      use t_control_array_character3
      use m_SGS_term_labels
      use m_diff_SGS_term_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(div_SGS_m_flux, field_ctl))               &
     &   call add_phys_name_ctl(SGS_momentum_flux, field_ctl)
      if(check_field_list_ctl(div_SGS_h_flux, field_ctl))               &
     &   call add_phys_name_ctl(SGS_heat_flux, field_ctl)
      if(check_field_list_ctl(div_SGS_c_flux, field_ctl))               &
     &   call add_phys_name_ctl(SGS_composit_flux, field_ctl)
!
      if(   check_field_list_ctl(div_SGS_inertia, field_ctl)            &
     & .or. check_field_list_ctl(rot_SGS_inertia, field_ctl)) then
        call add_phys_name_ctl(SGS_inertia, field_ctl)
      end if
      if(   check_field_list_ctl(div_SGS_Lorentz, field_ctl)            &
     & .or. check_field_list_ctl(rot_SGS_Lorentz, field_ctl)) then
        call add_phys_name_ctl(SGS_Lorentz, field_ctl)
      end if
!
      end subroutine add_field_ctl_4_diff_SGS_terms
!
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_SGS_ene_fluxes(field_ctl)
!
      use t_control_array_character3
!
      use m_SGS_enegy_flux_labels
      use m_base_field_labels
      use m_SGS_term_labels
      use m_diff_SGS_term_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(Reynolds_work, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(SGS_inertia, field_ctl)
      end if
      if(check_field_list_ctl(SGS_Lorentz_work, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(SGS_Lorentz, field_ctl)
      end if
      if(check_field_list_ctl(SGS_buoyancy_flux, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(SGS_buoyancy, field_ctl)
      end if
      if(check_field_list_ctl(SGS_comp_buoyancy_flux, field_ctl)) then
        call add_phys_name_ctl(velocity, field_ctl)
        call add_phys_name_ctl(SGS_composit_buoyancy, field_ctl)
      end if
!
      if(check_field_list_ctl(SGS_mag_induction_flux, field_ctl)) then
        call add_phys_name_ctl(magnetic_field, field_ctl)
        call add_phys_name_ctl(SGS_induction, field_ctl)
      end if
!
      if(check_field_list_ctl(SGS_temp_flux_gen, field_ctl)) then
        call add_phys_name_ctl(temperature, field_ctl)
        call add_phys_name_ctl(SGS_heat_flux, field_ctl)
      end if
      if(check_field_list_ctl(SGS_comp_flux_gen, field_ctl)) then
        call add_phys_name_ctl(composition, field_ctl)
        call add_phys_name_ctl(SGS_composit_flux, field_ctl)
      end if
!
      end subroutine add_field_ctl_4_SGS_ene_fluxes
!
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_model_coefs(field_ctl)
!
      use t_control_array_character3
!
      use m_SGS_model_coef_labels
      use m_base_field_labels
      use m_SGS_term_labels
      use m_wide_SGS_term_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(Csim_SGS_heat_flux, field_ctl)) then
        call add_phys_name_ctl(SGS_heat_flux, field_ctl)
        call add_phys_name_ctl(wide_SGS_heat_flux, field_ctl)
      end if
      if(check_field_list_ctl(Csim_SGS_composit_flux, field_ctl)) then
        call add_phys_name_ctl(SGS_composit_flux, field_ctl)
        call add_phys_name_ctl(wide_SGS_composit_flux, field_ctl)
      end if
!
!
      if(check_field_list_ctl(Csim_SGS_inertia, field_ctl)) then
        call add_phys_name_ctl(SGS_inertia, field_ctl)
        call add_phys_name_ctl(wide_SGS_inertia, field_ctl)
      end if
      if(check_field_list_ctl(Csim_SGS_Lorentz, field_ctl)) then
        call add_phys_name_ctl(SGS_Lorentz, field_ctl)
        call add_phys_name_ctl(wide_SGS_Lorentz, field_ctl)
      end if
!
      if(check_field_list_ctl(Csim_SGS_induction, field_ctl)) then
        call add_phys_name_ctl(SGS_vecp_induction, field_ctl)
        call add_phys_name_ctl(wide_SGS_vp_induction, field_ctl)
      end if
!
      if(check_field_list_ctl(Csim_SGS_buoyancy, field_ctl)) then
        call add_phys_name_ctl(SGS_heat_flux, field_ctl)
        call add_phys_name_ctl(velocity, field_ctl)
      end if
      if(check_field_list_ctl(Csim_SGS_composit_buo, field_ctl)) then
        call add_phys_name_ctl(SGS_composit_flux, field_ctl)
        call add_phys_name_ctl(velocity, field_ctl)
      end if
!
      end subroutine add_field_ctl_4_model_coefs
!
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_force_w_SGS(field_ctl)
!
      use t_control_array_character3
!
      use m_base_field_labels
      use m_base_force_labels
      use m_SGS_term_labels
      use m_force_w_SGS_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(induction_w_SGS, field_ctl)) then
         call add_phys_name_ctl(magnetic_induction, field_ctl)
         call add_phys_name_ctl(SGS_induction, field_ctl)
      end if
!
!
      if(check_field_list_ctl(intertia_w_SGS, field_ctl)) then
        call add_phys_name_ctl(inertia, field_ctl)
        call add_phys_name_ctl(SGS_inertia, field_ctl)
      end if
      if(check_field_list_ctl(Lorentz_w_SGS, field_ctl)) then
        call add_phys_name_ctl(Lorentz_force, field_ctl)
        call add_phys_name_ctl(SGS_Lorentz, field_ctl)
      end if
!
      if(check_field_list_ctl(momentum_flux_w_SGS, field_ctl)) then
        call add_phys_name_ctl(momentum_flux, field_ctl)
        call add_phys_name_ctl(SGS_momentum_flux, field_ctl)
      end if
      if(check_field_list_ctl(maxwell_tensor_w_SGS, field_ctl)) then
        call add_phys_name_ctl(maxwell_tensor, field_ctl)
        call add_phys_name_ctl(SGS_maxwell_tensor, field_ctl)
      end if
      if(check_field_list_ctl(induction_tensor_w_SGS, field_ctl)) then
        call add_phys_name_ctl(induction_tensor, field_ctl)
        call add_phys_name_ctl(SGS_induct_tensor, field_ctl)
      end if
!
      if(check_field_list_ctl(vecp_induction_w_SGS, field_ctl)) then
        call add_phys_name_ctl(vecp_induction, field_ctl)
        call add_phys_name_ctl(SGS_vecp_induction, field_ctl)
      end if
!
      if(check_field_list_ctl(heat_flux_w_SGS, field_ctl)) then
        call add_phys_name_ctl(heat_flux, field_ctl)
        call add_phys_name_ctl(SGS_heat_flux, field_ctl)
      end if
      if(check_field_list_ctl(compostion_flux_w_SGS, field_ctl)) then
        call add_phys_name_ctl(composite_flux, field_ctl)
        call add_phys_name_ctl(SGS_composit_flux, field_ctl)
      end if
!
      end subroutine add_field_ctl_4_force_w_SGS
!
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_true_SGS(field_ctl)
!
      use t_control_array_character3
      use t_SGS_enegy_flux_labels
      use m_div_force_labels
      use m_SGS_term_labels
      use m_true_SGS_term_labels
      use m_diff_SGS_term_labels
      use m_filtered_field_labels
      use m_div_force_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(Reynolds_work_true, field_ctl)) then
        call add_phys_name_ctl(filter_velocity, field_ctl)
        call add_phys_name_ctl(SGS_div_m_flux_true, field_ctl)
      end if
      if(check_field_list_ctl(SGS_Lorentz_work_true, field_ctl)) then
        call add_phys_name_ctl(filter_velocity, field_ctl)
        call add_phys_name_ctl(SGS_Lorentz_true, field_ctl)
      end if
      if(check_field_list_ctl(SGS_mag_induction_flux_true,              &
     &                        field_ctl)) then
        call add_phys_name_ctl(filter_magne, field_ctl)
        call add_phys_name_ctl(SGS_mag_induction_true, field_ctl)
      end if
!
      if(check_field_list_ctl(SGS_temp_flux_gen_true, field_ctl)) then
        call add_phys_name_ctl(filter_temperature, field_ctl)
        call add_phys_name_ctl(SGS_div_h_flux_true, field_ctl)
      end if
      if(check_field_list_ctl(SGS_comp_flux_gen_true, field_ctl)) then
        call add_phys_name_ctl(filter_composition, field_ctl)
        call add_phys_name_ctl(SGS_div_c_flux_true, field_ctl)
      end if
!
!
      if(check_field_list_ctl(SGS_div_m_flux_true, field_ctl)) then
        call add_phys_name_ctl(filter_velocity, field_ctl)
        call add_phys_name_ctl(div_momentum_flux, field_ctl)
      end if
      if(check_field_list_ctl(SGS_Lorentz_true, field_ctl)) then
        call add_phys_name_ctl(filter_magne, field_ctl)
        call add_phys_name_ctl(div_maxwell_tensor, field_ctl)
      end if
      if(check_field_list_ctl(SGS_mag_induction_true, field_ctl)) then
        call add_phys_name_ctl(filter_velocity, field_ctl)
        call add_phys_name_ctl(filter_magne, field_ctl)
        call add_phys_name_ctl(div_induction_tensor, field_ctl)
      end if
!
      if(check_field_list_ctl(SGS_div_h_flux_true, field_ctl)) then
        call add_phys_name_ctl(filter_velocity, field_ctl)
        call add_phys_name_ctl(filter_temperature, field_ctl)
        call add_phys_name_ctl(div_heat_flux, field_ctl)
        end if
      if(check_field_list_ctl(SGS_div_c_flux_true, field_ctl)) then
        call add_phys_name_ctl(filter_velocity, field_ctl)
        call add_phys_name_ctl(filter_composition, field_ctl)
        call add_phys_name_ctl(div_composition_flux, field_ctl)
      end if
!
      end subroutine add_field_ctl_4_true_SGS
!
! -----------------------------------------------------------------------
!
      end module check_SGS_terms

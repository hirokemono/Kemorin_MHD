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
      use t_SGS_term_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(SGS_induction%name, field_ctl))           &
     &   call add_phys_name_ctl(SGS_vecp_induction%name, field_ctl)
!
      if(check_field_list_ctl(SGS_buoyancy%name, field_ctl)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(SGS_heat_flux%name, field_ctl)
      end if
      if(check_field_list_ctl(SGS_composit_buoyancy%name,               &
     &                        field_ctl)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(SGS_composit_flux%name, field_ctl)
      end if
!
!
      if(check_field_list_ctl(SGS_inertia%name, field_ctl)) then
        call add_phys_name_ctl(fhd_vort, field_ctl)
        call add_phys_name_ctl(fhd_velo, field_ctl)
      end if
      if(check_field_list_ctl(SGS_Lorentz%name, field_ctl)) then
        call add_phys_name_ctl(fhd_current, field_ctl)
        call add_phys_name_ctl(fhd_magne, field_ctl)
      end if
!
      if(check_field_list_ctl(SGS_momentum_flux%name, field_ctl)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
      end if
      if(check_field_list_ctl(SGS_maxwell_tensor%name, field_ctl)) then
        call add_phys_name_ctl(fhd_magne, field_ctl)
      end if
!
      if(     check_field_list_ctl(SGS_vecp_induction%name, field_ctl)  &
     &   .or. check_field_list_ctl(SGS_induct_tensor%name,              &
     &                             field_ctl)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_magne, field_ctl)
      end if
!
      if(check_field_list_ctl(SGS_heat_flux%name, field_ctl)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_temp, field_ctl)
      end if
      if(check_field_list_ctl(SGS_composit_flux%name, field_ctl)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(fhd_light, field_ctl)
      end if
!
      end subroutine add_field_ctl_4_SGS_terms
!
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_diff_SGS_terms(field_ctl)
!
      use t_control_array_character3
      use t_SGS_term_labels
      use m_diff_SGS_term_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(div_SGS_m_flux%name, field_ctl))          &
     &   call add_phys_name_ctl(SGS_momentum_flux%name, field_ctl)
      if(check_field_list_ctl(div_SGS_h_flux%name, field_ctl))          &
     &   call add_phys_name_ctl(SGS_heat_flux%name, field_ctl)
      if(check_field_list_ctl(div_SGS_c_flux%name, field_ctl))          &
     &   call add_phys_name_ctl(SGS_composit_flux%name, field_ctl)
!
      if(   check_field_list_ctl(div_SGS_inertia%name, field_ctl)       &
     & .or. check_field_list_ctl(rot_SGS_inertia%name, field_ctl)) then
        call add_phys_name_ctl(SGS_inertia%name, field_ctl)
      end if
      if(   check_field_list_ctl(div_SGS_Lorentz%name, field_ctl)       &
     & .or. check_field_list_ctl(rot_SGS_Lorentz%name, field_ctl)) then
        call add_phys_name_ctl(SGS_Lorentz%name, field_ctl)
      end if
!
      end subroutine add_field_ctl_4_diff_SGS_terms
!
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_SGS_ene_fluxes(field_ctl)
!
      use t_control_array_character3
      use t_SGS_term_labels
      use t_SGS_enegy_flux_labels
      use m_diff_SGS_term_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(Reynolds_work%name, field_ctl)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(SGS_inertia%name, field_ctl)
      end if
      if(check_field_list_ctl(SGS_Lorentz_work%name, field_ctl)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(SGS_Lorentz%name, field_ctl)
      end if
      if(check_field_list_ctl(SGS_buoyancy_flux%name, field_ctl)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(SGS_buoyancy%name, field_ctl)
      end if
      if(check_field_list_ctl(SGS_comp_buoyancy_flux%name,              &
     &                        field_ctl)) then
        call add_phys_name_ctl(fhd_velo, field_ctl)
        call add_phys_name_ctl(SGS_composit_buoyancy%name, field_ctl)
      end if
!
      if(check_field_list_ctl(SGS_mag_induction_flux%name,              &
     &                        field_ctl)) then
        call add_phys_name_ctl(fhd_magne, field_ctl)
        call add_phys_name_ctl(SGS_induction%name, field_ctl)
      end if
!
      if(check_field_list_ctl(SGS_temp_flux_gen%name, field_ctl)) then
        call add_phys_name_ctl(fhd_temp, field_ctl)
        call add_phys_name_ctl(SGS_heat_flux%name, field_ctl)
      end if
      if(check_field_list_ctl(SGS_comp_flux_gen%name, field_ctl)) then
        call add_phys_name_ctl(fhd_light, field_ctl)
        call add_phys_name_ctl(SGS_composit_flux%name, field_ctl)
      end if
!
      end subroutine add_field_ctl_4_SGS_ene_fluxes
!
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_model_coefs(field_ctl)
!
      use t_control_array_character3
      use t_SGS_model_coef_labels
      use t_SGS_term_labels
      use m_wide_SGS_term_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(Csim_SGS_heat_flux%name, field_ctl)) then
        call add_phys_name_ctl(SGS_heat_flux%name, field_ctl)
        call add_phys_name_ctl(wide_SGS_heat_flux%name, field_ctl)
      end if
      if(check_field_list_ctl(Csim_SGS_composit_flux%name,              &
     &                        field_ctl)) then
        call add_phys_name_ctl(SGS_composit_flux%name, field_ctl)
        call add_phys_name_ctl(wide_SGS_composit_flux%name, field_ctl)
      end if
!
!
      if(check_field_list_ctl(Csim_SGS_inertia%name, field_ctl)) then
        call add_phys_name_ctl(SGS_inertia%name, field_ctl)
        call add_phys_name_ctl(wide_SGS_inertia%name, field_ctl)
      end if
      if(check_field_list_ctl(Csim_SGS_Lorentz%name, field_ctl)) then
        call add_phys_name_ctl(SGS_Lorentz%name, field_ctl)
        call add_phys_name_ctl(wide_SGS_Lorentz%name, field_ctl)
      end if
!
      if(check_field_list_ctl(Csim_SGS_induction%name, field_ctl)) then
        call add_phys_name_ctl(SGS_vecp_induction%name, field_ctl)
        call add_phys_name_ctl(wide_SGS_vp_induction%name, field_ctl)
      end if
!
      if(check_field_list_ctl(Csim_SGS_buoyancy%name, field_ctl)) then
        call add_phys_name_ctl(SGS_heat_flux%name, field_ctl)
        call add_phys_name_ctl(fhd_velo, field_ctl)
      end if
      if(check_field_list_ctl(Csim_SGS_composit_buo%name,               &
     &                        field_ctl)) then
        call add_phys_name_ctl(SGS_composit_flux%name, field_ctl)
        call add_phys_name_ctl(fhd_velo, field_ctl)
      end if
!
      end subroutine add_field_ctl_4_model_coefs
!
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_force_w_SGS(field_ctl)
!
      use t_control_array_character3
      use t_SGS_term_labels
      use t_base_force_labels
      use m_force_w_SGS_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(induction_w_SGS%name, field_ctl)) then
         call add_phys_name_ctl(magnetic_induction%name, field_ctl)
         call add_phys_name_ctl(SGS_induction%name, field_ctl)
      end if
!
!
      if(check_field_list_ctl(intertia_w_SGS%name, field_ctl)) then
        call add_phys_name_ctl(inertia%name, field_ctl)
        call add_phys_name_ctl(SGS_inertia%name, field_ctl)
      end if
      if(check_field_list_ctl(Lorentz_w_SGS%name, field_ctl)) then
        call add_phys_name_ctl(Lorentz_force%name, field_ctl)
        call add_phys_name_ctl(SGS_Lorentz%name, field_ctl)
      end if
!
      if(check_field_list_ctl(momentum_flux_w_SGS%name,                 &
     &                        field_ctl)) then
        call add_phys_name_ctl(momentum_flux%name, field_ctl)
        call add_phys_name_ctl(SGS_momentum_flux%name, field_ctl)
      end if
      if(check_field_list_ctl(maxwell_tensor_w_SGS%name,                &
     &                        field_ctl)) then
        call add_phys_name_ctl(maxwell_tensor%name, field_ctl)
        call add_phys_name_ctl(SGS_maxwell_tensor%name, field_ctl)
      end if
      if(     check_field_list_ctl(induction_tensor_w_SGS%name,         &
     &                             field_ctl)) then
        call add_phys_name_ctl(induction_tensor%name, field_ctl)
        call add_phys_name_ctl(SGS_induct_tensor%name, field_ctl)
      end if
!
      if(check_field_list_ctl(vecp_induction_w_SGS%name,                &
     &                        field_ctl)) then
        call add_phys_name_ctl(vecp_induction%name, field_ctl)
        call add_phys_name_ctl(SGS_vecp_induction%name, field_ctl)
      end if
!
      if(check_field_list_ctl(heat_flux_w_SGS%name, field_ctl)) then
        call add_phys_name_ctl(heat_flux%name, field_ctl)
        call add_phys_name_ctl(SGS_heat_flux%name, field_ctl)
      end if
      if(check_field_list_ctl(compostion_flux_w_SGS%name,               &
     &                        field_ctl)) then
        call add_phys_name_ctl(composite_flux%name, field_ctl)
        call add_phys_name_ctl(SGS_composit_flux%name, field_ctl)
      end if
!
      end subroutine add_field_ctl_4_force_w_SGS
!
! -----------------------------------------------------------------------
!
      subroutine add_field_ctl_4_true_SGS(field_ctl)
!
      use t_control_array_character3
      use t_SGS_term_labels
      use t_SGS_enegy_flux_labels
      use m_true_SGS_term_labels
      use m_diff_SGS_term_labels
      use m_filtered_field_labels
      use m_diff_force_labels
      use add_nodal_fields_ctl
!
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(check_field_list_ctl(Reynolds_work_true%name, field_ctl)) then
        call add_phys_name_ctl(fhd_filter_velo, field_ctl)
        call add_phys_name_ctl(SGS_div_m_flux_true%name, field_ctl)
      end if
      if(check_field_list_ctl(SGS_Lorentz_work_true%name,               &
     &                        field_ctl)) then
        call add_phys_name_ctl(fhd_filter_velo, field_ctl)
        call add_phys_name_ctl(SGS_Lorentz_true%name, field_ctl)
      end if
      if(check_field_list_ctl(SGS_mag_induction_flux_true%name,         &
     &                        field_ctl)) then
        call add_phys_name_ctl(fhd_filter_magne, field_ctl)
        call add_phys_name_ctl(SGS_mag_induction_true%name, field_ctl)
      end if
!
      if(check_field_list_ctl(SGS_temp_flux_gen_true%name,              &
     &                        field_ctl)) then
        call add_phys_name_ctl(fhd_filter_temp, field_ctl)
        call add_phys_name_ctl(SGS_div_h_flux_true%name, field_ctl)
      end if
      if(check_field_list_ctl(SGS_comp_flux_gen_true%name,              &
     &                        field_ctl)) then
        call add_phys_name_ctl(fhd_filter_comp, field_ctl)
        call add_phys_name_ctl(SGS_div_c_flux_true%name, field_ctl)
      end if
!
!
      if(check_field_list_ctl(SGS_div_m_flux_true%name, field_ctl)) then
        call add_phys_name_ctl(fhd_filter_velo, field_ctl)
        call add_phys_name_ctl(fhd_div_m_flux, field_ctl)
      end if
      if(check_field_list_ctl(SGS_Lorentz_true%name,               &
     &                        field_ctl)) then
        call add_phys_name_ctl(fhd_filter_magne, field_ctl)
        call add_phys_name_ctl(fhd_div_maxwell_t, field_ctl)
      end if
      if(check_field_list_ctl(SGS_mag_induction_true%name,         &
     &                        field_ctl)) then
        call add_phys_name_ctl(fhd_filter_velo, field_ctl)
        call add_phys_name_ctl(fhd_filter_magne, field_ctl)
        call add_phys_name_ctl(fhd_div_induct_t, field_ctl)
      end if
!
      if(check_field_list_ctl(SGS_div_h_flux_true%name,              &
     &                        field_ctl)) then
        call add_phys_name_ctl(fhd_filter_velo, field_ctl)
        call add_phys_name_ctl(fhd_filter_temp, field_ctl)
        call add_phys_name_ctl(fhd_div_h_flux, field_ctl)
        end if
      if(check_field_list_ctl(SGS_div_c_flux_true%name,              &
     &                        field_ctl)) then
        call add_phys_name_ctl(fhd_filter_velo, field_ctl)
        call add_phys_name_ctl(fhd_filter_comp, field_ctl)
        call add_phys_name_ctl(fhd_div_c_flux, field_ctl)
      end if
!
      end subroutine add_field_ctl_4_true_SGS
!
! -----------------------------------------------------------------------
!
      end module check_SGS_terms

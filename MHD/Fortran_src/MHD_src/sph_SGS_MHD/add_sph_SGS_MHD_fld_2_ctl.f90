!>@file   add_sph_SGS_MHD_fld_2_ctl.f90
!!@brief  module add_sph_SGS_MHD_fld_2_ctl
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Sep., 2009
!
!>@brief Add fields in control list for MHD dynamo simulation
!!
!!@verbatim
!!      subroutine add_field_name_4_SGS(SGS_param, field_ctl)
!!      subroutine add_field_name_dynamic_SGS                           &
!!     &         (SGS_param, fl_prop, field_ctl)
!!        type(SGS_model_control_params), intent(in) :: SGS_param
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(ctl_array_c3), intent(inout) :: field_ctl
!!@endverbatim
!
      module add_sph_SGS_MHD_fld_2_ctl
!
      use m_precision
!
      use m_phys_labels
      use t_SGS_control_parameter
      use t_control_array_character3
      use t_physical_property
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine add_field_name_4_SGS(SGS_param, field_ctl)
!
      use t_SGS_term_labels
      use m_diff_SGS_term_labels
      use m_filtered_field_labels
      use add_nodal_fields_ctl
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!   Add SGS terms
!
      if(SGS_param%iflag_SGS_m_flux .gt. id_SGS_none) then
        call add_phys_name_ctl(SGS_inertia%name, field_ctl)
        call add_phys_name_ctl(rot_SGS_inertia%name, field_ctl)
        call add_phys_name_ctl(div_SGS_inertia%name, field_ctl)
      end if
!
      if(SGS_param%iflag_SGS_lorentz .gt. id_SGS_none) then
        call add_phys_name_ctl(SGS_Lorentz%name, field_ctl)
        call add_phys_name_ctl(rot_SGS_Lorentz%name, field_ctl)
        call add_phys_name_ctl(div_SGS_Lorentz%name, field_ctl)
      end if
!
      if(SGS_param%iflag_SGS_uxb .gt. id_SGS_none) then
        call add_phys_name_ctl(SGS_vecp_induction%name, field_ctl)
        call add_phys_name_ctl(SGS_induction%name, field_ctl)
      end if
!
      if(SGS_param%iflag_SGS_h_flux .gt. id_SGS_none) then
        call add_phys_name_ctl(SGS_heat_flux%name, field_ctl)
        call add_phys_name_ctl(div_SGS_h_flux%name, field_ctl)
      end if
!
      if(SGS_param%iflag_SGS_c_flux .gt. id_SGS_none) then
        call add_phys_name_ctl(SGS_composit_flux%name, field_ctl)
        call add_phys_name_ctl(div_SGS_c_flux%name, field_ctl)
      end if
!
!   Add fieltered field
!
      if(SGS_param%iflag_SGS_m_flux .eq. id_SGS_similarity) then
        call add_phys_name_ctl(filter_velocity%name, field_ctl)
        call add_phys_name_ctl(filter_vorticity%name, field_ctl)
      else if(SGS_param%iflag_SGS_m_flux .eq. id_SGS_NL_grad) then
        call add_phys_name_ctl(grad_v_1%name, field_ctl)
        call add_phys_name_ctl(grad_v_2%name, field_ctl)
        call add_phys_name_ctl(grad_v_3%name, field_ctl)
        call add_phys_name_ctl(grad_w_1%name, field_ctl)
        call add_phys_name_ctl(grad_w_2%name, field_ctl)
        call add_phys_name_ctl(grad_w_3%name, field_ctl)
      end if
!
      if(SGS_param%iflag_SGS_lorentz .eq. id_SGS_similarity) then
        call add_phys_name_ctl(filter_magne%name, field_ctl)
        call add_phys_name_ctl(filter_current%name, field_ctl)
      else if(SGS_param%iflag_SGS_lorentz .eq. id_SGS_NL_grad) then
        call add_phys_name_ctl(grad_j_1%name, field_ctl)
        call add_phys_name_ctl(grad_j_2%name, field_ctl)
        call add_phys_name_ctl(grad_j_3%name, field_ctl)
        call add_phys_name_ctl(grad_b_1%name, field_ctl)
        call add_phys_name_ctl(grad_b_2%name, field_ctl)
        call add_phys_name_ctl(grad_b_3%name, field_ctl)
      end if
!
      if(SGS_param%iflag_SGS_uxb .eq. id_SGS_similarity) then
        call add_phys_name_ctl(filter_velocity%name, field_ctl)
        call add_phys_name_ctl(filter_magne%name, field_ctl)
      else if(SGS_param%iflag_SGS_uxb .eq. id_SGS_NL_grad) then
        call add_phys_name_ctl(grad_v_1%name, field_ctl)
        call add_phys_name_ctl(grad_v_2%name, field_ctl)
        call add_phys_name_ctl(grad_v_3%name, field_ctl)
        call add_phys_name_ctl(grad_b_1%name, field_ctl)
        call add_phys_name_ctl(grad_b_2%name, field_ctl)
        call add_phys_name_ctl(grad_b_3%name, field_ctl)
      end if
!
      if(SGS_param%iflag_SGS_h_flux .eq. id_SGS_similarity) then
        call add_phys_name_ctl(filter_velocity%name, field_ctl)
        call add_phys_name_ctl(filter_temperature%name, field_ctl)
      else if(SGS_param%iflag_SGS_h_flux .eq. id_SGS_NL_grad) then
        call add_phys_name_ctl(grad_v_1%name, field_ctl)
        call add_phys_name_ctl(grad_v_2%name, field_ctl)
        call add_phys_name_ctl(grad_v_3%name, field_ctl)
        call add_phys_name_ctl(grad_temp%name, field_ctl)
      end if
!
      if(SGS_param%iflag_SGS_c_flux .eq. id_SGS_similarity) then
        call add_phys_name_ctl(filter_velocity%name, field_ctl)
        call add_phys_name_ctl(filter_composition%name, field_ctl)
      else if(SGS_param%iflag_SGS_c_flux .eq. id_SGS_NL_grad) then
        call add_phys_name_ctl(grad_v_1%name, field_ctl)
        call add_phys_name_ctl(grad_v_2%name, field_ctl)
        call add_phys_name_ctl(grad_v_3%name, field_ctl)
        call add_phys_name_ctl(grad_composition%name, field_ctl)
      end if
!
      end subroutine add_field_name_4_SGS
!
! -----------------------------------------------------------------------
!
      subroutine add_field_name_dynamic_SGS                             &
     &         (SGS_param, fl_prop, field_ctl)
!
      use t_SGS_model_coef_labels
      use t_SGS_enegy_flux_labels
      use m_filtered_field_labels
      use m_grad_filter_field_labels
      use m_diff_filter_vect_labels
      use m_wide_filter_field_labels
      use m_wide_SGS_term_labels
      use add_nodal_fields_ctl
!
      type(SGS_model_control_params), intent(in) :: SGS_param
      type(fluid_property), intent(in) :: fl_prop
      type(ctl_array_c3), intent(inout) :: field_ctl
!
!
      if(SGS_param%iflag_dynamic .eq. id_SGS_DYNAMIC_OFF) return
!
!   Add model coefficients
!
      if(SGS_param%iflag_SGS_m_flux .gt. id_SGS_none) then
        call add_phys_name_ctl(Csim_SGS_inertia%name, field_ctl)
      end if
      if(SGS_param%iflag_SGS_lorentz .gt. id_SGS_none) then
        call add_phys_name_ctl(Csim_SGS_Lorentz%name, field_ctl)
      end if
      if(SGS_param%iflag_SGS_uxb .gt. id_SGS_none) then
        call add_phys_name_ctl(Csim_SGS_induction%name, field_ctl)
      end if
      if(SGS_param%iflag_SGS_h_flux .gt. id_SGS_none) then
        call add_phys_name_ctl(Csim_SGS_heat_flux%name, field_ctl)
      end if
      if(SGS_param%iflag_SGS_c_flux .gt. id_SGS_none) then
        call add_phys_name_ctl(Csim_SGS_composit_flux%name, field_ctl)
      end if
!
      if(SGS_param%iflag_SGS_gravity .gt. id_SGS_none) then
        if(fl_prop%iflag_4_gravity) then
          call add_phys_name_ctl(Csim_SGS_buoyancy%name, field_ctl)
        end if
        if(fl_prop%iflag_4_composit_buo) then
          call add_phys_name_ctl(Csim_SGS_composit_buo%name, field_ctl)
        end if
      end if
!
!    Add filtered field
!
      if(SGS_param%iflag_SGS_m_flux .eq. id_SGS_similarity) then
        call add_phys_name_ctl(wide_filter_velocity%name, field_ctl)
        call add_phys_name_ctl(wide_filter_vorticity%name, field_ctl)
      else if(SGS_param%iflag_SGS_m_flux .eq. id_SGS_NL_grad) then
        call add_phys_name_ctl(filter_velocity%name, field_ctl)
        call add_phys_name_ctl(filter_vorticity%name, field_ctl)
      end if
!
      if(SGS_param%iflag_SGS_lorentz .eq. id_SGS_similarity) then
        call add_phys_name_ctl(wide_filter_magne%name, field_ctl)
        call add_phys_name_ctl(wide_filter_current%name, field_ctl)
      else if(SGS_param%iflag_SGS_lorentz .eq. id_SGS_NL_grad) then
        call add_phys_name_ctl(filter_magne%name, field_ctl)
        call add_phys_name_ctl(filter_current%name, field_ctl)
      end if
!
      if(SGS_param%iflag_SGS_uxb .eq. id_SGS_similarity) then
        call add_phys_name_ctl(wide_filter_velocity%name, field_ctl)
        call add_phys_name_ctl(wide_filter_magne%name, field_ctl)
      else if(SGS_param%iflag_SGS_uxb .eq. id_SGS_NL_grad) then
        call add_phys_name_ctl(filter_velocity%name, field_ctl)
        call add_phys_name_ctl(filter_magne%name, field_ctl)
      end if
!
      if(SGS_param%iflag_SGS_h_flux .eq. id_SGS_similarity) then
        call add_phys_name_ctl(wide_filter_velocity%name, field_ctl)
        call add_phys_name_ctl(wide_filter_temp%name, field_ctl)
      else if(SGS_param%iflag_SGS_h_flux .eq. id_SGS_NL_grad) then
        call add_phys_name_ctl(filter_velocity%name, field_ctl)
        call add_phys_name_ctl(filter_temperature%name, field_ctl)
      end if
!
      if(SGS_param%iflag_SGS_c_flux .eq. id_SGS_similarity) then
        call add_phys_name_ctl(wide_filter_velocity%name, field_ctl)
        call add_phys_name_ctl(wide_filter_composition%name, field_ctl)
      else if(SGS_param%iflag_SGS_c_flux .eq. id_SGS_NL_grad) then
        call add_phys_name_ctl(filter_velocity%name, field_ctl)
        call add_phys_name_ctl(filter_composition%name, field_ctl)
      end if
!
!
      if(SGS_param%iflag_SGS_m_flux .eq. id_SGS_NL_grad) then
        call add_phys_name_ctl(grad_filtered_w_1%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_w_2%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_w_3%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_1%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_2%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_3%name, field_ctl)
      end if
!
      if(SGS_param%iflag_SGS_lorentz .eq. id_SGS_NL_grad) then
        call add_phys_name_ctl(grad_filtered_j_1%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_j_2%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_j_3%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_b_1%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_b_2%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_b_3%name, field_ctl)
      end if
!
      if(SGS_param%iflag_SGS_uxb .eq. id_SGS_NL_grad) then
        call add_phys_name_ctl(grad_filtered_v_1%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_2%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_3%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_b_1%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_b_2%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_b_3%name, field_ctl)
      end if
!
      if(SGS_param%iflag_SGS_h_flux .eq. id_SGS_NL_grad) then
        call add_phys_name_ctl(grad_filtered_v_1%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_2%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_3%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_temp%name, field_ctl)
      end if
!
      if(SGS_param%iflag_SGS_c_flux .eq. id_SGS_NL_grad) then
        call add_phys_name_ctl(grad_filtered_v_1%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_2%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_v_3%name, field_ctl)
        call add_phys_name_ctl(grad_filtered_comp%name, field_ctl)
      end if
!
!       Add SGS fluxes
!
      if(SGS_param%iflag_SGS_m_flux .gt. id_SGS_none)                   &
     &   call add_phys_name_ctl(wide_SGS_inertia%name, field_ctl)
      if(SGS_param%iflag_SGS_lorentz .gt. id_SGS_none)                  &
     &   call add_phys_name_ctl(wide_SGS_Lorentz%name, field_ctl)
      if(SGS_param%iflag_SGS_uxb .gt. id_SGS_none)                      &
     &   call add_phys_name_ctl(wide_SGS_vp_induction%name, field_ctl)
      if(SGS_param%iflag_SGS_h_flux .gt. id_SGS_none)                   &
     &   call add_phys_name_ctl(wide_SGS_heat_flux%name, field_ctl)
      if(SGS_param%iflag_SGS_c_flux .gt. id_SGS_none)                   &
     &   call add_phys_name_ctl(wide_SGS_composit_flux%name, field_ctl)
!
!
      if(SGS_param%iflag_SGS_gravity .gt. id_SGS_none) then
        call add_phys_name_ctl(Reynolds_work%name, field_ctl)
!
        if(fl_prop%iflag_4_gravity) then
          call add_phys_name_ctl(SGS_buoyancy_flux%name, field_ctl)
        end if
!
        if(fl_prop%iflag_4_composit_buo) then
          call add_phys_name_ctl                                        &
     &       (SGS_comp_buoyancy_flux%name, field_ctl)
        end if
      end if
!
!
      if(SGS_param%iflag_SGS_m_flux .gt. id_SGS_none)                   &
     &   call add_phys_name_ctl(double_SGS_inertia%name, field_ctl)
      if(SGS_param%iflag_SGS_lorentz .gt. id_SGS_none)                  &
     &   call add_phys_name_ctl(double_SGS_Lorentz%name, field_ctl)
      if(SGS_param%iflag_SGS_uxb .gt. id_SGS_none)                      &
     &   call add_phys_name_ctl(double_SGS_vp_induction%name,           &
     &                          field_ctl)
      if(SGS_param%iflag_SGS_h_flux .gt. id_SGS_none)                   &
     &   call add_phys_name_ctl(double_SGS_heat_flux%name, field_ctl)
      if(SGS_param%iflag_SGS_c_flux .gt. id_SGS_none)                   &
     &   call add_phys_name_ctl(double_SGS_composit_flux%name,          &
     &                          field_ctl)
!
!      if(SGS_param%iflag_SGS_gravity .gt. id_SGS_none) then
!        call add_phys_name_ctl(filter_velocity%name, field_ctl)
!        call add_phys_name_ctl(filter_temperature%name, field_ctl)
!
!        call add_phys_name_ctl(SGS_composit_buoyancy%name, field_ctl)
!      end if
!
      end subroutine add_field_name_dynamic_SGS
!
! -----------------------------------------------------------------------
!
      end module add_sph_SGS_MHD_fld_2_ctl

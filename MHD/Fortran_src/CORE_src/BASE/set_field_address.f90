!>@file   set_field_address.f90
!!        module set_field_address
!!
!! @author H. Matsui
!! @date   Programmed on July, 2006
!! @n      Modified  on Jan., 2012
!!
!!
!> @brief Set start address for field data
!!
!!@verbatim
!!      subroutine set_field_addresses(istart_adress, num_field,        &
!!     &          field_name, num_component, iphys)
!!      integer(kind = kint), intent(in) :: istart_adress
!!      integer(kind = kint), intent(in) :: num_field
!!      integer(kind = kint), intent(in) :: num_component(num_field)
!!      character(len = kchara), intent(in) :: field_name(num_field)
!!      type(phys_address), intent(inout) :: iphys
!!@endverbatim
!!
!!@n @param istart_adress             start address for field data
!!@n @param num_field                 number of field
!!@n @param num_component(num_field)  number of components of field
!!@n @param field_name(num_field)     list of field names
!!@n @param iphys                     structure of field addresses
!
!
      module set_field_address
!
      use m_precision
!
      use t_phys_address
      use t_phys_data
!
      implicit none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine set_field_addresses(istart_adress, num_field,          &
     &          field_name, num_component, iphys)
!
      use m_phys_labels
      use t_base_force_labels
      use t_diff_vector_labels
      use t_SGS_term_labels
      use t_SGS_enegy_flux_labels
      use t_SGS_model_coef_labels
      use m_rot_filtered_force_labels
      use m_div_filtered_force_labels
      use m_diff_filter_vect_labels
      use m_wide_SGS_term_labels
      use m_true_SGS_term_labels
!
      integer(kind = kint), intent(in) :: istart_adress
      integer(kind = kint), intent(in) :: num_field
      integer(kind = kint), intent(in) :: num_component(num_field)
      character(len = kchara), intent(in) :: field_name(num_field)
!
      type(phys_address), intent(inout) :: iphys
!
      integer(kind = kint) :: i, i0
      logical :: flag
!
!
      i0 = istart_adress
      do i = 1, num_field
!
        if      ( field_name(i) .eq. fhd_velo ) then
          iphys%i_velo = i0
        else if ( field_name(i) .eq. fhd_press ) then
          iphys%i_press = i0
        else if ( field_name(i) .eq. fhd_temp ) then
          iphys%i_temp = i0
        else if ( field_name(i) .eq. fhd_part_temp ) then
          iphys%i_per_temp = i0
        else if ( field_name(i) .eq. fhd_vort ) then
          iphys%i_vort = i0
        else if ( field_name(i) .eq. fhd_light ) then
          iphys%i_light = i0
        else if ( field_name(i) .eq. fhd_part_light ) then
          iphys%i_per_light = i0
        else if ( field_name(i) .eq. fhd_magne ) then
          iphys%i_magne = i0
        else if ( field_name(i) .eq. fhd_vecp ) then
          iphys%i_vecp = i0
        else if ( field_name(i) .eq. fhd_mag_potential ) then
          iphys%i_mag_p = i0
        else if ( field_name(i) .eq. fhd_scalar_potential ) then
          iphys%i_scalar_p = i0
        else if ( field_name(i) .eq. fhd_current ) then
          iphys%i_current = i0
        else if ( field_name(i) .eq. fhd_e_field ) then
          iphys%i_electric = i0
        else if ( field_name(i) .eq. fhd_poynting ) then
          iphys%i_poynting = i0
        else if ( field_name(i) .eq. fhd_entropy ) then
          iphys%i_entropy = i0
        else if ( field_name(i) .eq. fhd_per_entropy ) then
          iphys%i_per_entropy = i0
        else if ( field_name(i) .eq. fhd_ref_entropy ) then
          iphys%i_ref_entropy = i0
        else if ( field_name(i) .eq. fhd_density ) then
          iphys%i_density = i0
        else if ( field_name(i) .eq. fhd_per_density ) then
          iphys%i_per_density = i0
        else if ( field_name(i) .eq. fhd_ref_density ) then
          iphys%i_ref_density = i0
!
        else if ( field_name(i) .eq. fhd_heat_source ) then
          iphys%i_heat_source = i0
        else if ( field_name(i) .eq. fhd_light_source ) then
          iphys%i_light_source = i0
        else if ( field_name(i) .eq. fhd_entropy_source ) then
          iphys%i_entropy_source = i0
        end if
!
        if ( field_name(i) .eq. fhd_filter_velo ) then
          iphys%i_filter_velo =    i0
        else if ( field_name(i) .eq. fhd_filter_vort ) then
          iphys%i_filter_vort =    i0
        else if ( field_name(i) .eq. fhd_filter_temp ) then
          iphys%i_filter_temp =    i0
        else if ( field_name(i) .eq. fhd_filter_vecp ) then
          iphys%i_filter_vecp =    i0
        else if ( field_name(i) .eq. fhd_filter_magne ) then
          iphys%i_filter_magne =   i0
        else if ( field_name(i) .eq. fhd_filter_current ) then
          iphys%i_filter_current = i0
        else if ( field_name(i) .eq. fhd_filter_pert_temp ) then
          iphys%i_filter_par_t =   i0
        else if ( field_name(i) .eq. fhd_filter_comp ) then
          iphys%i_filter_comp =    i0
!
        else if ( field_name(i) .eq. fhd_truncated_B) then
          iphys%i_truncated_B =    i0
!
        else if ( field_name(i) .eq. div_filtered_velo%name ) then
          iphys%i_div_filter_v =    i0
        else if ( field_name(i) .eq. div_filtered_magne%name ) then
          iphys%i_div_filter_b =    i0
        else if ( field_name(i) .eq. div_filtered_vector_potential%name ) then
          iphys%i_div_filter_a =    i0
!
        else if ( field_name(i) .eq. fhd_kinetic_helicity ) then
          iphys%i_k_heli = i0
        else if ( field_name(i) .eq. fhd_magnetic_helicity ) then
          iphys%i_m_heli = i0
        else if ( field_name(i) .eq. fhd_current_helicity ) then
          iphys%i_c_heli = i0
        else if ( field_name(i) .eq. fhd_cross_helicity ) then
          iphys%i_x_heli = i0
!
        else if (field_name(i) .eq. magnetic_ene_generation%name) then
          iphys%ene_flux%i_me_gen =   i0
        else if (field_name(i) .eq. Lorentz_work%name) then
          iphys%ene_flux%i_ujb =      i0
        else if (field_name(i) .eq. work_against_Lorentz%name) then
          iphys%ene_flux%i_nega_ujb = i0
        else if (field_name(i) .eq. mag_tension_work%name) then
          iphys%ene_flux%i_m_tension_wk = i0
        else if (field_name(i) .eq. buoyancy_flux%name) then
          iphys%ene_flux%i_buo_gen =   i0
        else if (field_name(i) .eq. composite_buoyancy_flux%name) then
          iphys%ene_flux%i_c_buo_gen = i0
        else if (field_name(i) .eq. viscous_ene_diffusion%name) then
          iphys%i_vis_e_diffuse = i0
        else if (field_name(i) .eq. magnetic_ene_diffusion%name) then
          iphys%i_mag_e_diffuse = i0
        else if (field_name(i) .eq. temp_generation%name) then
          iphys%ene_flux%i_temp_gen = i0
        else if (field_name(i) .eq. pert_temp_generation%name) then
          iphys%ene_flux%i_par_t_gen = i0
        else if (field_name(i) .eq. pert_comp_generation%name) then
          iphys%i_par_c_gen = i0
!
        else if (field_name(i) .eq. fhd_thermal_diffusion) then
          iphys%i_t_diffuse =  i0
        else if (field_name(i) .eq. fhd_viscous) then
          iphys%i_v_diffuse =  i0
        else if (field_name(i) .eq. fhd_w_viscous) then
          iphys%i_w_diffuse =  i0
        else if (field_name(i) .eq. fhd_vecp_diffuse) then
          iphys%i_vp_diffuse = i0
        else if (field_name(i) .eq. fhd_mag_diffuse) then
          iphys%i_b_diffuse = i0
        else if (field_name(i) .eq. fhd_c_diffuse) then
          iphys%i_c_diffuse =  i0
!
        else if (field_name(i) .eq. fhd_div_h_flux ) then
          iphys%i_h_flux_div =  i0
        else if (field_name(i) .eq. fhd_div_ph_flux ) then
          iphys%i_ph_flux_div = i0
        else if (field_name(i) .eq. fhd_div_c_flux ) then
          iphys%i_c_flux_div =  i0
        else if (field_name(i) .eq. fhd_div_pc_flux ) then
          iphys%i_pc_flux_div = i0
        else if (field_name(i) .eq. fhd_div_m_flux) then
          iphys%i_m_flux_div =  i0
        else if (field_name(i) .eq. fhd_div_maxwell_t) then
          iphys%i_maxwell_div = i0
        else if (field_name(i) .eq. fhd_div_induct_t) then
          iphys%i_induct_div =  i0
        end if
!
        call set_base_force_addresses                                   &
     &     (i0, field_name(i), iphys%forces, flag)
!
        call set_SGS_term_addresses                                     &
     &     (i0, field_name(i), iphys%SGS_term, flag)
        call set_div_SGS_term_addresses                                 &
     &     (i0, field_name(i), iphys%div_SGS, flag)
        call set_rot_SGS_term_addresses                                 &
     &     (i0, field_name(i), iphys%rot_SGS, flag)
!
        call set_wide_SGS_term_addresses                                &
     &     (i0, field_name(i), iphys%wide_SGS, flag)
        call set_double_SGS_term_addresses                              &
     &     (i0, field_name(i), iphys%dble_SGS, flag)
!
        call set_SGS_model_coef_addresses                               &
     &     (i0, field_name(i), iphys%Csim, flag)
!
        call set_SGS_ene_flux_addresses                                 &
     &     (i0, field_name(i), iphys%SGS_ene_flux, flag)
!
        call set_force_w_SGS_addresses                                  &
     &     (i0, field_name(i), iphys%frc_w_SGS, flag)
!
        if ( field_name(i) .eq. fhd_geostrophic ) then
          iphys%i_geostrophic =  i0
        end if
!
        call set_true_SGS_term_addresses                                &
     &     (i0, field_name(i), iphys%true_SGS, flag)
        call set_true_div_SGS_term_addresses                            &
     &     (i0, field_name(i), iphys%true_div_SGS, flag)
        call set_true_SGS_ene_flux_addresses                            &
     &     (i0, field_name(i), iphys%true_SGS_eflux, flag)
!
        if ( field_name(i) .eq. pressure_work%name ) then
          iphys%i_p_phi = i0
        else if ( field_name(i) .eq. m_potential_work%name ) then
          iphys%i_m_phi = i0
        else if ( field_name(i) .eq. fhd_ref_temp ) then
          iphys%i_ref_t = i0
        else if ( field_name(i) .eq. fhd_ref_light ) then
          iphys%i_ref_c = i0
        end if
!
        call set_gradient_field_addresses                               &
      &     (i0, field_name(i), iphys%grad_fld, flag)
        call set_diff_vector_addresses                                  &
      &     (i0, field_name(i), iphys%diff_vector, flag)
!
        if      ( field_name(i) .eq. fhd_square_v ) then
          iphys%i_square_v = i0
        else if ( field_name(i) .eq. fhd_square_w ) then
          iphys%i_square_w = i0
        else if ( field_name(i) .eq. fhd_square_b ) then
          iphys%i_square_b = i0
        else if ( field_name(i) .eq. fhd_square_a ) then
          iphys%i_square_a = i0
        else if ( field_name(i) .eq. fhd_square_j ) then
          iphys%i_square_j = i0
        else if ( field_name(i) .eq. fhd_square_t ) then
          iphys%i_square_t = i0
        else if ( field_name(i) .eq. fhd_square_c ) then
          iphys%i_square_c = i0
        end if
!
        call set_grad_filter_field_addresses                            &
     &     (i0, field_name(i), iphys%grad_fil_fld, flag)
        call set_diff_filter_vect_addresses                             &
     &     (i0, field_name(i), iphys%diff_fil_vect, flag)
!
        call set_dynamic_SGS_work_addresses                             &
     &     (i0, field_name(i), iphys%SGS_wk, flag)
!
        call set_wide_fil_vector_addresses                              &
     &     (i0, field_name(i), iphys%wide_filter_fld, flag)
        call set_wide_fil_scaler_addresses                              &
     &     (i0, field_name(i), iphys%wide_filter_fld, flag)
        call set_wide_fil_grad_addresses                                &
     &     (i0, field_name(i), iphys%wide_filter_grad, flag)
!
        call set_dble_fil_vector_addresses                              &
     &     (i0, field_name(i), iphys%dbl_filter_fld, flag)
        call set_dble_fil_scaler_addresses                              &
     &     (i0, field_name(i), iphys%dbl_filter_fld, flag)
        call set_dble_fil_grad_addresses                                &
     &     (i0, field_name(i), iphys%dbl_filter_grad, flag)
!
        if ( field_name(i) .eq. fhd_div_inertia ) then
          iphys%i_div_inertia =    i0
        else if ( field_name(i) .eq. fhd_div_Lorentz ) then
          iphys%i_div_Lorentz =    i0
        else if ( field_name(i) .eq. fhd_div_Coriolis ) then
          iphys%i_div_Coriolis =   i0
        else if ( field_name(i) .eq. fhd_div_buoyancy ) then
          iphys%i_div_buoyancy =   i0
        else if ( field_name(i) .eq. fhd_div_comp_buo ) then
          iphys%i_div_comp_buo =   i0
        else if ( field_name(i) .eq. fhd_div_viscous ) then
          iphys%i_div_viscous =    i0
!
        else if ( field_name(i) .eq. fhd_rot_inertia ) then
          iphys%i_rot_inertia =    i0
        else if ( field_name(i) .eq. fhd_rot_Lorentz ) then
          iphys%i_rot_Lorentz =    i0
        else if ( field_name(i) .eq. fhd_rot_Coriolis ) then
          iphys%i_rot_Coriolis =   i0
        else if ( field_name(i) .eq. fhd_rot_buoyancy ) then
          iphys%i_rot_buoyancy =   i0
        else if ( field_name(i) .eq. fhd_rot_comp_buo ) then
          iphys%i_rot_comp_buo =   i0
!
        else if ( field_name(i) .eq. fhd_forces ) then
          iphys%i_forces =     i0
        else if ( field_name(i) .eq. fhd_rot_forces ) then
          iphys%i_rot_forces = i0
        else if ( field_name(i) .eq. fhd_div_forces ) then
          iphys%i_div_forces = i0
        end if
!
        call set_filtered_force_addresses(i0, field_name(i),            &
     &      iphys%rot_frc_by_filter, flag)
        call set_rot_fil_force_addresses(i0, field_name(i),             &
     &      iphys%rot_frc_by_filter, flag)
        call set_div_fil_force_addresses(i0, field_name(i),             &
     &      iphys%div_frc_by_filter, flag)
        call set_filter_ene_flux_addresses(i0, field_name(i),           &
     &      iphys%eflux_by_filter, flag)
!
        if ( field_name(i) .eq. fhd_pre_mom ) then
          iphys%i_pre_mom =      i0
        else if ( field_name(i) .eq. fhd_pre_uxb ) then
          iphys%i_pre_uxb =      i0
        else if ( field_name(i) .eq. fhd_pre_heat ) then
          iphys%i_pre_heat =     i0
        else if ( field_name(i) .eq. fhd_pre_composit ) then
          iphys%i_pre_composit = i0
        else if ( field_name(i) .eq. fhd_pre_press ) then
          iphys%i_pre_press =    i0
!
        else if ( field_name(i) .eq. fhd_chk_mom ) then
          iphys%i_chk_mom =       i0
        else if ( field_name(i) .eq. fhd_chk_uxb ) then
          iphys%i_chk_uxb =       i0
        else if ( field_name(i) .eq. fhd_chk_heat ) then
          iphys%i_chk_heat =      i0
        else if ( field_name(i) .eq. fhd_chk_composit ) then
          iphys%i_chk_composit =  i0
        else if ( field_name(i) .eq. fhd_chk_press ) then
          iphys%i_chk_press =     i0
        else if ( field_name(i) .eq. fhd_chk_potential ) then
          iphys%i_chk_potential = i0
!
        else if ( field_name(i) .eq. fhd_chk_mom_2 ) then
          iphys%i_chk_mom_2 =       i0
        else if ( field_name(i) .eq. fhd_chk_uxb_2 ) then
          iphys%i_chk_uxb_2 =       i0
        else if ( field_name(i) .eq. fhd_chk_heat_2 ) then
          iphys%i_chk_heat_2 =      i0
        else if ( field_name(i) .eq. fhd_chk_composit_2 ) then
          iphys%i_chk_composit_2 =  i0
        else if ( field_name(i) .eq. fhd_chk_press_2 ) then
          iphys%i_chk_press_2 =     i0
        else if ( field_name(i) .eq. fhd_chk_potential_2 ) then
          iphys%i_chk_potential_2 = i0
        end if
!
        if ( field_name(i) .eq. fhd_velocity_scale) then
          iphys%i_velo_scale =  i0
        else if ( field_name(i) .eq. fhd_magnetic_scale) then
          iphys%i_magne_scale = i0
        else if ( field_name(i) .eq. fhd_temp_scale) then
          iphys%i_temp_scale =  i0
        else if ( field_name(i) .eq. fhd_composition_scale) then
          iphys%i_comp_scale =  i0
        end if
!
!   Old field label... Should be deleted later!!
        if (field_name(i) .eq. buoyancy_work%name) then
          iphys%ene_flux%i_buo_gen =   i0
        end if
!
        i0 = i0 + num_component(i)
      end do
!
      end subroutine set_field_addresses
!
!  --------------------------------------------------------------------
!
      end module set_field_address

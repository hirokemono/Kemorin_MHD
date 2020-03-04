!>@file   set_mean_square_array.f90
!!        module set_mean_square_array
!!
!! @author H. Matsui
!! @date   Programmed in 2002
!! @n      Modified  on Jan., 2013
!!
!
!> @brief addresses for volume integrated data
!!
!!@verbatim
!!      subroutine set_mean_square_values                               &
!!     &         (nod_fld, i_rms, j_ave, ifld_msq)
!!        type(phys_data), intent(in) :: nod_fld
!!        type(phys_address), intent(inout) :: i_rms, j_ave
!!        type(mean_square_address), intent(inout) :: ifld_msq
!!        type(mean_square_values), intent(inout) :: fem_msq
!!@endverbatim
!
      module set_mean_square_array
!
      use m_precision
!
      use t_phys_address
      use t_phys_data
      use t_mean_square_filed_list
      use t_mean_square_values
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_mean_square_values                                 &
     &         (nod_fld, iphys, i_rms, j_ave, ifld_msq, msq_list)
!
      use t_base_field_labels
      use t_diff_vector_labels
      use t_SGS_term_labels
      use t_SGS_enegy_flux_labels
      use t_SGS_model_coef_labels
      use m_phys_constants
      use m_volume_average_labels
      use m_diff_SGS_term_labels
      use m_filtered_force_labels
      use m_dble_filter_field_labels
      use m_filtered_ene_flux_labels
      use m_diff_filter_vect_labels
!
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys
!
      type(phys_address), intent(inout) :: i_rms, j_ave
      type(mean_square_address), intent(inout) :: ifld_msq
      type(mean_square_list), intent(inout) :: msq_list
!
      integer (kind = kint) :: i, num_comps
      character(len = kchara) :: field_name
!
!
      do i = 1, nod_fld%num_phys
        field_name = nod_fld%phys_name(i)
        num_comps =  nod_fld%num_component(i)
        if (nod_fld%iflag_monitor(i) .eq. 1) then
          if(field_name .eq. fhd_velo) then
            call set_rms_address(field_name, num_comps, iphys%i_velo,   &
     &          i_rms%i_velo, j_ave%i_velo, msq_list)
!
            i_rms%grad_fld%i_div_v = msq_list%numrms + 1
            j_ave%grad_fld%i_div_v = msq_list%numave + 1
!
            ifld_msq%ja_amom = msq_list%numave + 2
!
            msq_list%numrms = msq_list%numrms + 1
            msq_list%numave = msq_list%numave + 4
          end if
!
          if ( field_name .eq. fhd_magne ) then
            call set_rms_address(field_name, num_comps, iphys%i_magne,  &
     &          i_rms%i_magne, j_ave%i_magne, msq_list)
!
            ifld_msq%ir_me_ic =  msq_list%numrms + 1
            ifld_msq%ja_mag_ic = msq_list%numave + 1
!
            i_rms%grad_fld%i_div_b = msq_list%numrms + 2
            j_ave%grad_fld%i_div_b = msq_list%numave + 4
!
            msq_list%numrms = msq_list%numrms + 2
            msq_list%numave = msq_list%numave + 4
          end if
!
          if ( field_name .eq. fhd_vecp ) then
            call set_rms_address(field_name, num_comps, iphys%i_vecp,   &
     &          i_rms%i_vecp, j_ave%i_vecp, msq_list)
!
            i_rms%grad_fld%i_div_a =  msq_list%numrms + 1
            j_ave%grad_fld%i_div_a = msq_list%numave + 1
!
            msq_list%numrms = msq_list%numrms + 1
            msq_list%numave = msq_list%numave + 1
          end if
!
          if ( field_name .eq. fhd_vort ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_vort,                    &
     &          i_rms%i_vort, j_ave%i_vort, msq_list)
!
            ifld_msq%ir_rms_w = msq_list%numrms + 1
!
            msq_list%numrms = msq_list%numrms + 1
          end if
!
          if ( field_name .eq. fhd_current ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_current,                 &
     &          i_rms%i_current, j_ave%i_current, msq_list)
!
            ifld_msq%ir_sqj_ic = msq_list%numrms + 1
            ifld_msq%ja_j_ic = msq_list%numave + 1
!
            ifld_msq%ir_rms_j =    msq_list%numrms + 2
            ifld_msq%ir_rms_j_ic = msq_list%numrms + 3
!
            msq_list%numrms = msq_list%numrms + 3
            msq_list%numave = msq_list%numave + 3
          end if
!
          if ( field_name .eq. fhd_filter_velo ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_filter_velo,             &
     &          i_rms%i_filter_velo, j_ave%i_filter_velo, msq_list)
!
            i_rms%i_div_filter_v = msq_list%numrms + 1
            j_ave%i_div_filter_v = msq_list%numave + 1
!
            ifld_msq%jr_amom_f = msq_list%numave + 2
!
            msq_list%numrms = msq_list%numrms + 1
            msq_list%numave = msq_list%numave + 4
          end if
!
          if(field_name .eq. fhd_filter_vecp) then
            call set_rms_address                                        &
     &         (field_name, n_scalar, iphys%i_filter_vecp,              &
     &          i_rms%i_filter_vecp, j_ave%i_filter_vecp, msq_list)
!
            i_rms%i_div_filter_a = msq_list%numrms + 1
            j_ave%i_div_filter_a = msq_list%numave + 1
!
            msq_list%numrms = msq_list%numrms + 1
            msq_list%numave = msq_list%numave + 4
          end if
!
          if ( field_name .eq. fhd_filter_magne ) then
            num_comps = num_comps + n_vector + n_scalar
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_filter_magne,            &
     &          i_rms%i_filter_magne, j_ave%i_filter_magne, msq_list)
!
            ifld_msq%ir_me_f_ic =  msq_list%numrms + 1
            ifld_msq%ja_mag_f_ic = msq_list%numave + 1
!
            i_rms%i_div_filter_b = msq_list%numrms + 2
            j_ave%i_div_filter_b = msq_list%numave + 4
!
            msq_list%numrms = msq_list%numrms + 2
            msq_list%numave = msq_list%numave + 6
          end if
!
          if ( field_name .eq. fhd_e_field ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_electric,                &
     &          i_rms%i_electric, j_ave%i_electric, msq_list)
          else if ( field_name .eq. fhd_poynting ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_poynting,                &
     &          i_rms%i_poynting, j_ave%i_poynting, msq_list)
          else if ( field_name .eq. fhd_temp ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_temp,                    &
     &          i_rms%i_temp, j_ave%i_temp, msq_list)
          else if ( field_name .eq. fhd_press ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_press,                   &
     &          i_rms%i_press, j_ave%i_press, msq_list)
          else if ( field_name .eq. fhd_mag_potential ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_mag_p,                   &
     &          i_rms%i_mag_p, j_ave%i_mag_p, msq_list)
          end if
!
          if ( field_name .eq. fhd_part_temp ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_per_temp,                &
     &          i_rms%i_per_temp, j_ave%i_per_temp, msq_list)
          else if ( field_name .eq. fhd_light ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_light,                   &
     &          i_rms%i_light, j_ave%i_light, msq_list)
          else if ( field_name .eq. fhd_part_light ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_per_light,               &
     &          i_rms%i_per_light, j_ave%i_per_light, msq_list)
          else if ( field_name .eq. fhd_entropy ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_entropy,                 &
     &          i_rms%i_entropy, j_ave%i_entropy, msq_list)
          else if ( field_name .eq. fhd_per_entropy ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_per_entropy,             &
     &          i_rms%i_per_entropy, j_ave%i_per_entropy, msq_list)
          else if ( field_name .eq. fhd_density ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_density,                 &
     &          i_rms%i_density, j_ave%i_density, msq_list)
          else if ( field_name .eq. fhd_per_density ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_per_density,             &
     &          i_rms%i_per_density, j_ave%i_per_density, msq_list)
!
          else if ( field_name .eq. fhd_heat_source ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_heat_source,             &
     &          i_rms%i_heat_source, j_ave%i_heat_source, msq_list)
          else if ( field_name .eq. fhd_light_source ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_light_source,            &
     &          i_rms%i_light_source, j_ave%i_light_source, msq_list)
          else if ( field_name .eq. fhd_entropy_source ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_entropy_source,          &
     &          i_rms%i_entropy_source, j_ave%i_entropy_source,         &
     &          msq_list)
          end if
!
          if(check_force_vectors(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if ( field_name .eq. fhd_filter_temp ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_filter_temp,             &
     &          i_rms%i_filter_temp, j_ave%i_filter_temp, msq_list)
          else if ( field_name .eq. fhd_filter_comp ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_filter_comp,             &
     &          i_rms%i_filter_comp, j_ave%i_filter_comp, msq_list)
          else if(check_wide_filter_scalar(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          else if(check_double_filter_scalar(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if      ( field_name .eq. fhd_square_v ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_square_v,                &
     &          i_rms%i_square_v, j_ave%i_square_v, msq_list)
          else if ( field_name .eq. fhd_square_w ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_square_w,                &
     &          i_rms%i_square_w, j_ave%i_square_w, msq_list)
          else if ( field_name .eq. fhd_square_b ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_square_b,                &
     &          i_rms%i_square_b, j_ave%i_square_b, msq_list)
          else if ( field_name .eq. fhd_square_a ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_square_a,                &
     &          i_rms%i_square_a, j_ave%i_square_a, msq_list)
          else if ( field_name .eq. fhd_square_j ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_square_j,                &
     &          i_rms%i_square_j, j_ave%i_square_j, msq_list)
          else if ( field_name .eq. fhd_square_t ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_square_t,                &
     &          i_rms%i_square_t, j_ave%i_square_t, msq_list)
          else if ( field_name .eq. fhd_square_c ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_square_c,                &
     &          i_rms%i_square_c, j_ave%i_square_c, msq_list)
          end if
!
          if(check_difference_vectors(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if(check_diff_filter_vectors(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
          if(check_grad_filter_field(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if(check_flux_tensors(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
          if(check_asym_flux_tensors(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if ( field_name .eq. fhd_div_m_flux ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_m_flux_div,              &
     &          i_rms%i_m_flux_div, j_ave%i_m_flux_div, msq_list)
          else if ( field_name .eq. fhd_div_maxwell_t ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_maxwell_div,             &
     &          i_rms%i_maxwell_div, j_ave%i_maxwell_div, msq_list)
          else if ( field_name .eq. fhd_div_induct_t ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_induct_div,              &
     &          i_rms%i_induct_div, j_ave%i_induct_div, msq_list)
          else if(check_filtered_force(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if ( field_name .eq. fhd_viscous ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_v_diffuse,               &
     &          i_rms%i_v_diffuse, j_ave%i_v_diffuse, msq_list)
          else if ( field_name .eq. fhd_vecp_diffuse ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_vp_diffuse,              &
     &          i_rms%i_vp_diffuse, j_ave%i_vp_diffuse, msq_list)
          else if ( field_name .eq. fhd_mag_diffuse ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_b_diffuse,               &
     &          i_rms%i_b_diffuse, j_ave%i_b_diffuse, msq_list)
          else if ( field_name .eq. fhd_thermal_diffusion ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_t_diffuse,               &
     &          i_rms%i_t_diffuse, j_ave%i_t_diffuse, msq_list)
          else if ( field_name .eq. fhd_c_diffuse) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_c_diffuse,               &
     &          i_rms%i_c_diffuse, j_ave%i_c_diffuse, msq_list)
          end if
!
          if(check_SGS_vector_terms(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
          if(check_SGS_tensor_terms(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
          if(check_SGS_induction_tensor(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if(check_div_SGS_flux_vector(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
          if(check_div_SGS_flux_tensor(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
          if(check_rot_SGS_terms(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if(check_flux_tensor_w_SGS(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
          if(check_induction_tensor_w_SGS(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if(    check_wide_SGS_vector_terms(field_name)                &
     &      .or. check_double_SGS_vector_terms(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if ( field_name .eq. fhd_geostrophic ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_geostrophic,             &
     &          i_rms%i_geostrophic, j_ave%i_geostrophic, msq_list)
          end if
!
          if(check_force_w_SGS(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if ( field_name .eq. fhd_mag_ene_gen ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_me_gen,                  &
     &          i_rms%i_me_gen, j_ave%i_me_gen, msq_list)
          else if ( field_name .eq. fhd_Lorentz_work ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_ujb,                     &
     &          i_rms%i_ujb, j_ave%i_ujb, msq_list)
          else if ( field_name .eq. fhd_work_agst_Lorentz ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_nega_ujb,                &
     &          i_rms%i_nega_ujb, j_ave%i_nega_ujb, msq_list)
          else if ( field_name .eq. fhd_mag_tension_work ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_m_tension_wk,            &
     &          i_rms%i_m_tension_wk, j_ave%i_m_tension_wk, msq_list)
          else if ( field_name .eq. fhd_buoyancy_flux ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_buo_gen,                 &
     &          i_rms%i_buo_gen, j_ave%i_buo_gen, msq_list)
          else if ( field_name .eq. fhd_comp_buo_flux ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_c_buo_gen,               &
     &          i_rms%i_c_buo_gen, j_ave%i_c_buo_gen, msq_list)
          else if(check_filter_enegy_fluxes(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if(check_scalar_advection(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if ( field_name .eq. fhd_vis_ene_diffuse ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_vis_e_diffuse,           &
     &          i_rms%i_vis_e_diffuse, j_ave%i_vis_e_diffuse, msq_list)
          else if ( field_name .eq. fhd_mag_ene_diffuse ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_mag_e_diffuse,           &
     &          i_rms%i_mag_e_diffuse, j_ave%i_mag_e_diffuse, msq_list)
          else if ( field_name .eq. fhd_div_h_flux ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_h_flux_div,              &
     &          i_rms%i_h_flux_div, j_ave%i_h_flux_div, msq_list)
          else if ( field_name .eq. fhd_div_ph_flux ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_ph_flux_div,             &
     &          i_rms%i_ph_flux_div, j_ave%i_ph_flux_div, msq_list)
          else if ( field_name .eq. fhd_div_c_flux ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_c_flux_div,              &
     &          i_rms%i_c_flux_div, j_ave%i_c_flux_div, msq_list)
          else if ( field_name .eq. fhd_div_pc_flux ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_pc_flux_div,             &
     &          i_rms%i_pc_flux_div, j_ave%i_pc_flux_div, msq_list)
          else if ( field_name .eq. fhd_temp_generation ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_temp_gen,                &
     &          i_rms%i_temp_gen, j_ave%i_temp_gen, msq_list)
          else if ( field_name .eq. fhd_part_temp_gen ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_par_t_gen,               &
     &          i_rms%i_par_t_gen, j_ave%i_par_t_gen, msq_list)
          else if ( field_name .eq. fhd_part_comp_gen ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_par_c_gen,               &
     &          i_rms%i_par_c_gen, j_ave%i_par_c_gen, msq_list)
          end if
!
          if(check_SGS_ene_fluxes(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if (field_name .eq. fhd_SGS_div_h_flux_true) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_SGS_div_hf_true,         &
     &          i_rms%i_SGS_div_hf_true, j_ave%i_SGS_div_hf_true,       &
     &          msq_list)
          else if (field_name .eq. fhd_SGS_div_c_flux_true) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_SGS_div_cf_true,         &
     &          i_rms%i_SGS_div_cf_true, j_ave%i_SGS_div_cf_true,       &
     &          msq_list)
          else if (field_name .eq. fhd_SGS_div_m_flux_true) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_SGS_div_mf_true,         &
     &          i_rms%i_SGS_div_mf_true, j_ave%i_SGS_div_mf_true,       &
     &          msq_list)
          else if ( field_name .eq. fhd_SGS_Lorentz_true ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_SGS_Lor_true,            &
     &          i_rms%i_SGS_Lor_true, j_ave%i_SGS_Lor_true, msq_list)
          else if ( field_name .eq. fhd_SGS_mag_induct_true ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_SGS_idct_true,           &
     &          i_rms%i_SGS_idct_true, j_ave%i_SGS_idct_true, msq_list)
          end if
!
          if ( field_name .eq. fhd_SGS_Lorentz_wk_true ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_SGS_Lor_wk_tr,           &
     &          i_rms%i_SGS_Lor_wk_tr, j_ave%i_SGS_Lor_wk_tr, msq_list)
          else if ( field_name .eq. fhd_Reynolds_work_true ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_reynolds_wk_tr,          &
     &          i_rms%i_reynolds_wk_tr, j_ave%i_reynolds_wk_tr,         &
     &          msq_list)
          else if ( field_name .eq. fhd_SGS_temp_gen_true ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_SGS_t_gen_tr,            &
     &          i_rms%i_SGS_t_gen_tr, j_ave%i_SGS_t_gen_tr, msq_list)
          else if ( field_name .eq. fhd_SGS_comp_gen_true ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_SGS_c_gen_tr,            &
     &          i_rms%i_SGS_c_gen_tr, j_ave%i_SGS_c_gen_tr, msq_list)
          else if ( field_name .eq. fhd_SGS_m_ene_gen_true ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_SGS_me_gen_tr,           &
     &          i_rms%i_SGS_me_gen_tr, j_ave%i_SGS_me_gen_tr, msq_list)
          end if
!
          if ( field_name .eq. fhd_filter_vort ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_filter_vort,             &
     &          i_rms%i_filter_vort, j_ave%i_filter_vort, msq_list)
          else if ( field_name .eq. fhd_filter_current ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_filter_current,          &
     &          i_rms%i_filter_current, j_ave%i_filter_current,         &
     &          msq_list)
          else if(field_name .eq. fhd_truncated_B) then
            call set_rms_address                                        &
     &         (field_name, n_scalar, iphys%i_truncated_B,              &
     &          i_rms%i_truncated_B, j_ave%i_truncated_B, msq_list)
          end if
!
          if(check_wide_filter_vector(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          else if(check_wide_filter_scalar(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          else if(check_wide_filter_grad(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
!
          else if(check_double_filter_vector(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          else if(check_double_filter_scalar(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          else if(check_double_filter_grad(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
          if ( field_name .eq. fhd_velocity_scale ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_velo_scale,              &
     &          i_rms%i_velo_scale, j_ave%i_velo_scale, msq_list)
          else if ( field_name .eq. fhd_magnetic_scale ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_magne_scale,             &
     &          i_rms%i_magne_scale, j_ave%i_magne_scale, msq_list)
          else if ( field_name .eq. fhd_temp_scale ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_temp_scale,              &
     &          i_rms%i_temp_scale, j_ave%i_temp_scale, msq_list)
          else if ( field_name .eq. fhd_composition_scale ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_comp_scale,              &
     &          i_rms%i_comp_scale, j_ave%i_comp_scale, msq_list)
          end if
!
          if(check_SGS_moedel_coefs(field_name)) then
            call set_rms_address_list(i, nod_fld, msq_list)
          end if
!
!   Old field label... Should be deleted later!!
          if ( field_name .eq. fhd_buoyancy_work ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_buo_gen,                 &
     &          i_rms%i_buo_gen, j_ave%i_buo_gen, msq_list)
          end if
!
        else
          if ( field_name .eq. fhd_velo) then
            call set_rms_address                                        &
     &         (e_hd_div_v, n_scalar, iphys%i_velo,                     &
     &          i_rms%grad_fld%i_div_v, j_ave%grad_fld%i_div_v,         &
     &          msq_list)
          else if ( field_name .eq. fhd_magne ) then
            call set_rms_address                                        &
     &         (e_hd_div_b, n_scalar, iphys%i_magne,                    &
     &          i_rms%grad_fld%i_div_b, j_ave%grad_fld%i_div_b,         &
     &          msq_list)
          else if ( field_name .eq. fhd_vecp ) then
            call set_rms_address                                        &
     &         (e_hd_div_a, n_scalar, iphys%i_vecp,                     &
     &          i_rms%grad_fld%i_div_a, j_ave%grad_fld%i_div_a,         &
     &          msq_list)
          else if ( field_name .eq. fhd_filter_velo ) then
            call set_rms_address                                        &
     &         (e_hd_fil_div_v, n_scalar, iphys%i_filter_velo,          &
     &          i_rms%i_div_filter_v, j_ave%i_div_filter_v, msq_list)
          else if ( field_name .eq. fhd_filter_magne ) then
            call set_rms_address                                        &
     &         (e_hd_fil_div_b, n_scalar, iphys%i_filter_magne,         &
     &          i_rms%i_div_filter_b, j_ave%i_div_filter_b, msq_list)
          else if ( field_name .eq. fhd_filter_vecp ) then
            call set_rms_address                                        &
     &         (e_hd_fil_div_a, n_scalar, iphys%i_div_filter_a,         &
     &          i_rms%i_div_filter_a, j_ave%i_div_filter_a, msq_list)
          else if ( field_name .eq. fhd_mag_potential ) then
            call set_rms_address                                        &
     &         (field_name, num_comps, iphys%i_mag_p,                   &
     &          i_rms%i_mag_p, j_ave%i_mag_p, msq_list)
          end if

        end if
      end do
!
      ifld_msq%ivol =   msq_list%numrms + 1
      msq_list%numrms = msq_list%numrms + 1
!
      end subroutine set_mean_square_values
!
! ----------------------------------------------------------------------
!
      end module set_mean_square_array

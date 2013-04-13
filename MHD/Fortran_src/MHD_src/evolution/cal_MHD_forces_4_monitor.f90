!
!     module cal_MHD_forces_4_monitor
!
!     Written by H. Matsui
!
!      subroutine cal_fluxes_4_monitor
!      subroutine cal_forces_4_monitor
!      subroutine cal_work_4_forces
!
      module cal_MHD_forces_4_monitor
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_control_parameter
      use m_phys_labels
      use m_physical_property
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_fluxes_4_monitor
!
      use m_node_phys_address
!
      use cal_fluxes
      use int_vol_coriolis_term
!
!
      if (iphys%i_h_flux .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(fhd_h_flux)
        call cal_flux_vector(iphys%i_h_flux,                            &
     &      iphys%i_velo, iphys%i_temp)
      else if (iphys%i_ph_flux .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(fhd_ph_flux)
        call cal_flux_vector(iphys%i_ph_flux,                           &
     &      iphys%i_velo, iphys%i_par_temp)
      else if (iphys%i_c_flux .gt.  izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(fhd_c_flux)
        call cal_flux_vector(iphys%i_c_flux,                            &
     &      iphys%i_velo, iphys%i_light)
      else if (iphys%i_m_flux .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(fhd_mom_flux)
        call cal_flux_tensor(iphys%i_m_flux,                            &
     &      iphys%i_velo, iphys%i_velo)
      else if (iphys%i_maxwell .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(fhd_maxwell_t)
        call cal_maxwell_tensor(iphys%i_maxwell, iphys%i_magne)
      else if (iphys%i_induct_t .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(fhd_induct_t)
        call cal_induction_tensor(iphys%i_induct_t,                     &
     &      iphys%i_magne, iphys%i_velo)
      else if (iphys%i_density .gt. izero) then
        if(iflag_debug.gt.0) write(*,*) 'lead  ', trim(fhd_density)
        call set_boussinesq_density_at_node
      end if
!
      end subroutine cal_fluxes_4_monitor
!
!-----------------------------------------------------------------------
!
      subroutine cal_forces_4_monitor
!
      use m_node_phys_data
      use m_node_phys_address
!
      use cal_terms_for_heat
      use cal_momentum_terms
      use cal_magnetic_terms
      use cal_induction_terms
!
!
      if (iphys%i_h_advect .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_heat_advect)
        call cal_terms_4_heat(iphys%i_h_advect)
      end if
!
      if (iphys%i_ph_advect .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_part_h_advect)
        call cal_terms_4_heat(iphys%i_ph_advect)
      end if
!
      if (iphys%i_h_flux_div .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_div_h_flux)
        call cal_terms_4_heat(iphys%i_h_flux_div)
      end if
!
      if (iphys%i_ph_flux_div .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_div_ph_flux)
        call cal_terms_4_heat(iphys%i_ph_flux_div)
      end if
!
!
      if (iphys%i_m_advect .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_inertia)
        call cal_terms_4_momentum(iphys%i_m_advect)
      end if
!
      if (iphys%i_m_flux_div .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_div_m_flux)
        call cal_terms_4_momentum(iphys%i_m_flux_div)
      end if
!
      if (iphys%i_maxwell_div .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_div_maxwell_t)
        call cal_terms_4_momentum(iphys%i_maxwell_div)
      end if
!
      if (iphys%i_m_tension .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_mag_tension)
        call cal_terms_4_momentum(iphys%i_m_tension)
      end if
!
      if (iphys%i_lorentz .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_Lorentz)
        call cal_terms_4_momentum(iphys%i_lorentz)
      end if
!
      if (iphys%i_buoyancy .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_buoyancy)
        call cal_terms_4_momentum(iphys%i_buoyancy)
      end if
!
      if (iphys%i_comp_buo .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_comp_buo)
        call cal_terms_4_momentum(iphys%i_comp_buo)
      end if
!
      if (iphys%i_filter_buo .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_filter_buo)
        call cal_terms_4_momentum(iphys%i_filter_buo)
      end if
!
      if (iphys%i_coriolis .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_Coriolis)
        call cal_terms_4_momentum(iphys%i_coriolis)
      end if
!
!
      if (iphys%i_induct_div .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_div_induct_t)
        call cal_terms_4_magnetic(iphys%i_induct_div)
      end if
!
      if (iphys%i_induction .gt. izero                                  &
     &      .and. iflag_t_evo_4_magne .gt. izero ) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_mag_induct)
          call cal_terms_4_magnetic(iphys%i_induction)
      end if
!
      if (iphys%i_vp_induct .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_vp_induct)
        call cal_vecp_induction
      end if
!
!
      if (iphys%i_t_diffuse .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_thermal_diffusion)
        call cal_thermal_diffusion
      end if
!
      if (iphys%i_t_diffuse .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_thermal_diffusion)
        call cal_thermal_diffusion
      end if
!
      if (iphys%i_v_diffuse .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_viscous)
        call cal_viscous_diffusion
      end if
!
      if (iphys%i_vp_diffuse .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_vecp_diffuse)
        call cal_vecp_diffusion
      end if
!
      if (iphys%i_b_diffuse .gt. izero                                  &
     &      .and. iflag_t_evo_4_magne .gt. izero ) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_mag_diffuse)
        call cal_magnetic_diffusion
      end if
!
      end subroutine cal_forces_4_monitor
!
!-----------------------------------------------------------------------
!
      subroutine cal_work_4_forces
!
      use m_node_phys_address
      use m_node_phys_data
!
      use buoyancy_flux
      use products_nodal_fields
      use add_nodal_fields
      use subtract_nodal_fields
      use multi_by_const_fields
      use int_magne_diffusion
      use int_magne_induction
!
!
      if (iphys%i_induction .gt. izero                                  &
     &      .and. iflag_t_evo_4_vect_p .gt. 0 ) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_mag_induct)
          call s_int_magne_induction
      end if
!
      if (iphys%i_b_diffuse .gt. izero                                  &
     &      .and. iflag_t_evo_4_vect_p .gt. 0 ) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_mag_diffuse)
        call s_int_magne_diffusion
      end if
!
      if (iphys%i_electric .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_e_field)
        if (iphys%i_vp_diffuse .gt. 0) then
          call add_2_nod_vectors(iphys%i_electric,                      &
     &          iphys%i_vp_diffuse, iphys%i_vp_induct)
          call multi_by_const_nod_vector(iphys%i_electric,              &
     &          iphys%i_electric, dminus)
        else
          call multi_by_const_nod_vector(iphys%i_electric,              &
     &          iphys%i_current, coef_d_magne)
          call cal_phys_vector_product(iphys%i_vp_induct,               &
     &          iphys%i_velo, iphys%i_magne)
          call subtract_2_nod_tensors(iphys%i_electric,                 &
     &          iphys%i_electric, iphys%i_vp_induct)
        end if
      end if
!
!
!
      if (iphys%i_ujb .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_Lorentz_work)
        call cal_tri_product_4_scalar(iphys%i_ujb, iphys%i_velo,        &
     &        iphys%i_current, iphys%i_magne, coef_lor)
      end if
!
      if (iphys%i_nega_ujb .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_work_agst_Lorentz)
        call cal_tri_product_4_scalar(iphys%i_nega_ujb, iphys%i_velo,   &
     &        iphys%i_magne, iphys%i_current, coef_lor)
      end if
!
      if (iphys%i_me_gen .gt. izero) then
        if ( iflag_t_evo_4_magne .gt. 0 ) then
          call cal_tri_product_4_scalar(iphys%i_me_gen,                 &
     &        iphys%i_current, iphys%i_velo, iphys%i_magne, coef_lor)
        else
          call cal_phys_dot_product(iphys%i_me_gen, iphys%i_current,    &
     &        iphys%i_vp_induct)
        end if
      end if
!
!
!
!
      if (iphys%i_buo_gen .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_buoyancy_flux)
        call cal_gravity_flux(coef_buo, iphys%i_temp,                   &
     &        iphys%i_buo_gen)
      end if
!
      if (iphys%i_c_buo_gen .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_comp_buo_flux)
        call cal_gravity_flux(coef_comp_buo, iphys%i_light,             &
     &      iphys%i_c_buo_gen)
      end if
!
      if (iphys%i_f_buo_gen .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_filter_buo_flux)
        call cal_gravity_flux(coef_buo, iphys%i_filter_temp,            &
     &      iphys%i_f_buo_gen)
      end if
!
!
      if (iphys%i_temp_gen .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_temp_generation)
        call cal_phys_product_4_scalar(iphys%i_temp_gen,                &
     &        iphys%i_h_advect, iphys%i_temp)
      end if
!
      if (iphys%i_par_t_gen .gt. izero) then
        if(iflag_debug .ge. iflag_routine_msg)                          &
     &             write(*,*) 'lead  ', trim(fhd_part_temp_gen)
        call cal_phys_product_4_scalar(iphys%i_par_t_gen,               &
     &        iphys%i_ph_advect, iphys%i_par_temp)
      end if
!
!
      if (iphys%i_vis_e_diffuse .gt. izero) then
        call cal_phys_dot_product(iphys%i_vis_e_diffuse,                &
     &         iphys%i_velo, iphys%i_v_diffuse)
      end if
!
      if (iphys%i_mag_e_diffuse .gt. izero) then
        call cal_phys_dot_product(iphys%i_mag_e_diffuse,                &
     &      iphys%i_magne, iphys%i_b_diffuse)
      end if
!
      if (iphys%i_m_tension_wk .gt. izero) then
        call cal_phys_dot_product(iphys%i_m_tension_wk,                 &
     &         iphys%i_electric, iphys%i_magne)
      end if
!
      if (iphys%i_poynting .gt. izero) then
        call cal_phys_vector_product(iphys%i_poynting,                  &
     &         iphys%i_electric, iphys%i_magne)
      end if
!
      end subroutine cal_work_4_forces
!
!-----------------------------------------------------------------------
!
      end module cal_MHD_forces_4_monitor

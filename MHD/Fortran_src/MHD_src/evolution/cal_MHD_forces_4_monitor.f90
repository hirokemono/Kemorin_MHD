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
      integer (kind = kint) :: i
!
!
      do i = 1, num_nod_phys
        if (phys_nod_name(i).eq.fhd_heat_advect) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call cal_terms_4_heat(iphys%i_h_advect)
        else if (phys_nod_name(i).eq.fhd_part_h_advect) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call cal_terms_4_heat(iphys%i_ph_advect)
        else if (phys_nod_name(i).eq.fhd_div_h_flux) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call cal_terms_4_heat(iphys%i_h_flux_div)
        else if (phys_nod_name(i).eq.fhd_div_ph_flux) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call cal_terms_4_heat(iphys%i_ph_flux_div)
        else if (phys_nod_name(i).eq.fhd_inertia) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call cal_terms_4_momentum(iphys%i_m_advect)
!
        else if (phys_nod_name(i).eq.fhd_div_m_flux) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call cal_terms_4_momentum(iphys%i_m_flux_div)
        else if (phys_nod_name(i).eq.fhd_div_maxwell_t) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call cal_terms_4_momentum(iphys%i_maxwell_div)
        else if (phys_nod_name(i).eq.fhd_div_induct_t) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call cal_terms_4_magnetic(iphys%i_induct_div)
!
        else if (phys_nod_name(i).eq.fhd_mag_tension) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call cal_terms_4_momentum(iphys%i_m_tension)
        else if (phys_nod_name(i).eq.fhd_Lorentz) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call cal_terms_4_momentum(iphys%i_lorentz)
!
        else if ( phys_nod_name(i) .eq. fhd_buoyancy) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call cal_terms_4_momentum(iphys%i_buoyancy)
        else if ( phys_nod_name(i) .eq. fhd_comp_buo) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call cal_terms_4_momentum(iphys%i_comp_buo)
        else if ( phys_nod_name(i) .eq. fhd_filter_buo) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call cal_terms_4_momentum(iphys%i_filter_buo)
!
        else if (phys_nod_name(i).eq.fhd_Coriolis) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call cal_terms_4_momentum(iphys%i_coriolis)
!
        else if (phys_nod_name(i).eq.fhd_mag_induct                     &
     &      .and. iflag_t_evo_4_magne .gt. 0 ) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call cal_terms_4_magnetic(iphys%i_induction)
        else if (phys_nod_name(i).eq.fhd_vp_induct) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call cal_vecp_induction
!
        else if (phys_nod_name(i).eq.fhd_thermal_diffusion) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call cal_thermal_diffusion
        else if (phys_nod_name(i).eq.fhd_viscous) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call cal_viscous_diffusion
        else if (phys_nod_name(i).eq.fhd_vecp_diffuse) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call cal_vecp_diffusion
        else if (phys_nod_name(i).eq.fhd_mag_diffuse                    &
     &      .and. iflag_t_evo_4_magne .gt. 0 ) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call cal_magnetic_diffusion
        end if
      end do
!
      do i = 1, num_nod_phys
      end do
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
      integer (kind = kint) :: i
!
!
      do i = 1, num_nod_phys
        if (phys_nod_name(i).eq.fhd_mag_induct                          &
     &      .and. iflag_t_evo_4_vect_p .gt. 0 ) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call s_int_magne_induction
        else if (phys_nod_name(i).eq.fhd_mag_diffuse                    &
     &      .and. iflag_t_evo_4_vect_p .gt. 0 ) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call s_int_magne_diffusion
        end if
!
        if (phys_nod_name(i).eq.fhd_e_field) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          if (iphys%i_vp_diffuse .gt. 0) then
            call add_2_nod_vectors(iphys%i_electric,                    &
     &          iphys%i_vp_diffuse, iphys%i_vp_induct)
            call multi_by_const_nod_vector(iphys%i_electric,            &
     &          iphys%i_electric, dminus)
          else
            call multi_by_const_nod_vector(iphys%i_electric,            &
     &          iphys%i_current, coef_d_magne)
            call cal_phys_vector_product(iphys%i_vp_induct,             &
     &          iphys%i_velo, iphys%i_magne)
            call subtract_2_nod_tensors(iphys%i_electric,               &
     &          iphys%i_electric, iphys%i_vp_induct)
          end if
        end if
      end do
!
!
!
      do i = 1, num_nod_phys
!
        if ( phys_nod_name(i).eq.fhd_Lorentz_work) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call cal_tri_product_4_scalar(iphys%i_ujb, iphys%i_velo,      &
     &        iphys%i_current, iphys%i_magne, coef_lor)
!
        else if ( phys_nod_name(i).eq.fhd_work_agst_Lorentz) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call cal_tri_product_4_scalar(iphys%i_nega_ujb, iphys%i_velo, &
     &        iphys%i_magne, iphys%i_current, coef_lor)
!
        else if ( phys_nod_name(i).eq.fhd_mag_ene_gen) then
          if ( iflag_t_evo_4_magne .gt. 0 ) then
            call cal_tri_product_4_scalar(iphys%i_me_gen,               &
     &          iphys%i_current, iphys%i_velo, iphys%i_magne, coef_lor)
          else
            call cal_phys_dot_product(iphys%i_me_gen, iphys%i_current,  &
     &          iphys%i_vp_induct)
          end if
!
        else if ( phys_nod_name(i).eq.fhd_buoyancy_work ) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call cal_gravity_flux(coef_buo, iphys%i_temp,                 &
     &        iphys%i_buo_gen)
        else if ( phys_nod_name(i).eq.fhd_comp_buo_work ) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call cal_gravity_flux(coef_comp_buo, iphys%i_light,           &
     &        iphys%i_c_buo_gen)
        else if ( phys_nod_name(i).eq.fhd_filter_buo_work) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call cal_gravity_flux(coef_buo, iphys%i_filter_temp,          &
     &        iphys%i_f_buo_gen)
!
        else if (phys_nod_name(i).eq.fhd_temp_generation) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call cal_phys_product_4_scalar(iphys%i_par_t_gen,             &
     &        iphys%i_h_advect, iphys%i_temp)
        else if (phys_nod_name(i).eq.fhd_part_temp_gen ) then
          if(iflag_debug.gt.0)                                          &
     &             write(*,*) 'lead  ', trim(phys_nod_name(i))
          call cal_phys_product_4_scalar(iphys%i_par_t_gen,             &
     &        iphys%i_ph_advect, iphys%i_par_temp)
!
        else if (phys_nod_name(i).eq.fhd_vis_ene_diffuse ) then
          call cal_phys_dot_product(iphys%i_vis_e_diffuse,              &
     &         iphys%i_velo, iphys%i_v_diffuse)
        else if (phys_nod_name(i).eq.fhd_mag_ene_diffuse ) then
          call cal_phys_dot_product(iphys%i_mag_e_diffuse,              &
     &        iphys%i_magne, iphys%i_b_diffuse)
!
        else if (phys_nod_name(i).eq.fhd_mag_tension_work ) then
          call cal_phys_dot_product(iphys%i_m_tension_wk,               &
     &         iphys%i_velo, iphys%i_m_tension)
!
        else if (phys_nod_name(i).eq.fhd_poynting ) then
          call cal_phys_vector_product(iphys%i_poynting,                &
     &         iphys%i_electric, iphys%i_magne)
!
        end if
      end do
!
      end subroutine cal_work_4_forces
!
!-----------------------------------------------------------------------
!
      end module cal_MHD_forces_4_monitor

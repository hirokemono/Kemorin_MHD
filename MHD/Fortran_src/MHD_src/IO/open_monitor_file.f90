!
!      module open_monitor_file
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!      subroutine s_open_monitor_file(my_rank)
!      subroutine close_monitor_file(my_rank)
!
      module open_monitor_file
!
      use m_file_control_parameter
!
      use m_precision
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_open_monitor_file (my_rank)
!
      use m_node_phys_data
      use m_phys_labels
      use m_volume_average_labels
      use write_field_labels
!
      integer (kind=kint), intent(in) :: my_rank
      integer (kind=kint) :: i
!
!
      call set_vector_vol_average_labels
!
      if ( my_rank .eq. 0 ) then
        open (time_step_data_code,file = volume_ave_file_name,          &
     &      status='replace')
        open (rms_data_code,file = volume_rms_file_name,                &
     &      status='replace')
!
!
        call write_one_label(rms_data_code,       fhd_t_step)
        call write_one_label(time_step_data_code, fhd_t_step)
        call write_one_label(rms_data_code,       fhd_time)
        call write_one_label(time_step_data_code, fhd_time)
!
        do i = 1, num_nod_phys
          if (iflag_nod_fld_monitor(i) .eq. 1) then
            if ( phys_nod_name(i) .eq. fhd_velo ) then
              call write_two_labels(rms_data_code,                      &
     &            e_hd_k_ene, e_hd_div_v)
              call write_seven_labels(time_step_data_code,              &
     &            e_hd_vvec(1), e_hd_vvec(2), e_hd_vvec(3), e_hd_div_v, &
     &            e_hd_lvec(1), e_hd_lvec(2), e_hd_lvec(3) )
!
            else if ( phys_nod_name(i) .eq. fhd_magne) then
              call write_three_labels(rms_data_code,                    &
     &            e_hd_m_ene, e_hd_m_ene_cd, e_hd_div_b)
              call write_seven_labels(time_step_data_code,              &
     &            e_hd_bvec(1),    e_hd_bvec(2),    e_hd_bvec(3),       &
     &            e_hd_bvec_cd(1), e_hd_bvec_cd(2), e_hd_bvec_cd(3),    &
     &            e_hd_div_b )
!
            else if ( phys_nod_name(i) .eq. fhd_vecp) then
              call write_one_label(rms_data_code,       e_hd_div_a)
              call write_one_label(time_step_data_code, e_hd_div_a)
!
            else if ( phys_nod_name(i) .eq. fhd_vort) then
              call write_two_labels(rms_data_code,                      &
     &            e_hd_sq_w, e_hd_rms_w)
              call write_vector_label(time_step_data_code, e_hd_wvec)
!
            else if ( phys_nod_name(i) .eq. fhd_current) then
              call write_four_labels(rms_data_code,                     &
     &            e_hd_sq_j, e_hd_sq_j_cd, e_hd_rms_j, e_hd_rms_j_cd)
              call write_six_labels(time_step_data_code,                &
     &            e_hd_jvec(1),    e_hd_jvec(2),    e_hd_jvec(3),       &
     &            e_hd_jvec_cd(1), e_hd_jvec_cd(2), e_hd_jvec_cd(3) )
!
            else if ( phys_nod_name(i) .eq. fhd_e_field ) then
              call write_one_label(rms_data_code,          e_hd_rms_e)
              call write_vector_label(time_step_data_code, e_hd_evec)
!
            else if ( phys_nod_name(i) .eq. fhd_poynting ) then
              call write_one_label(rms_data_code, e_hd_rms_pflux)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_pflux_v)
!
            else if ( phys_nod_name(i) .eq. fhd_temp ) then
              call write_one_label(rms_data_code,       e_hd_temp)
              call write_one_label(time_step_data_code, e_hd_temp)
!
            else if ( phys_nod_name(i) .eq. fhd_part_temp) then
              call write_one_label(rms_data_code,       e_hd_part_temp)
              call write_one_label(time_step_data_code, e_hd_part_temp)
!
            else if ( phys_nod_name(i) .eq. fhd_light) then
              call write_one_label(rms_data_code,       fhd_light)
              call write_one_label(time_step_data_code, fhd_light)
!
            else if ( phys_nod_name(i) .eq. fhd_part_light) then
              call write_one_label(rms_data_code, fhd_part_light)
              call write_one_label(time_step_data_code, fhd_part_light)
!
            else if ( phys_nod_name(i) .eq. fhd_press ) then
              call write_one_label(rms_data_code,       e_hd_press)
              call write_one_label(time_step_data_code, e_hd_press)
!
            else if ( phys_nod_name(i) .eq. fhd_mag_potential ) then
              call write_one_label(rms_data_code,       e_hd_mag_p)
              call write_one_label(time_step_data_code, e_hd_mag_p)
!
            else if ( phys_nod_name(i) .eq. fhd_entropy) then
              call write_one_label(rms_data_code,       fhd_entropy)
              call write_one_label(time_step_data_code, fhd_entropy)
!
            else if ( phys_nod_name(i) .eq. fhd_per_entropy) then
              call write_one_label(rms_data_code,      fhd_per_entropy)
              call write_one_label(time_step_data_code,fhd_per_entropy)
!
            else if ( phys_nod_name(i) .eq. fhd_density) then
              call write_one_label(rms_data_code,       fhd_density)
              call write_one_label(time_step_data_code, fhd_density)
!
            else if ( phys_nod_name(i) .eq. fhd_per_density) then
              call write_one_label(rms_data_code,      fhd_per_density)
              call write_one_label(time_step_data_code,fhd_per_density)
!
!
            else if ( phys_nod_name(i) .eq. fhd_filter_v ) then
              call write_two_labels(rms_data_code,                      &
     &            e_hd_fil_k_ene, e_hd_fil_div_v)
              call write_seven_labels(time_step_data_code,              &
     &            e_hd_fil_vvec(1), e_hd_fil_vvec(2), e_hd_fil_vvec(3), &
     &            e_hd_fil_div_v, e_hd_fil_lvec(1),                     &
     &            e_hd_fil_lvec(2), e_hd_fil_lvec(3) )
!
            else if ( phys_nod_name(i) .eq. fhd_filter_b ) then
              call write_three_labels(rms_data_code,                    &
     &            e_hd_fil_m_ene, e_hd_fil_m_ene_cd, e_hd_fil_div_b)
              call write_seven_labels(time_step_data_code,              &
     &            e_hd_fil_bvec(1), e_hd_fil_bvec(2), e_hd_fil_bvec(3), &
     &            e_hd_fil_bvec_cd(1), e_hd_fil_bvec_cd(2),             &
     &            e_hd_fil_bvec_cd(3), e_hd_fil_div_b)
!
            else if ( phys_nod_name(i) .eq. fhd_filter_a ) then
              call write_one_label(rms_data_code,       e_hd_fil_div_a)
              call write_one_label(time_step_data_code, e_hd_fil_div_a)
!
            else if ( phys_nod_name(i) .eq. fhd_filter_temp ) then
              call write_one_label(rms_data_code,       fhd_filter_temp)
              call write_one_label(time_step_data_code, fhd_filter_temp)
!
            else if ( phys_nod_name(i) .eq. fhd_filter_comp ) then
              call write_one_label(rms_data_code,       fhd_filter_comp)
              call write_one_label(time_step_data_code, fhd_filter_comp)
!
!
            else if ( phys_nod_name(i) .eq. fhd_press_grad ) then
              call write_one_label(rms_data_code, fhd_press_grad)
              call write_vector_label(time_step_data_code,              &
      &            e_hd_press_grad_v)
!
            else if ( phys_nod_name(i) .eq. fhd_mag_tension ) then
              call write_one_label(rms_data_code, e_hd_mag_tension)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_mag_tension)
!
            else if ( phys_nod_name(i) .eq. fhd_inertia ) then
              call write_one_label(rms_data_code, fhd_inertia)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_inertia_v)
!
            else if ( phys_nod_name(i) .eq. fhd_div_m_flux ) then
              call write_one_label(rms_data_code, fhd_div_m_flux)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_div_m_flux_v)
!
            else if ( phys_nod_name(i) .eq. fhd_div_maxwell_t ) then
              call write_one_label(rms_data_code, e_hd_div_maxwell)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_div_maxwell_v)
!
            else if ( phys_nod_name(i) .eq. fhd_div_induct_t ) then
              call write_one_label(rms_data_code, fhd_div_induct_t)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_div_induct_v)
!
            else if ( phys_nod_name(i) .eq. fhd_mag_induct ) then
              call write_one_label(rms_data_code, e_hd_mag_induct)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_mag_induct_v)
!
            else if ( phys_nod_name(i) .eq. fhd_vp_induct ) then
              call write_one_label(rms_data_code, e_hd_vp_induct)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_vp_induct_v)
!
            else if ( phys_nod_name(i) .eq. fhd_vecp_diffuse ) then
              call write_one_label(rms_data_code, e_hd_vp_diffuse)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_vp_diffuse_v)
!
            else if ( phys_nod_name(i) .eq. fhd_mag_diffuse ) then
              call write_one_label(rms_data_code, e_hd_mag_diffuse)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_mag_diffuse_v)
!
            else if ( phys_nod_name(i) .eq. fhd_viscous ) then
              call write_one_label(rms_data_code, e_hd_vis_diffuse)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_vis_diffuse_v)
!
            else if ( phys_nod_name(i) .eq. fhd_Lorentz ) then
              call write_one_label(rms_data_code, e_hd_Lorentz)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_Lorentz_v)
!
            else if ( phys_nod_name(i) .eq. fhd_Coriolis ) then
              call write_one_label(rms_data_code, e_hd_Coriolis)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_Coriolis_v)
!
            else if ( phys_nod_name(i) .eq. fhd_buoyancy ) then
              call write_one_label(rms_data_code, fhd_buoyancy)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_buoyancy_v)
!
            else if ( phys_nod_name(i) .eq. fhd_comp_buo ) then
              call write_one_label(rms_data_code, fhd_comp_buo)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_comp_buo_v)
!
            else if ( phys_nod_name(i) .eq. fhd_filter_buo ) then
              call write_one_label(rms_data_code, fhd_filter_buo)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_filter_buo_v)
!
            else if ( phys_nod_name(i) .eq. fhd_ph_flux ) then
              call write_one_label(rms_data_code, fhd_ph_flux)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_ph_flux_v)
!
            else if ( phys_nod_name(i) .eq. fhd_h_flux ) then
              call write_one_label(rms_data_code, fhd_h_flux)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_h_flux_v)
!
            else if ( phys_nod_name(i) .eq. fhd_c_flux ) then
              call write_one_label(rms_data_code, fhd_c_flux)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_c_flux_v)
!
            else if ( phys_nod_name(i) .eq. fhd_mom_flux ) then
              call write_one_label(rms_data_code, fhd_mom_flux)
              call write_sym_tensor_label(time_step_data_code,          &
     &            e_hd_m_flux_st)
!
            else if ( phys_nod_name(i) .eq. fhd_maxwell_t ) then
              call write_one_label(rms_data_code, fhd_maxwell_t)
              call write_sym_tensor_label(time_step_data_code,          &
     &            e_hd_maxwell_st)
!
            else if ( phys_nod_name(i) .eq. fhd_induct_t ) then
              call write_one_label(rms_data_code, fhd_induct_t)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_induct_at)
!
            else if ( phys_nod_name(i) .eq. fhd_SGS_h_flux ) then
              call write_one_label(rms_data_code, fhd_SGS_h_flux)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_SGS_hf_v)
!
            else if ( phys_nod_name(i) .eq. fhd_SGS_c_flux ) then
              call write_one_label(rms_data_code, fhd_SGS_c_flux)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_SGS_cf_v)
!
            else if ( phys_nod_name(i) .eq. fhd_SGS_m_flux) then
              call write_one_label(rms_data_code, fhd_SGS_m_flux)
              call write_sym_tensor_label(time_step_data_code,          &
      &           e_hd_SGS_mf_st)
!
            else if ( phys_nod_name(i) .eq. fhd_SGS_maxwell_t ) then
              call write_one_label(rms_data_code, fhd_SGS_maxwell_t)
              call write_sym_tensor_label(time_step_data_code,          &
      &           e_hd_SGS_mxwl_st)
!
            else if ( phys_nod_name(i) .eq. fhd_SGS_induct_t ) then
              call write_one_label(rms_data_code, fhd_SGS_induct_t)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_SGS_idct_at)
!
            else if ( phys_nod_name(i) .eq. fhd_div_SGS_m_flux ) then
              call write_one_label(rms_data_code, e_hd_SGS_inertia)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_SGS_inertia_v)
!
            else if ( phys_nod_name(i) .eq. fhd_SGS_Lorentz ) then
              call write_one_label(rms_data_code, fhd_SGS_Lorentz)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_SGS_Lorentz_v)
!
            else if ( phys_nod_name(i) .eq. fhd_SGS_induction ) then
              call write_one_label(rms_data_code, e_hd_SGS_induct)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_SGS_induct_v)
!
            else if ( phys_nod_name(i) .eq. fhd_SGS_vp_induct ) then
              call write_one_label(rms_data_code, e_hd_SGS_vp_induct)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_SGS_vp_induct_v)
!
            else if ( phys_nod_name(i) .eq. fhd_SGS_buoyancy ) then
              call write_one_label(rms_data_code, fhd_SGS_buoyancy)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_SGS_buoyancy_v)
!
            else if ( phys_nod_name(i) .eq. fhd_SGS_comp_buo ) then
              call write_one_label(rms_data_code, fhd_SGS_comp_buo)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_SGS_comp_buo_v)
!
            else if ( phys_nod_name(i) .eq. fhd_mag_ene_gen ) then
              call write_one_label(rms_data_code, e_hd_ME_generate)
              call write_one_label(time_step_data_code,                 &
     &            e_hd_ME_generate)
            else if ( phys_nod_name(i) .eq. fhd_Lorentz_work ) then
              call write_one_label(rms_data_code, fhd_Lorentz_work)
              call write_one_label(time_step_data_code,                 &
     &            fhd_Lorentz_work)
            else if ( phys_nod_name(i) .eq. fhd_work_agst_Lorentz) then
              call write_one_label(rms_data_code,                       &
     &            fhd_work_agst_Lorentz)
              call write_one_label(time_step_data_code,                 &
     &            fhd_work_agst_Lorentz)
            else if ( phys_nod_name(i) .eq. fhd_mag_tension_work ) then
              call write_one_label(rms_data_code, fhd_mag_tension_work)
              call write_one_label(time_step_data_code,                 &
     &            fhd_mag_tension_work)
            else if ( phys_nod_name(i) .eq. fhd_buoyancy_flux ) then
              call write_one_label(rms_data_code, fhd_buoyancy_flux)
              call write_one_label(time_step_data_code,                 &
     &            fhd_buoyancy_flux)
            else if ( phys_nod_name(i) .eq. fhd_comp_buo_flux ) then
              call write_one_label(rms_data_code, fhd_comp_buo_flux)
              call write_one_label(time_step_data_code,                 &
     &            fhd_comp_buo_flux)
            else if ( phys_nod_name(i) .eq. fhd_filter_buo_flux ) then
              call write_one_label(rms_data_code, fhd_filter_buo_flux)
              call write_one_label(time_step_data_code,                 &
     &            fhd_filter_buo_flux)
            else if ( phys_nod_name(i) .eq. fhd_vis_ene_diffuse ) then
              call write_one_label(rms_data_code, fhd_vis_ene_diffuse)
              call write_one_label(time_step_data_code,                 &
     &            fhd_vis_ene_diffuse)
            else if ( phys_nod_name(i) .eq. fhd_mag_ene_diffuse ) then
              call write_one_label(rms_data_code, fhd_mag_ene_diffuse)
              call write_one_label(time_step_data_code,                 &
     &            fhd_mag_ene_diffuse)
!
            else if ( phys_nod_name(i) .eq. fhd_thermal_diffusion) then
              call write_one_label(rms_data_code,                       &
     &            fhd_thermal_diffusion)
              call write_one_label(time_step_data_code,                 &
     &            fhd_thermal_diffusion)
!
            else if ( phys_nod_name(i) .eq. fhd_heat_advect ) then
              call write_one_label(rms_data_code, fhd_heat_advect)
              call write_one_label(time_step_data_code,                 &
     &            fhd_heat_advect)
!
            else if ( phys_nod_name(i) .eq. fhd_part_h_advect ) then
              call write_one_label(rms_data_code, fhd_part_h_advect)
              call write_one_label(time_step_data_code,                 &
     &            fhd_part_h_advect)
!
            else if ( phys_nod_name(i) .eq. fhd_div_h_flux ) then
              call write_one_label(rms_data_code,       fhd_div_h_flux)
              call write_one_label(time_step_data_code, fhd_div_h_flux)
!
            else if ( phys_nod_name(i) .eq. fhd_div_ph_flux ) then
              call write_one_label(rms_data_code, fhd_div_ph_flux)
              call write_one_label(time_step_data_code,                 &
     &            fhd_div_ph_flux)
!
            else if ( phys_nod_name(i) .eq. fhd_temp_generation ) then
              call write_one_label(rms_data_code,       e_hd_temp_gen)
              call write_one_label(time_step_data_code, e_hd_temp_gen)
            else if ( phys_nod_name(i) .eq. fhd_part_temp_gen ) then
              call write_one_label(rms_data_code,                       &
     &            fhd_part_temp_gen)
              call write_one_label(time_step_data_code,                 &
     &            fhd_part_temp_gen)
            else if ( phys_nod_name(i) .eq. fhd_div_SGS_h_flux ) then
              call write_one_label(rms_data_code, fhd_div_SGS_h_flux)
              call write_one_label(time_step_data_code,                 &
     &            fhd_div_SGS_h_flux)
            else if ( phys_nod_name(i) .eq. fhd_SGS_temp_gen ) then
              call write_one_label(rms_data_code, fhd_SGS_temp_gen)
              call write_one_label(time_step_data_code,                 &
     &            fhd_SGS_temp_gen)
            else if ( phys_nod_name(i) .eq. fhd_SGS_m_ene_gen ) then
              call write_one_label(rms_data_code, e_hd_SGS_m_ene_gen)
              call write_one_label(time_step_data_code,                 &
     &            e_hd_SGS_m_ene_gen)
            else if ( phys_nod_name(i) .eq. fhd_SGS_Lorentz_work ) then
              call write_one_label(rms_data_code, fhd_SGS_Lorentz_work)
              call write_one_label(time_step_data_code,                 &
     &            fhd_SGS_Lorentz_work)
            else if ( phys_nod_name(i) .eq. fhd_Reynolds_work ) then
              call write_one_label(rms_data_code, fhd_Reynolds_work)
              call write_one_label(time_step_data_code,                 &
     &            fhd_Reynolds_work)
            else if ( phys_nod_name(i) .eq. fhd_SGS_buo_flux ) then
              call write_one_label(rms_data_code, fhd_SGS_buo_flux)
              call write_one_label(time_step_data_code,                 &
     &            fhd_SGS_buo_flux)
            else if ( phys_nod_name(i)                                  &
     &               .eq. fhd_SGS_comp_buo_flux ) then
              call write_one_label(rms_data_code,                       &
     &            fhd_SGS_comp_buo_flux)
              call write_one_label(time_step_data_code,                 &
     &            fhd_SGS_comp_buo_flux)
!
            else if ( phys_nod_name(i) .eq. fhd_SGS_div_h_flux_true )   &
     &             then
              call write_one_label(rms_data_code,                       &
     &            fhd_SGS_div_h_flux_true)
              call write_one_label(time_step_data_code,                 &
     &            fhd_SGS_div_h_flux_true)
            else if ( phys_nod_name(i) .eq. fhd_SGS_div_m_flux_true )   &
     &             then
              call write_one_label(rms_data_code,                       &
     &            fhd_SGS_div_m_flux_true)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_SGS_div_mf_true_v)
            else if ( phys_nod_name(i) .eq. fhd_SGS_Lorentz_true ) then
              call write_one_label(rms_data_code,                       &
     &            fhd_SGS_Lorentz_true)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_SGS_Lorentz_true_v)
            else if ( phys_nod_name(i) .eq. fhd_SGS_mag_induct_true )   &
     &             then
              call write_one_label(rms_data_code,                       &
     &            fhd_SGS_mag_induct_true)
              call write_vector_label(time_step_data_code,              &
     &            e_hd_SGS_m_induct_true_v)
!
            else if ( phys_nod_name(i) .eq. fhd_SGS_Lorentz_wk_true )   &
     &             then
              call write_one_label(rms_data_code,                       &
     &            fhd_SGS_Lorentz_wk_true)
              call write_one_label(time_step_data_code,                 &
     &            fhd_SGS_Lorentz_wk_true)
            else if ( phys_nod_name(i) .eq. fhd_Reynolds_work_true )    &
     &             then
              call write_one_label(rms_data_code,                       &
     &            fhd_Reynolds_work_true)
              call write_one_label(time_step_data_code,                 &
     &            fhd_Reynolds_work_true)
            else if ( phys_nod_name(i) .eq. fhd_SGS_temp_gen_true) then
              call write_one_label(rms_data_code,                       &
     &            fhd_SGS_temp_gen_true)
              call write_one_label(time_step_data_code,                 &
     &            fhd_SGS_temp_gen_true)
            else if ( phys_nod_name(i) .eq. fhd_SGS_m_ene_gen_true )    &
     &             then
              call write_one_label(rms_data_code,                       &
     &            fhd_SGS_m_ene_gen_true)
              call write_one_label(time_step_data_code,                 &
     &            fhd_SGS_m_ene_gen_true)
!
!    Old field label... Shold be deleted...
            else if ( phys_nod_name(i) .eq. fhd_buoyancy_work ) then
              call write_one_label(rms_data_code, fhd_buoyancy_work)
              call write_one_label(time_step_data_code,                 &
     &            fhd_buoyancy_work)

            end if
!
          else
!
            if ( phys_nod_name(i) .eq. fhd_velo) then
              call write_one_label(rms_data_code,       e_hd_div_v)
              call write_one_label(time_step_data_code, e_hd_div_v)
            else if ( phys_nod_name(i) .eq. fhd_magne) then
              call write_one_label(rms_data_code,       e_hd_div_b)
              call write_one_label(time_step_data_code, e_hd_div_b)
            else if ( phys_nod_name(i) .eq. fhd_vecp) then
              call write_one_label(rms_data_code,       e_hd_div_a)
              call write_one_label(time_step_data_code, e_hd_div_a)
!
            else if ( phys_nod_name(i) .eq. fhd_filter_v) then
              call write_one_label(rms_data_code,       e_hd_fil_div_v)
              call write_one_label(time_step_data_code, e_hd_fil_div_v)
            else if ( phys_nod_name(i) .eq. fhd_filter_b) then
              call write_one_label(rms_data_code,       e_hd_fil_div_b)
              call write_one_label(time_step_data_code, e_hd_fil_div_b)
            else if ( phys_nod_name(i) .eq. fhd_filter_a) then
              call write_one_label(rms_data_code,       e_hd_fil_div_a)
              call write_one_label(time_step_data_code, e_hd_fil_div_a)
            end if
!
          end if
        end do
!
        call write_one_label(rms_data_code, e_hd_volume)
!
        write(rms_data_code,*)      
        write(time_step_data_code,*)
      end if
!
      end subroutine s_open_monitor_file
!
! ----------------------------------------------------------------------
!
      subroutine close_monitor_file (my_rank)
!
      integer(kind=kint), intent(in) :: my_rank
!
!
      if ( my_rank .eq. 0 ) then
        close (time_step_data_code)
        close (rms_data_code)
      end if
!
      end subroutine close_monitor_file
!
! ----------------------------------------------------------------------
!
      end module open_monitor_file

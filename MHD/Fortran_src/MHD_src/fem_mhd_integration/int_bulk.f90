!
!      module int_bulk
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!      subroutine s_int_bulk
!
      module int_bulk
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
      subroutine s_int_bulk
!
      use m_constants
      use calypso_mpi
      use m_control_parameter
      use m_geometry_data
      use m_geometry_data_MHD
      use m_finite_element_matrix
      use m_fem_gauss_int_coefs
      use m_int_vol_data
      use m_phys_labels
      use m_node_phys_address
      use m_bulk_values
      use int_all_energy
      use int_vol_angular_momentum
      use int_all_rms_scalar
      use int_rms_ave_sym_tensor
      use int_rms_ave_asym_tensor
!
!
! ---------  initialize
!
      bulk_local(1:num_bulk) = 0.0d0
      rms_local(1:num_rms-1)  = 0.0d0
!
! ----- lead average in a element -------------
!
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_press, j_ave%i_press, iphys%i_press)
      call int_all_4_scalar (ele1%istack_ele_smp, intg_point_t_evo,     &
     &    i_rms%i_mag_p, j_ave%i_mag_p, iphys%i_mag_p)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_temp, j_ave%i_temp, iphys%i_temp)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_par_temp, j_ave%i_par_temp, iphys%i_par_temp )
!
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_light, j_ave%i_light, iphys%i_light )
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_par_light, j_ave%i_par_light, iphys%i_par_light)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_entropy, j_ave%i_entropy, iphys%i_entropy)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_par_entropy, j_ave%i_par_entropy,                     &
     &    iphys%i_par_entropy)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_density, j_ave%i_density, iphys%i_density)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_par_density, j_ave%i_par_density,                     &
     &    iphys%i_par_density)
!
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_heat_source, j_ave%i_heat_source,                     &
     &    iphys%i_heat_source)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_light_source, j_ave%i_light_source,                   &
     &    iphys%i_light_source)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_entropy_source, j_ave%i_entropy_source,               &
     &    iphys%i_entropy_source)
!
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_filter_temp, j_ave%i_filter_temp,                     &
     &    iphys%i_filter_temp)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_filter_comp, j_ave%i_filter_comp,                     &
     &    iphys%i_filter_comp)
!
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_me_gen, j_ave%i_me_gen, iphys%i_me_gen)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_ujb, j_ave%i_ujb, iphys%i_ujb)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_nega_ujb, j_ave%i_nega_ujb, iphys%i_nega_ujb)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_m_tension_wk, j_ave%i_m_tension_wk,                   &
     &    iphys%i_m_tension_wk)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_buo_gen, j_ave%i_buo_gen, iphys%i_buo_gen)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_c_buo_gen, j_ave%i_c_buo_gen, iphys%i_c_buo_gen)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_f_buo_gen, j_ave%i_f_buo_gen, iphys%i_f_buo_gen)
!
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_vis_e_diffuse, j_ave%i_vis_e_diffuse,                 &
     &    iphys%i_vis_e_diffuse)
      call int_all_4_scalar (iele_cd_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_mag_e_diffuse, j_ave%i_mag_e_diffuse,                 &
     &    iphys%i_mag_e_diffuse)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_h_advect, j_ave%i_h_advect, iphys%i_h_advect)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_ph_advect, j_ave%i_ph_advect, iphys%i_ph_advect)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_t_diffuse, j_ave%i_t_diffuse, iphys%i_t_diffuse)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_c_diffuse, j_ave%i_c_diffuse, iphys%i_c_diffuse)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_h_flux_div, j_ave%i_h_flux_div, iphys%i_h_flux_div)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_ph_flux_div, j_ave%i_ph_flux_div,                     &
     &    iphys%i_ph_flux_div)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_temp_gen, j_ave%i_temp_gen, iphys%i_temp_gen)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_par_t_gen, j_ave%i_par_t_gen, iphys%i_par_t_gen)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_SGS_div_h_flux, j_ave%i_SGS_div_h_flux,               &
     &    iphys%i_SGS_div_h_flux)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_SGS_temp_gen, j_ave%i_SGS_temp_gen,                   &
     &    iphys%i_SGS_temp_gen)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_SGS_me_gen, j_ave%i_SGS_me_gen, iphys%i_SGS_me_gen)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_SGS_Lor_wk, j_ave%i_SGS_Lor_wk, iphys%i_SGS_lor_wk)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_reynolds_wk, j_ave%i_reynolds_wk,                     &
     &    iphys%i_reynolds_wk)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_SGS_div_hf_true, j_ave%i_SGS_div_hf_true,             &
     &    iphys%i_SGS_div_hf_true)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_SGS_buo_wk, j_ave%i_SGS_buo_wk, iphys%i_SGS_buo_wk)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_SGS_comp_buo_wk, j_ave%i_SGS_comp_buo_wk,             &
     &    iphys%i_SGS_comp_buo_wk)
!
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_SGS_Lor_wk_tr, j_ave%i_SGS_Lor_wk_tr,                 &
     &    iphys%i_SGS_Lor_wk_tr)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_reynolds_wk_tr, j_ave%i_reynolds_wk_tr,               &
     &    iphys%i_reynolds_wk_tr)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_SGS_t_gen_tr, j_ave%i_SGS_t_gen_tr,                   &
     &    iphys%i_SGS_t_gen_tr)
      call int_all_4_scalar (iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_SGS_me_gen_tr, j_ave%i_SGS_me_gen_tr,                 &
     &    iphys%i_SGS_me_gen_tr)
!
! ----- lead energy and angular momentum for vector data-------------
!
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_velo, j_ave%i_velo, iphys%i_velo)
      call int_all_angular_mom(iele_fl_smp_stack, intg_point_t_evo,     &
     &    ja_amom, iphys%i_velo)
!
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_filter_velo, j_ave%i_filter_velo,                     &
     &    iphys%i_filter_velo)
      call int_all_angular_mom(iele_fl_smp_stack, intg_point_t_evo,     &
     &    jr_amom_f, iphys%i_filter_velo)
!
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_magne, j_ave%i_magne, iphys%i_magne)
      call int_all_4_vector( iele_cd_smp_stack, intg_point_t_evo,       &
     &    ir_me_ic, ja_mag_ic, iphys%i_magne) 
!
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_vort, j_ave%i_vort, iphys%i_vort)
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_current, j_ave%i_current, iphys%i_current)
      call int_all_4_vector( iele_cd_smp_stack, intg_point_t_evo,       &
     &    ir_sqj_ic, ja_j_ic, iphys%i_current) 
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_filter_magne, j_ave%i_filter_magne,                   &
     &    iphys%i_filter_magne)
      call int_all_4_vector( iele_cd_smp_stack, intg_point_t_evo,       &
     &    ir_me_f_ic, ja_mag_f_ic, iphys%i_filter_magne) 
!
      if(i_rms%i_velo .gt. 0) then
        rms_local(i_rms%i_velo) = half * rms_local(i_rms%i_velo)
      end if
      if(i_rms%i_magne .gt. 0) then
        rms_local(i_rms%i_magne)    = half * rms_local(i_rms%i_magne)
      end if
      if(ir_me_ic .gt. 0) then
        rms_local(ir_me_ic) = half * rms_local(ir_me_ic)
      end if
!
      if(i_rms%i_filter_velo .gt. 0) then
        rms_local(i_rms%i_filter_velo)                                  &
     &      = half * rms_local(i_rms%i_filter_velo)
      end if
      if(i_rms%i_filter_magne .gt. 0) then
        rms_local(i_rms%i_filter_magne   )                              &
     &      = half * rms_local(i_rms%i_filter_magne   )
      end if
      if(ir_me_f_ic .gt. 0) then
        rms_local(ir_me_f_ic) = half * rms_local(ir_me_f_ic)
      end if
!
      if(ir_rms_w .gt. 0) then
        rms_local(ir_rms_w) = rms_local(i_rms%i_vort)
      end if
      if(ir_rms_j .gt. 0) then
        rms_local(ir_rms_j)    =  rms_local(i_rms%i_current)
      end if
      if(ir_rms_j_ic .gt. 0) then
        rms_local(ir_rms_j_ic) =  rms_local(ir_sqj_ic)
      end if
!
      call int_all_4_vector( iele_cd_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_electric, j_ave%i_electric, iphys%i_electric)
      call int_all_4_vector( iele_cd_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_poynting, j_ave%i_poynting, iphys%i_poynting)
!
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_m_tension, j_ave%i_m_tension, iphys%i_m_tension)
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_h_flux, j_ave%i_h_flux, iphys%i_h_flux)
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_ph_flux, j_ave%i_ph_flux, iphys%i_ph_flux)
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_c_flux, j_ave%i_c_flux, iphys%i_c_flux)
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_m_advect, j_ave%i_m_advect, iphys%i_m_advect)
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_m_flux_div, j_ave%i_m_flux_div, iphys%i_m_flux_div)
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_maxwell_div, j_ave%i_maxwell_div,                     &
     &    iphys%i_maxwell_div)
      call int_all_4_vector( iele_cd_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_induct_div, j_ave%i_induct_div, iphys%i_induct_div)
      call int_all_4_vector( iele_cd_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_induction, j_ave%i_induction, iphys%i_induction)
      call int_all_4_vector( iele_cd_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_vp_induct, j_ave%i_vp_induct, iphys%i_vp_induct)
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_mag_stretch, j_ave%i_mag_stretch,                     &
     &    iphys%i_mag_stretch)
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_lorentz, j_ave%i_lorentz, iphys%i_lorentz)
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_coriolis, j_ave%i_coriolis, iphys%i_coriolis)
!
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_buoyancy, j_ave%i_buoyancy, iphys%i_buoyancy)
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_comp_buo, j_ave%i_comp_buo, iphys%i_comp_buo)
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_filter_buo, j_ave%i_filter_buo, iphys%i_filter_buo)
!
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_v_diffuse, j_ave%i_v_diffuse, iphys%i_v_diffuse)
      call int_all_4_vector( iele_cd_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_vp_diffuse, j_ave%i_vp_diffuse, iphys%i_vp_diffuse)
      call int_all_4_vector( iele_cd_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_b_diffuse, j_ave%i_b_diffuse, iphys%i_b_diffuse)
!
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_grad_vx, j_ave%i_grad_vx, iphys%i_grad_vx)
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_grad_vy, j_ave%i_grad_vy, iphys%i_grad_vy)
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_grad_vz, j_ave%i_grad_vz, iphys%i_grad_vz)
!
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_SGS_h_flux, j_ave%i_SGS_h_flux, iphys%i_SGS_h_flux)
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_SGS_c_flux, j_ave%i_SGS_c_flux, iphys%i_SGS_c_flux)
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_SGS_div_m_flux, j_ave%i_SGS_div_m_flux,               &
     &    iphys%i_SGS_div_m_flux)
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_SGS_Lorentz, j_ave%i_SGS_Lorentz,                     &
     &    iphys%i_SGS_Lorentz)
      call int_all_4_vector( iele_cd_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_SGS_induction, j_ave%i_SGS_induction,                 &
     &    iphys%i_SGS_induction)
      call int_all_4_vector( iele_cd_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_SGS_vp_induct, j_ave%i_SGS_vp_induct,                 &
     &    iphys%i_SGS_vp_induct)
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_SGS_buoyancy, j_ave%i_SGS_buoyancy,                   &
     &    iphys%i_SGS_buoyancy)
      call int_all_4_vector( iele_fl_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_SGS_comp_buo, j_ave%i_SGS_comp_buo,                   &
     &    iphys%i_SGS_comp_buo)
!
      call int_all_4_vector(iele_fl_smp_stack, intg_point_t_evo,        &
     &    i_rms%i_SGS_div_mf_true, j_ave%i_SGS_div_mf_true,             &
     &    iphys%i_SGS_div_mf_true)
      call int_all_4_vector(iele_fl_smp_stack, intg_point_t_evo,        &
     &    i_rms%i_SGS_Lor_true, j_ave%i_SGS_Lor_true,                   &
     &    iphys%i_SGS_Lor_true)
      call int_all_4_vector( iele_cd_smp_stack, intg_point_t_evo,       &
     &    i_rms%i_SGS_idct_true, j_ave%i_SGS_idct_true,                 &
     &    iphys%i_SGS_idct_true)
!
!
! ----- lead average values for symmetric tensor data-------------
!
      call int_ave_4_sym_tensor( iele_fl_smp_stack, intg_point_t_evo,   &
     &    i_rms%i_m_flux, j_ave%i_m_flux, iphys%i_m_flux)
      call int_ave_4_sym_tensor( iele_fl_smp_stack, intg_point_t_evo,   &
     &    i_rms%i_maxwell, j_ave%i_maxwell, iphys%i_maxwell)
      call int_ave_4_sym_tensor( iele_fl_smp_stack, intg_point_t_evo,   &
     &    i_rms%i_SGS_m_flux, j_ave%i_SGS_m_flux, iphys%i_SGS_m_flux)
      call int_ave_4_sym_tensor( iele_fl_smp_stack, intg_point_t_evo,   &
     &    i_rms%i_SGS_maxwell, j_ave%i_SGS_maxwell,                     &
     &    iphys%i_SGS_maxwell)
!
      call int_ave_4_asym_tensor( iele_cd_smp_stack, intg_point_t_evo,  &
     &    i_rms%i_induct_t, j_ave%i_induct_t, iphys%i_induct_t)
      call int_ave_4_asym_tensor( iele_cd_smp_stack, intg_point_t_evo,  &
     &    i_rms%i_SGS_induct_t, j_ave%i_SGS_induct_t,                   &
     &    iphys%i_SGS_induct_t)
!
      end subroutine s_int_bulk
!
! ----------------------------------------------------------------------
!
      end module int_bulk

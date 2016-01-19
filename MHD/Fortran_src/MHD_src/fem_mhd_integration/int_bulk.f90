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
      use t_geometry_data
      use t_phys_data
      use t_phys_address
      use t_jacobian_3d
      use t_finite_element_mat
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_int_mean_squares(node, ele, iphys, nod_fld,          &
     &          jac_3d_q, jac_3d_l, fem_wk, mhd_fem_wk)
!
      use m_constants
      use calypso_mpi
      use m_control_parameter
      use m_geometry_data_MHD
      use m_fem_gauss_int_coefs
      use m_phys_labels
      use m_mean_square_values
      use int_all_energy
      use int_all_ave_tensors
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_3d), intent(in) :: jac_3d_q, jac_3d_l
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!
! ---------  initialize
!
      bulk_local(1:num_bulk) = 0.0d0
      rms_local(1:num_rms-1)  = 0.0d0
!
! ----- lead average in a element -------------
!
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_press, j_ave%i_press, iphys%i_press,                  &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar (ele%istack_ele_smp, intg_point_t_evo,      &
     &    i_rms%i_mag_p, j_ave%i_mag_p, iphys%i_mag_p,                  &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_temp, j_ave%i_temp, iphys%i_temp,                     &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_par_temp, j_ave%i_par_temp, iphys%i_par_temp,         &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
!
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_light, j_ave%i_light, iphys%i_light,                  &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_par_light, j_ave%i_par_light, iphys%i_par_light,      &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_entropy, j_ave%i_entropy, iphys%i_entropy,            &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_par_entropy, j_ave%i_par_entropy,                     &
     &    iphys%i_par_entropy,                                          &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_density, j_ave%i_density, iphys%i_density,            &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_par_density, j_ave%i_par_density,                     &
     &    iphys%i_par_density,                                          &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
!
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_heat_source, j_ave%i_heat_source,                     &
     &    iphys%i_heat_source,                                          &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_light_source, j_ave%i_light_source,                   &
     &    iphys%i_light_source,                                         &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_entropy_source, j_ave%i_entropy_source,               &
     &    iphys%i_entropy_source,                                       &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
!
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_filter_temp, j_ave%i_filter_temp,                     &
     &    iphys%i_filter_temp,                                          &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_filter_comp, j_ave%i_filter_comp,                     &
     &    iphys%i_filter_comp,                                          &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
!
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_me_gen, j_ave%i_me_gen, iphys%i_me_gen,               &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_ujb, j_ave%i_ujb, iphys%i_ujb,                        &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_nega_ujb, j_ave%i_nega_ujb, iphys%i_nega_ujb,         &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_m_tension_wk, j_ave%i_m_tension_wk,                   &
     &    iphys%i_m_tension_wk,                                         &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_buo_gen, j_ave%i_buo_gen, iphys%i_buo_gen,            &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_c_buo_gen, j_ave%i_c_buo_gen, iphys%i_c_buo_gen,      &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_f_buo_gen, j_ave%i_f_buo_gen, iphys%i_f_buo_gen,      &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
!
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_vis_e_diffuse, j_ave%i_vis_e_diffuse,                 &
     &    iphys%i_vis_e_diffuse,                                        &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (conduct1%istack_ele_fld_smp, intg_point_t_evo,                &
     &    i_rms%i_mag_e_diffuse, j_ave%i_mag_e_diffuse,                 &
     &    iphys%i_mag_e_diffuse,                                        &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_h_advect, j_ave%i_h_advect, iphys%i_h_advect,         &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_ph_advect, j_ave%i_ph_advect, iphys%i_ph_advect,      &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_t_diffuse, j_ave%i_t_diffuse, iphys%i_t_diffuse,      &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_c_diffuse, j_ave%i_c_diffuse, iphys%i_c_diffuse,      &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_h_flux_div, j_ave%i_h_flux_div, iphys%i_h_flux_div,   &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_ph_flux_div, j_ave%i_ph_flux_div,                     &
     &    iphys%i_ph_flux_div,                                          &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_temp_gen, j_ave%i_temp_gen, iphys%i_temp_gen,         &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_par_t_gen, j_ave%i_par_t_gen, iphys%i_par_t_gen,      &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_SGS_div_h_flux, j_ave%i_SGS_div_h_flux,               &
     &    iphys%i_SGS_div_h_flux,                                       &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_SGS_temp_gen, j_ave%i_SGS_temp_gen,                   &
     &    iphys%i_SGS_temp_gen,                                         &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_SGS_me_gen, j_ave%i_SGS_me_gen, iphys%i_SGS_me_gen,   &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_SGS_Lor_wk, j_ave%i_SGS_Lor_wk, iphys%i_SGS_lor_wk,   &
     
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_reynolds_wk, j_ave%i_reynolds_wk,                     &
     &    iphys%i_reynolds_wk,                                          &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_SGS_div_hf_true, j_ave%i_SGS_div_hf_true,             &
     &    iphys%i_SGS_div_hf_true,                                      &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_SGS_buo_wk, j_ave%i_SGS_buo_wk, iphys%i_SGS_buo_wk,   &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_SGS_comp_buo_wk, j_ave%i_SGS_comp_buo_wk,             &
     &    iphys%i_SGS_comp_buo_wk,                                      &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
!
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_SGS_Lor_wk_tr, j_ave%i_SGS_Lor_wk_tr,                 &
     &    iphys%i_SGS_Lor_wk_tr,                                        &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_reynolds_wk_tr, j_ave%i_reynolds_wk_tr,               &
     &    iphys%i_reynolds_wk_tr,                                       &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_SGS_t_gen_tr, j_ave%i_SGS_t_gen_tr,                   &
     &    iphys%i_SGS_t_gen_tr,                                         &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_scalar                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_SGS_me_gen_tr, j_ave%i_SGS_me_gen_tr,                 &
     &    iphys%i_SGS_me_gen_tr,                                        &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
!
! ----- lead energy and angular momentum for vector data-------------
!
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_velo, j_ave%i_velo, iphys%i_velo,                     &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_angular_mom(fluid1%istack_ele_fld_smp,               &
     &    intg_point_t_evo, ja_amom, iphys%i_velo,                      &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, mhd_fem_wk, fem_wk)
!
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_filter_velo, j_ave%i_filter_velo,                     &
     &    iphys%i_filter_velo,                                          &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_angular_mom(fluid1%istack_ele_fld_smp,               &
     &    intg_point_t_evo, jr_amom_f, iphys%i_filter_velo,             &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, mhd_fem_wk, fem_wk)
!
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_magne, j_ave%i_magne, iphys%i_magne,                  &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (conduct1%istack_ele_fld_smp, intg_point_t_evo,                &
     &    ir_me_ic, ja_mag_ic, iphys%i_magne,                           &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
!
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_vort, j_ave%i_vort, iphys%i_vort,                     &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_current, j_ave%i_current, iphys%i_current,            &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (conduct1%istack_ele_fld_smp, intg_point_t_evo,                &
     &    ir_sqj_ic, ja_j_ic, iphys%i_current,                          &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_filter_magne, j_ave%i_filter_magne,                   &
     &    iphys%i_filter_magne,                                         &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (conduct1%istack_ele_fld_smp, intg_point_t_evo,                &
     &    ir_me_f_ic, ja_mag_f_ic, iphys%i_filter_magne,                &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
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
      call int_all_4_vector                                             &
     &   (conduct1%istack_ele_fld_smp, intg_point_t_evo,                &
     &    i_rms%i_electric, j_ave%i_electric, iphys%i_electric,         &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (conduct1%istack_ele_fld_smp, intg_point_t_evo,                &
     &    i_rms%i_poynting, j_ave%i_poynting, iphys%i_poynting,         &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
!
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_m_tension, j_ave%i_m_tension, iphys%i_m_tension,      &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_h_flux, j_ave%i_h_flux, iphys%i_h_flux,               &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_ph_flux, j_ave%i_ph_flux, iphys%i_ph_flux,            &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_c_flux, j_ave%i_c_flux, iphys%i_c_flux,               &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_m_advect, j_ave%i_m_advect, iphys%i_m_advect,         &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_m_flux_div, j_ave%i_m_flux_div, iphys%i_m_flux_div,   &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_maxwell_div, j_ave%i_maxwell_div,                     &
     &    iphys%i_maxwell_div,                                          &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (conduct1%istack_ele_fld_smp, intg_point_t_evo,                &
     &    i_rms%i_induct_div, j_ave%i_induct_div, iphys%i_induct_div,   &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (conduct1%istack_ele_fld_smp, intg_point_t_evo,                &
     &    i_rms%i_induction, j_ave%i_induction, iphys%i_induction,      &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (conduct1%istack_ele_fld_smp, intg_point_t_evo,                &
     &    i_rms%i_vp_induct, j_ave%i_vp_induct, iphys%i_vp_induct,      &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_mag_stretch, j_ave%i_mag_stretch,                     &
     &    iphys%i_mag_stretch,                                          &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_lorentz, j_ave%i_lorentz, iphys%i_lorentz,            &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_coriolis, j_ave%i_coriolis, iphys%i_coriolis,         &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
!
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_buoyancy, j_ave%i_buoyancy, iphys%i_buoyancy,         &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_comp_buo, j_ave%i_comp_buo, iphys%i_comp_buo,         &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_filter_buo, j_ave%i_filter_buo, iphys%i_filter_buo,   &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
!
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_v_diffuse, j_ave%i_v_diffuse, iphys%i_v_diffuse,      &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (conduct1%istack_ele_fld_smp, intg_point_t_evo,                &
     &    i_rms%i_vp_diffuse, j_ave%i_vp_diffuse, iphys%i_vp_diffuse,   &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (conduct1%istack_ele_fld_smp, intg_point_t_evo,                &
     &    i_rms%i_b_diffuse, j_ave%i_b_diffuse, iphys%i_b_diffuse,      &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
!
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_grad_vx, j_ave%i_grad_vx, iphys%i_grad_vx,            &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_grad_vy, j_ave%i_grad_vy, iphys%i_grad_vy,            &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_grad_vz, j_ave%i_grad_vz, iphys%i_grad_vz,            &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
!
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_SGS_h_flux, j_ave%i_SGS_h_flux, iphys%i_SGS_h_flux,   &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_SGS_c_flux, j_ave%i_SGS_c_flux, iphys%i_SGS_c_flux,   &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_SGS_div_m_flux, j_ave%i_SGS_div_m_flux,               &
     &    iphys%i_SGS_div_m_flux,                                       &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_SGS_Lorentz, j_ave%i_SGS_Lorentz,                     &
     &    iphys%i_SGS_Lorentz,                                          &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (conduct1%istack_ele_fld_smp, intg_point_t_evo,                &
     &    i_rms%i_SGS_induction, j_ave%i_SGS_induction,                 &
     &    iphys%i_SGS_induction,                                        &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (conduct1%istack_ele_fld_smp, intg_point_t_evo,                &
     &    i_rms%i_SGS_vp_induct, j_ave%i_SGS_vp_induct,                 &
     &    iphys%i_SGS_vp_induct,                                        &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_SGS_buoyancy, j_ave%i_SGS_buoyancy,                   &
     &    iphys%i_SGS_buoyancy,                                         &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_SGS_comp_buo, j_ave%i_SGS_comp_buo,                   &
     &    iphys%i_SGS_comp_buo,                                         &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
!
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_SGS_div_mf_true, j_ave%i_SGS_div_mf_true,             &
     &    iphys%i_SGS_div_mf_true,                                      &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_SGS_Lor_true, j_ave%i_SGS_Lor_true,                   &
     &    iphys%i_SGS_Lor_true,                                         &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_vector                                             &
     &   (conduct1%istack_ele_fld_smp, intg_point_t_evo,                &
     &    i_rms%i_SGS_idct_true, j_ave%i_SGS_idct_true,                 &
     &    iphys%i_SGS_idct_true,                                        &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
!
!
! ----- lead average values for symmetric tensor data-------------
!
      call int_all_4_sym_tensor                                         &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_m_flux, j_ave%i_m_flux, iphys%i_m_flux,               &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_sym_tensor                                         &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_maxwell, j_ave%i_maxwell, iphys%i_maxwell,            &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_sym_tensor                                         &
     &   (fluid1%istack_ele_fld_smp, intg_point_t_evo,                  &
     &    i_rms%i_SGS_m_flux, j_ave%i_SGS_m_flux, iphys%i_SGS_m_flux,   &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_sym_tensor(fluid1%istack_ele_fld_smp,              &
     &    intg_point_t_evo, i_rms%i_SGS_maxwell,                        &
     &    j_ave%i_SGS_maxwell, iphys%i_SGS_maxwell,                     &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
!
      call int_all_4_asym_tensor                                        &
     &   (conduct1%istack_ele_fld_smp, intg_point_t_evo,                &
     &    i_rms%i_induct_t, j_ave%i_induct_t, iphys%i_induct_t,         &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
      call int_all_4_asym_tensor(conduct1%istack_ele_fld_smp,           &
     &    intg_point_t_evo, i_rms%i_SGS_induct_t,                       &
     &    j_ave%i_SGS_induct_t, iphys%i_SGS_induct_t,                   &
     &    node, ele, nod_fld, jac_3d_q, jac_3d_l, fem_wk)
!
      end subroutine s_int_mean_squares
!
! ----------------------------------------------------------------------
!
      end module int_bulk

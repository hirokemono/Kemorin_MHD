!
!      module int_bulk
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!      Modified by H. Matsui on Aug, 2007
!
!!      subroutine s_int_mean_squares(npoint_integrate,                 &
!!     &          mesh, fluid, conduct, iphys, nod_fld, jacs,           &
!!     &          i_rms, j_ave, ifld_msq, fem_wk, mhd_fem_wk, fem_msq)
!!      subroutine int_no_evo_mean_squares(i_step, dt, mesh,            &
!!     &          fl_prop, cd_prop, iphys, nod_fld, iphys_ele, ele_fld, &
!!     &          fluid, jacs, i_rms, j_ave, fem_wk, fem_msq)
!!        type(mesh_geometry), intent(in) :: mesh
!!        type(field_geometry_data), intent(in) :: fluid, conduct
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in) :: cd_prop
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(in) :: nod_fld
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_type), intent(in) :: jacs
!!        type(phys_address), intent(in) :: i_rms, j_ave
!!        type(mean_square_address), intent(in) :: ifld_msq
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
!!        type(mean_square_values), intent(inout) :: fem_msq
!
      module int_bulk
!
      use m_precision
!
      use t_physical_property
      use t_geometry_data_MHD
      use t_mesh_data
      use t_phys_data
      use t_phys_address
      use t_jacobians
      use t_finite_element_mat
      use t_MHD_finite_element_mat
      use t_mean_square_values
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_int_mean_squares(npoint_integrate,                   &
     &          mesh, fluid, conduct, iphys, nod_fld, jacs,             &
     &          i_rms, j_ave, ifld_msq, fem_wk, mhd_fem_wk, fem_msq)
!
      use m_constants
      use calypso_mpi
      use m_phys_labels
      use int_all_energy
      use int_all_ave_tensors
!
      integer(kind = kint), intent(in) :: npoint_integrate
      type(mesh_geometry), intent(in) :: mesh
      type(field_geometry_data), intent(in) :: fluid, conduct
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(jacobians_type), intent(in) :: jacs
      type(phys_address), intent(in) :: i_rms, j_ave
      type(mean_square_address), intent(in) :: ifld_msq
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_MHD_fe_mat), intent(inout) :: mhd_fem_wk
      type(mean_square_values), intent(inout) :: fem_msq
!
! ---------  initialize
!
      fem_msq%ave_local(1:fem_msq%num_ave) = 0.0d0
      fem_msq%rms_local(1:fem_msq%num_rms-1)  = 0.0d0
!
! ----- lead average in a element -------------
!
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_press, j_ave%i_press, iphys%i_press,                  &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar(mesh%ele%istack_ele_smp, npoint_integrate,  &
     &    i_rms%i_mag_p, j_ave%i_mag_p, iphys%i_mag_p,                  &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_temp, j_ave%i_temp, iphys%i_temp,                     &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_par_temp, j_ave%i_par_temp, iphys%i_par_temp,         &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
!
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_light, j_ave%i_light, iphys%i_light,                  &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_par_light, j_ave%i_par_light, iphys%i_par_light,      &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_entropy, j_ave%i_entropy, iphys%i_entropy,            &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_par_entropy, j_ave%i_par_entropy,                     &
     &    iphys%i_par_entropy,                                          &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_density, j_ave%i_density, iphys%i_density,            &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_par_density, j_ave%i_par_density,                     &
     &    iphys%i_par_density,                                          &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
!
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_heat_source, j_ave%i_heat_source,                     &
     &    iphys%i_heat_source,                                          &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_light_source, j_ave%i_light_source,                   &
     &    iphys%i_light_source,                                         &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_entropy_source, j_ave%i_entropy_source,               &
     &    iphys%i_entropy_source,                                       &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
!
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_filter_temp, j_ave%i_filter_temp,                     &
     &    iphys%i_filter_temp,                                          &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_filter_comp, j_ave%i_filter_comp,                     &
     &    iphys%i_filter_comp,                                          &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
!
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_me_gen, j_ave%i_me_gen, iphys%i_me_gen,               &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_ujb, j_ave%i_ujb, iphys%i_ujb,                        &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_nega_ujb, j_ave%i_nega_ujb, iphys%i_nega_ujb,         &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_m_tension_wk, j_ave%i_m_tension_wk,                   &
     &    iphys%i_m_tension_wk,                                         &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_buo_gen, j_ave%i_buo_gen, iphys%i_buo_gen,            &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_c_buo_gen, j_ave%i_c_buo_gen, iphys%i_c_buo_gen,      &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_f_buo_gen, j_ave%i_f_buo_gen, iphys%i_f_buo_gen,      &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
!
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_vis_e_diffuse, j_ave%i_vis_e_diffuse,                 &
     &    iphys%i_vis_e_diffuse,                                        &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (conduct%istack_ele_fld_smp, npoint_integrate,                 &
     &    i_rms%i_mag_e_diffuse, j_ave%i_mag_e_diffuse,                 &
     &    iphys%i_mag_e_diffuse,                                        &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_h_advect, j_ave%i_h_advect, iphys%i_h_advect,         &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_ph_advect, j_ave%i_ph_advect, iphys%i_ph_advect,      &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_pc_advect, j_ave%i_pc_advect, iphys%i_pc_advect,      &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_t_diffuse, j_ave%i_t_diffuse, iphys%i_t_diffuse,      &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_c_diffuse, j_ave%i_c_diffuse, iphys%i_c_diffuse,      &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_h_flux_div, j_ave%i_h_flux_div, iphys%i_h_flux_div,   &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_ph_flux_div, j_ave%i_ph_flux_div,                     &
     &    iphys%i_ph_flux_div,                                          &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_c_flux_div, j_ave%i_c_flux_div, iphys%i_c_flux_div,   &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_pc_flux_div, j_ave%i_pc_flux_div,                     &
     &    iphys%i_pc_flux_div,                                          &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_temp_gen, j_ave%i_temp_gen, iphys%i_temp_gen,         &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_par_t_gen, j_ave%i_par_t_gen, iphys%i_par_t_gen,      &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_par_c_gen, j_ave%i_par_c_gen, iphys%i_par_c_gen,      &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_SGS_div_h_flux, j_ave%i_SGS_div_h_flux,               &
     &    iphys%i_SGS_div_h_flux,                                       &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_SGS_temp_gen, j_ave%i_SGS_temp_gen,                   &
     &    iphys%i_SGS_temp_gen,                                         &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_SGS_me_gen, j_ave%i_SGS_me_gen, iphys%i_SGS_me_gen,   &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_SGS_Lor_wk, j_ave%i_SGS_Lor_wk, iphys%i_SGS_lor_wk,   &
     
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_reynolds_wk, j_ave%i_reynolds_wk,                     &
     &    iphys%i_reynolds_wk,                                          &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_SGS_div_hf_true, j_ave%i_SGS_div_hf_true,             &
     &    iphys%i_SGS_div_hf_true,                                      &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_SGS_div_cf_true, j_ave%i_SGS_div_cf_true,             &
     &    iphys%i_SGS_div_cf_true,                                      &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_SGS_buo_wk, j_ave%i_SGS_buo_wk, iphys%i_SGS_buo_wk,   &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_SGS_comp_buo_wk, j_ave%i_SGS_comp_buo_wk,             &
     &    iphys%i_SGS_comp_buo_wk,                                      &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
!
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_SGS_Lor_wk_tr, j_ave%i_SGS_Lor_wk_tr,                 &
     &    iphys%i_SGS_Lor_wk_tr,                                        &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_reynolds_wk_tr, j_ave%i_reynolds_wk_tr,               &
     &    iphys%i_reynolds_wk_tr,                                       &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_SGS_t_gen_tr, j_ave%i_SGS_t_gen_tr,                   &
     &    iphys%i_SGS_t_gen_tr,                                         &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_SGS_c_gen_tr, j_ave%i_SGS_c_gen_tr,                   &
     &    iphys%i_SGS_c_gen_tr,                                         &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_scalar                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_SGS_me_gen_tr, j_ave%i_SGS_me_gen_tr,                 &
     &    iphys%i_SGS_me_gen_tr,                                        &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
!
! ----- lead energy and angular momentum for vector data-------------
!
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_velo, j_ave%i_velo, iphys%i_velo,                     &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_angular_mom(fluid%istack_ele_fld_smp,                &
     &    npoint_integrate, ifld_msq%ja_amom, iphys%i_velo,             &
     &    mesh, nod_fld, jacs, mhd_fem_wk, fem_wk, fem_msq)
!
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_filter_velo, j_ave%i_filter_velo,                     &
     &    iphys%i_filter_velo,                                          &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_angular_mom(fluid%istack_ele_fld_smp,                &
     &    npoint_integrate, ifld_msq%jr_amom_f, iphys%i_filter_velo,    &
     &    mesh, nod_fld, jacs, mhd_fem_wk, fem_wk, fem_msq)
!
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_magne, j_ave%i_magne, iphys%i_magne,                  &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (conduct%istack_ele_fld_smp, npoint_integrate,                 &
     &    ifld_msq%ir_me_ic, ifld_msq%ja_mag_ic, iphys%i_magne,         &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
!
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_vort, j_ave%i_vort, iphys%i_vort,                     &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_current, j_ave%i_current, iphys%i_current,            &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (conduct%istack_ele_fld_smp, npoint_integrate,                 &
     &    ifld_msq%ir_sqj_ic, ifld_msq%ja_j_ic, iphys%i_current,        &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_filter_magne, j_ave%i_filter_magne,                   &
     &    iphys%i_filter_magne,                                         &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (conduct%istack_ele_fld_smp, npoint_integrate,                 &
     &    ifld_msq%ir_me_f_ic, ifld_msq%ja_mag_f_ic,                    &
     &    iphys%i_filter_magne,                                         &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
!
      if(i_rms%i_velo .gt. 0) then
        fem_msq%rms_local(i_rms%i_velo)                                 &
     &      = half * fem_msq%rms_local(i_rms%i_velo)
      end if
      if(i_rms%i_magne .gt. 0) then
        fem_msq%rms_local(i_rms%i_magne)                                &
     &      = half * fem_msq%rms_local(i_rms%i_magne)
      end if
      if(ifld_msq%ir_me_ic .gt. 0) then
        fem_msq%rms_local(ifld_msq%ir_me_ic)                            &
     &      = half * fem_msq%rms_local(ifld_msq%ir_me_ic)
      end if
!
      if(i_rms%i_filter_velo .gt. 0) then
        fem_msq%rms_local(i_rms%i_filter_velo)                          &
     &      = half * fem_msq%rms_local(i_rms%i_filter_velo)
      end if
      if(i_rms%i_filter_magne .gt. 0) then
        fem_msq%rms_local(i_rms%i_filter_magne   )                      &
     &      = half * fem_msq%rms_local(i_rms%i_filter_magne   )
      end if
      if(ifld_msq%ir_me_f_ic .gt. 0) then
        fem_msq%rms_local(ifld_msq%ir_me_f_ic)                          &
     &      = half * fem_msq%rms_local(ifld_msq%ir_me_f_ic)
      end if
!
      if(ifld_msq%ir_rms_w .gt. 0) then
        fem_msq%rms_local(ifld_msq%ir_rms_w)                            &
     &      = fem_msq%rms_local(i_rms%i_vort)
      end if
      if(ifld_msq%ir_rms_j .gt. 0) then
        fem_msq%rms_local(ifld_msq%ir_rms_j)                            &
     &      =  fem_msq%rms_local(i_rms%i_current)
      end if
      if(ifld_msq%ir_rms_j_ic .gt. 0) then
        fem_msq%rms_local(ifld_msq%ir_rms_j_ic)                         &
     &      = fem_msq%rms_local(ifld_msq%ir_sqj_ic)
      end if
!
      call int_all_4_vector                                             &
     &   (conduct%istack_ele_fld_smp, npoint_integrate,                 &
     &    i_rms%i_electric, j_ave%i_electric, iphys%i_electric,         &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (conduct%istack_ele_fld_smp, npoint_integrate,                 &
     &    i_rms%i_poynting, j_ave%i_poynting, iphys%i_poynting,         &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
!
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_m_tension, j_ave%i_m_tension, iphys%i_m_tension,      &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_h_flux, j_ave%i_h_flux, iphys%i_h_flux,               &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_ph_flux, j_ave%i_ph_flux, iphys%i_ph_flux,            &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_c_flux, j_ave%i_c_flux, iphys%i_c_flux,               &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_pc_flux, j_ave%i_pc_flux, iphys%i_pc_flux,            &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_m_advect, j_ave%i_m_advect, iphys%i_m_advect,         &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_m_flux_div, j_ave%i_m_flux_div, iphys%i_m_flux_div,   &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_maxwell_div, j_ave%i_maxwell_div,                     &
     &    iphys%i_maxwell_div,                                          &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (conduct%istack_ele_fld_smp, npoint_integrate,                 &
     &    i_rms%i_induct_div, j_ave%i_induct_div, iphys%i_induct_div,   &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (conduct%istack_ele_fld_smp, npoint_integrate,                 &
     &    i_rms%i_induction, j_ave%i_induction, iphys%i_induction,      &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (conduct%istack_ele_fld_smp, npoint_integrate,                 &
     &    i_rms%i_vp_induct, j_ave%i_vp_induct, iphys%i_vp_induct,      &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_mag_stretch, j_ave%i_mag_stretch,                     &
     &    iphys%i_mag_stretch,                                          &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_lorentz, j_ave%i_lorentz, iphys%i_lorentz,            &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_coriolis, j_ave%i_coriolis, iphys%i_coriolis,         &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
!
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_buoyancy, j_ave%i_buoyancy, iphys%i_buoyancy,         &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_comp_buo, j_ave%i_comp_buo, iphys%i_comp_buo,         &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_filter_buo, j_ave%i_filter_buo, iphys%i_filter_buo,   &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
!
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_v_diffuse, j_ave%i_v_diffuse, iphys%i_v_diffuse,      &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (conduct%istack_ele_fld_smp, npoint_integrate,                 &
     &    i_rms%i_vp_diffuse, j_ave%i_vp_diffuse, iphys%i_vp_diffuse,   &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (conduct%istack_ele_fld_smp, npoint_integrate,                 &
     &    i_rms%i_b_diffuse, j_ave%i_b_diffuse, iphys%i_b_diffuse,      &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
!
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_grad_vx, j_ave%i_grad_vx, iphys%i_grad_vx,            &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_grad_vy, j_ave%i_grad_vy, iphys%i_grad_vy,            &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_grad_vz, j_ave%i_grad_vz, iphys%i_grad_vz,            &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
!
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_SGS_h_flux, j_ave%i_SGS_h_flux, iphys%i_SGS_h_flux,   &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_SGS_c_flux, j_ave%i_SGS_c_flux, iphys%i_SGS_c_flux,   &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_SGS_div_m_flux, j_ave%i_SGS_div_m_flux,               &
     &    iphys%i_SGS_div_m_flux,                                       &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_SGS_Lorentz, j_ave%i_SGS_Lorentz,                     &
     &    iphys%i_SGS_Lorentz,                                          &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (conduct%istack_ele_fld_smp, npoint_integrate,                 &
     &    i_rms%i_SGS_induction, j_ave%i_SGS_induction,                 &
     &    iphys%i_SGS_induction,                                        &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (conduct%istack_ele_fld_smp, npoint_integrate,                 &
     &    i_rms%i_SGS_vp_induct, j_ave%i_SGS_vp_induct,                 &
     &    iphys%i_SGS_vp_induct,                                        &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_SGS_buoyancy, j_ave%i_SGS_buoyancy,                   &
     &    iphys%i_SGS_buoyancy,                                         &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_SGS_comp_buo, j_ave%i_SGS_comp_buo,                   &
     &    iphys%i_SGS_comp_buo,                                         &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
!
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_SGS_div_mf_true, j_ave%i_SGS_div_mf_true,             &
     &    iphys%i_SGS_div_mf_true,                                      &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_SGS_Lor_true, j_ave%i_SGS_Lor_true,                   &
     &    iphys%i_SGS_Lor_true,                                         &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_vector                                             &
     &   (conduct%istack_ele_fld_smp, npoint_integrate,                 &
     &    i_rms%i_SGS_idct_true, j_ave%i_SGS_idct_true,                 &
     &    iphys%i_SGS_idct_true,                                        &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
!
!
! ----- lead average values for symmetric tensor data-------------
!
      call int_all_4_sym_tensor                                         &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_m_flux, j_ave%i_m_flux, iphys%i_m_flux,               &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_sym_tensor                                         &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_maxwell, j_ave%i_maxwell, iphys%i_maxwell,            &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_sym_tensor                                         &
     &   (fluid%istack_ele_fld_smp, npoint_integrate,                   &
     &    i_rms%i_SGS_m_flux, j_ave%i_SGS_m_flux, iphys%i_SGS_m_flux,   &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_sym_tensor(fluid%istack_ele_fld_smp,               &
     &    npoint_integrate, i_rms%i_SGS_maxwell,                        &
     &    j_ave%i_SGS_maxwell, iphys%i_SGS_maxwell,                     &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
!
      call int_all_4_asym_tensor                                        &
     &   (conduct%istack_ele_fld_smp, npoint_integrate,                 &
     &    i_rms%i_induct_t, j_ave%i_induct_t, iphys%i_induct_t,         &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
      call int_all_4_asym_tensor(conduct%istack_ele_fld_smp,            &
     &    npoint_integrate, i_rms%i_SGS_induct_t,                       &
     &    j_ave%i_SGS_induct_t, iphys%i_SGS_induct_t,                   &
     &    mesh, nod_fld, jacs, fem_wk, fem_msq)
!
      end subroutine s_int_mean_squares
!
! ----------------------------------------------------------------------
!
      subroutine int_no_evo_mean_squares(i_step, dt, mesh,              &
     &          fl_prop, cd_prop, iphys, nod_fld, iphys_ele, ele_fld,   &
     &          fluid, jacs, i_rms, j_ave, fem_wk, fem_msq)
!
      use int_norm_div_MHD
      use int_rms_div_MHD
      use estimate_stabilities
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: dt
!
      type(mesh_geometry), intent(in) :: mesh
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in) :: cd_prop
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_type), intent(in) :: jacs
      type(field_geometry_data), intent(in) :: fluid
      type(phys_address), intent(in) :: i_rms, j_ave
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(mean_square_values), intent(inout) :: fem_msq
!
!
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
        call int_norm_divergence                                        &
     &    (fluid%istack_ele_fld_smp, iphys%i_velo, mesh%node, mesh%ele, &
     &     nod_fld, jacs%g_FEM, jacs%jac_3d, fem_wk,                    &
     &     fem_msq%ave_local(j_ave%i_div_v))
        call int_rms_divergence                                         &
     &    (fluid%istack_ele_fld_smp, iphys%i_velo, mesh%node, mesh%ele, &
     &     nod_fld, jacs%g_FEM, jacs%jac_3d, fem_wk,                    &
     &     fem_msq%rms_local(i_rms%i_div_v))
        call cal_stability_4_advect(i_step, dt, mesh%ele, fluid,        &
     &      ele_fld%ntot_phys, iphys_ele%i_velo, ele_fld%d_fld)
      end if
!
      if  (cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        call int_norm_divergence                                        &
     &     (mesh%ele%istack_ele_smp, iphys%i_vecp, mesh%node, mesh%ele, &
     &      nod_fld, jacs%g_FEM, jacs%jac_3d, fem_wk,                   &
     &      fem_msq%ave_local(j_ave%i_div_a))
        call int_rms_divergence                                         &
     &     (mesh%ele%istack_ele_smp, iphys%i_vecp, mesh%node, mesh%ele, &
     &      nod_fld, jacs%g_FEM, jacs%jac_3d, fem_wk,                   &
     &      fem_msq%rms_local(i_rms%i_div_a))
      end if
!
      if      (cd_prop%iflag_Bevo_scheme .gt. id_no_evolution           &
     &    .or. cd_prop%iflag_Aevo_scheme .gt. id_no_evolution) then
        call int_norm_divergence                                        &
     &    (mesh%ele%istack_ele_smp, iphys%i_magne, mesh%node, mesh%ele, &
     &     nod_fld, jacs%g_FEM, jacs%jac_3d, fem_wk,                    &
     &     fem_msq%ave_local(j_ave%i_div_b))
        call int_rms_divergence                                         &
     &    (mesh%ele%istack_ele_smp, iphys%i_magne, mesh%node, mesh%ele, &
     &     nod_fld, jacs%g_FEM, jacs%jac_3d, fem_wk,                    &
     &     fem_msq%rms_local(i_rms%i_div_b))
      end if
!
      end subroutine int_no_evo_mean_squares
!
! ----------------------------------------------------------------------
!
      end module int_bulk

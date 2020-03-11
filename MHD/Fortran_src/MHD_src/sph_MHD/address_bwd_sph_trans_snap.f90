!>@file   address_bwd_sph_trans_snap.f90
!!@brief  module address_bwd_sph_trans_snap
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine b_trans_address_vector_snap                          &
!!     &         (ipol, itor, iphys, b_trns, trns_back)
!!      subroutine b_trans_address_scalar_snap                          &
!!     &         (ipol, itor, iphys, b_trns, trns_back)
!!        type(phys_address), intent(in) :: ipol, itor, iphys
!!        type(address_each_sph_trans), intent(inout) :: trns_back
!!        type(phys_address), intent(inout) :: b_trns
!!@endverbatim
!
      module address_bwd_sph_trans_snap
!
      use m_precision
!
      use m_phys_labels
      use m_phys_constants
      use t_phys_address
      use t_addresses_sph_transform
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_vector_snap                            &
     &         (ipol, itor, iphys, b_trns, trns_back)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
      trns_back%nfield = 0
      call alloc_sph_trns_field_name(trns_back)
!
!      if(b_trns%i_velo .eq. 0) then
      call add_field_name_4_sph_trns_snap                               &
     &   (velocity%name, velocity%n_comp,                               &
     &    ipol%i_velo, itor%i_velo, iphys%i_velo,                       &
     &    b_trns%i_velo, trns_back)
!      end if
!      if(b_trns%i_vort .eq. 0) then
      call add_field_name_4_sph_trns_snap                               &
     &   (vorticity%name, vorticity%n_comp,                             &
     &    ipol%i_vort, itor%i_vort, iphys%i_vort,                       &
     &    b_trns%i_vort, trns_back)
!      end if
!      if(b_trns%i_magne .eq. 0) then
      call add_field_name_4_sph_trns_snap                               &
     &   (magnetic_field%name, magnetic_field%n_comp,                   &
     &    ipol%i_magne, itor%i_magne, iphys%i_magne,                    &
     &    b_trns%i_magne, trns_back)
!      end if
!      if(b_trns%i_current .eq. 0) then
      call add_field_name_4_sph_trns_snap(fhd_current, n_vector,        &
     &    ipol%i_current, itor%i_current, iphys%i_current,              &
     &    b_trns%i_current, trns_back)
!      end if
!
      call add_field_name_4_sph_trns_snap                               &
     &   (viscous_diffusion%name, viscous_diffusion%n_comp,             &
     &    ipol%diffusion%i_v_diffuse, itor%diffusion%i_v_diffuse,       &
     &    iphys%diffusion%i_v_diffuse, b_trns%diffusion%i_v_diffuse,    &
     &    trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (vorticity_diffusion%name, vorticity_diffusion%n_comp,         &
     &    ipol%diffusion%i_w_diffuse, itor%diffusion%i_w_diffuse,       &
     &    iphys%diffusion%i_w_diffuse, b_trns%diffusion%i_w_diffuse,    &
     &    trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (vector_potential_diffusion%name,                              &
     &    vector_potential_diffusion%n_comp,                            &
     &    ipol%diffusion%i_vp_diffuse, itor%diffusion%i_vp_diffuse,     &
     &    iphys%diffusion%i_vp_diffuse, b_trns%diffusion%i_vp_diffuse,  &
     &    trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (magnetic_diffusion%name, magnetic_diffusion%n_comp,           &
     &    ipol%diffusion%i_b_diffuse, itor%diffusion%i_b_diffuse,       &
     &    iphys%diffusion%i_b_diffuse, b_trns%diffusion%i_b_diffuse,    &
     &    trns_back)
!
      call add_field_name_4_sph_trns_snap                               &
     &   (rot_inertia%name, rot_inertia%n_comp,                         &
     &    ipol%rot_forces%i_m_advect, itor%rot_forces%i_m_advect,       &
     &    iphys%rot_forces%i_m_advect, b_trns%rot_forces%i_m_advect,    &
     &    trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (rot_Coriolis_force%name, rot_Coriolis_force%n_comp,           &
     &    ipol%rot_forces%i_Coriolis, itor%rot_forces%i_Coriolis,       &
     &    iphys%rot_forces%i_Coriolis, b_trns%rot_forces%i_Coriolis,    &
     &    trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (rot_Lorentz_force%name, rot_Lorentz_force%n_comp,             &
     &    ipol%rot_forces%i_lorentz, itor%rot_forces%i_lorentz,         &
     &    iphys%rot_forces%i_lorentz, b_trns%rot_forces%i_lorentz,      &
     &    trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (rot_buoyancy%name, rot_buoyancy%n_comp,                       &
     &    ipol%rot_forces%i_buoyancy, itor%rot_forces%i_buoyancy,       &
     &    iphys%rot_forces%i_buoyancy, b_trns%rot_forces%i_buoyancy,    &
     &    trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (rot_composite_buoyancy%name, rot_composite_buoyancy%n_comp,   &
     &    ipol%rot_forces%i_comp_buo, itor%rot_forces%i_comp_buo,       &
     &    iphys%rot_forces%i_comp_buo, b_trns%rot_forces%i_comp_buo,    &
     &    trns_back)
!
      call add_field_name_4_sph_trns_snap                               &
     &   (buoyancy%name, buoyancy%n_comp,                               &
     &    ipol%forces%i_buoyancy, itor%forces%i_buoyancy,               &
     &    iphys%forces%i_buoyancy, b_trns%forces%i_buoyancy, trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (composite_buoyancy%name, composite_buoyancy%n_comp,           &
     &    ipol%forces%i_comp_buo, itor%forces%i_comp_buo,               &
     &    iphys%forces%i_comp_buo, b_trns%forces%i_comp_buo, trns_back)
!
      call add_field_name_4_sph_trns_snap                               &
     &   (rest_of_geostrophic%name, rest_of_geostrophic%n_comp,         &
     &    ipol%prod_fld%i_geostrophic, itor%prod_fld%i_geostrophic,     &
     &    iphys%prod_fld%i_geostrophic, b_trns%prod_fld%i_geostrophic,  &
     &    trns_back)
!
      call add_field_name_4_sph_trns_snap                               &
     &   (heat_flux_w_SGS%name, heat_flux_w_SGS%n_comp,                 &
     &    ipol%frc_w_SGS%i_SGS_h_flux, itor%frc_w_SGS%i_SGS_h_flux,     &
     &    iphys%frc_w_SGS%i_SGS_h_flux, b_trns%frc_w_SGS%i_SGS_h_flux,  &
     &    trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (compostion_flux_w_SGS%name, compostion_flux_w_SGS%n_comp,     &
     &    ipol%frc_w_SGS%i_SGS_c_flux, itor%frc_w_SGS%i_SGS_c_flux,     &
     &    iphys%frc_w_SGS%i_SGS_c_flux, b_trns%frc_w_SGS%i_SGS_c_flux,  &
     &    trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (intertia_w_SGS%name, intertia_w_SGS%n_comp,                   &
     &    ipol%frc_w_SGS%i_SGS_inertia, itor%frc_w_SGS%i_SGS_inertia,   &
     &    iphys%frc_w_SGS%i_SGS_inertia,                                &
     &    b_trns%frc_w_SGS%i_SGS_inertia, trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (Lorentz_w_SGS%name, Lorentz_w_SGS%n_comp,                     &
     &    ipol%frc_w_SGS%i_SGS_Lorentz, itor%frc_w_SGS%i_SGS_Lorentz,   &
     &    iphys%frc_w_SGS%i_SGS_Lorentz,                                &
     &    b_trns%frc_w_SGS%i_SGS_Lorentz, trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (vecp_induction_w_SGS%name, vecp_induction_w_SGS%n_comp,       &
     &    ipol%frc_w_SGS%i_SGS_vp_induct,                               &
     &    itor%frc_w_SGS%i_SGS_vp_induct,                               &
     &    iphys%frc_w_SGS%i_SGS_vp_induct,                              &
     &    b_trns%frc_w_SGS%i_SGS_vp_induct, trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (induction_w_SGS%name, induction_w_SGS%n_comp,                 &
     &    ipol%frc_w_SGS%i_SGS_induction,                               &
     &    itor%frc_w_SGS%i_SGS_induction,                               &
     &    iphys%frc_w_SGS%i_SGS_induction,                              &
     &    b_trns%frc_w_SGS%i_SGS_induction,                             &
     &    trns_back)
!
      call add_field_name_4_sph_trns_snap                               &
     &   (rot_SGS_inertia%name, rot_SGS_inertia%n_comp,                 &
     &    ipol%rot_SGS%i_SGS_inertia, itor%rot_SGS%i_SGS_inertia,       &
     &    iphys%rot_SGS%i_SGS_inertia, b_trns%rot_SGS%i_SGS_inertia,    &
     &    trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (rot_SGS_Lorentz%name, rot_SGS_Lorentz%n_comp,                 &
     &    ipol%rot_SGS%i_SGS_Lorentz, itor%rot_SGS%i_SGS_Lorentz,       &
     &    iphys%rot_SGS%i_SGS_Lorentz, b_trns%rot_SGS%i_SGS_Lorentz,    &
     &    trns_back)
!
      call add_field_name_4_sph_trns(ipol%SGS_term%i_SGS_induction,     &
     &    SGS_induction%name, SGS_induction%n_comp,                     &
     &    ipol%SGS_term%i_SGS_induction, itor%SGS_term%i_SGS_induction, &
     &    iphys%SGS_term%i_SGS_induction,                               &
     &    b_trns%SGS_term%i_SGS_induction, trns_back)
!
      call add_field_name_4_sph_trns_snap                               &
     &   (pressure_gradient%name, pressure_gradient%n_comp,             &
     &    ipol%forces%i_press_grad, itor%forces%i_press_grad,           &
     &    iphys%forces%i_press_grad, b_trns%forces%i_press_grad,        &
     &    trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (magnetic_induction%name, magnetic_induction%n_comp,           &
     &    ipol%forces%i_induction, itor%forces%i_induction,             &
     &    iphys%forces%i_induction, b_trns%forces%i_induction,          &
     &    trns_back)
!
      call add_field_name_4_sph_trns_snap                               &
     &   (grad_temp%name, grad_temp%n_comp,                             &
     &    ipol%grad_fld%i_grad_temp, itor%grad_fld%i_grad_temp,         &
     &    iphys%grad_fld%i_grad_temp, b_trns%grad_fld%i_grad_temp,      &
     &    trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (grad_composition%name, grad_composition%n_comp,               &
     &    ipol%grad_fld%i_grad_composit, itor%grad_fld%i_grad_composit, &
     &    iphys%grad_fld%i_grad_composit,                               &
     &    b_trns%grad_fld%i_grad_composit, trns_back)
!
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_vx, grad_v_1%name, grad_v_1%n_comp,   &
     &    ipol%diff_vector%i_grad_vx, itor%diff_vector%i_grad_vx,       &
     &    iphys%diff_vector%i_grad_vx, b_trns%diff_vector%i_grad_vx,    &
     &    trns_back)
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_vy, grad_v_2%name, grad_v_2%n_comp,   &
     &    ipol%diff_vector%i_grad_vy, itor%diff_vector%i_grad_vy,       &
     &    iphys%diff_vector%i_grad_vy, b_trns%diff_vector%i_grad_vy,    &
     &    trns_back)
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_vy, grad_v_3%name, grad_v_3%n_comp,   &
     &    ipol%diff_vector%i_grad_vz, itor%diff_vector%i_grad_vz,       &
     &    iphys%diff_vector%i_grad_vz, b_trns%diff_vector%i_grad_vz,    &
     &    trns_back)
!
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_wx, grad_w_1%name, grad_w_1%n_comp,   &
     &    ipol%diff_vector%i_grad_wx, itor%diff_vector%i_grad_wx,       &
     &    iphys%diff_vector%i_grad_wx, b_trns%diff_vector%i_grad_wx,    &
     &    trns_back)
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_wy, grad_w_2%name, grad_w_2%n_comp,   &
     &    ipol%diff_vector%i_grad_wy, itor%diff_vector%i_grad_wy,       &
     &    iphys%diff_vector%i_grad_wy, b_trns%diff_vector%i_grad_wy,    &
     &    trns_back)
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_wz, grad_w_3%name, grad_w_3%n_comp,   &
     &    ipol%diff_vector%i_grad_wz, itor%diff_vector%i_grad_wz,       &
     &    iphys%diff_vector%i_grad_wz, b_trns%diff_vector%i_grad_wz,    &
     &    trns_back)
!
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_ax, grad_a_1%name, grad_a_1%n_comp,   &
     &    ipol%diff_vector%i_grad_ax, itor%diff_vector%i_grad_ax,       &
     &    iphys%diff_vector%i_grad_ax, b_trns%diff_vector%i_grad_ax,    &
     &    trns_back)
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_ay, grad_a_2%name, grad_a_2%n_comp,   &
     &    ipol%diff_vector%i_grad_ay, itor%diff_vector%i_grad_ay,       &
     &    iphys%diff_vector%i_grad_ay, b_trns%diff_vector%i_grad_ay,    &
     &    trns_back)
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_az, grad_a_3%name, grad_a_3%n_comp,   &
     &    ipol%diff_vector%i_grad_az, itor%diff_vector%i_grad_az,       &
     &    iphys%diff_vector%i_grad_az, b_trns%diff_vector%i_grad_az,    &
     &    trns_back)
!
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_bx, grad_b_1%name, grad_b_1%n_comp,   &
     &    ipol%diff_vector%i_grad_bx, itor%diff_vector%i_grad_bx,       &
     &    iphys%diff_vector%i_grad_bx, b_trns%diff_vector%i_grad_bx,    &
     &    trns_back)
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_by, grad_b_2%name, grad_b_2%n_comp,   &
     &    ipol%diff_vector%i_grad_by, itor%diff_vector%i_grad_by,       &
     &    iphys%diff_vector%i_grad_by, b_trns%diff_vector%i_grad_by,    &
     &    trns_back)
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_bz, grad_b_3%name, grad_b_3%n_comp,   &
     &    ipol%diff_vector%i_grad_bz, itor%diff_vector%i_grad_bz,       &
     &    iphys%diff_vector%i_grad_bz, b_trns%diff_vector%i_grad_bz,    &
     &    trns_back)
!
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_jx, grad_j_1%name, grad_j_1%n_comp,   &
     &    ipol%diff_vector%i_grad_jx, itor%diff_vector%i_grad_jx,       &
     &    iphys%diff_vector%i_grad_jx, b_trns%diff_vector%i_grad_jx,    &
     &    trns_back)
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_jy, grad_j_2%name, grad_j_2%n_comp,   &
     &    ipol%diff_vector%i_grad_jy, itor%diff_vector%i_grad_jy,       &
     &    iphys%diff_vector%i_grad_jy, b_trns%diff_vector%i_grad_jy,    &
     &    trns_back)
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_jz, grad_j_3%name, grad_j_3%n_comp,   &
     &    ipol%diff_vector%i_grad_jz, itor%diff_vector%i_grad_jz,       &
     &    iphys%diff_vector%i_grad_jz, b_trns%diff_vector%i_grad_jz,    &
     &    trns_back)
!
      call add_field_name_4_sph_trns_snap                               &
     &   (truncated_magnetic_field%name,                                &
     &    truncated_magnetic_field%n_comp,                              &
     &    ipol%prod_fld%i_truncated_B, itor%prod_fld%i_truncated_B,     &
     &    iphys%prod_fld%i_truncated_B, b_trns%prod_fld%i_truncated_B,  &
     &    trns_back)
!
      trns_back%num_vector = trns_back%nfield
!
      end subroutine b_trans_address_vector_snap
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_scalar_snap                            &
     &         (ipol, itor, iphys, b_trns, trns_back)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
!      if(b_trns%i_temp.eq.0 .or. ipol%i_per_temp.gt.0) then
      call add_field_name_4_sph_trns_snap(fhd_temp, n_scalar,           &
     &    ipol%i_temp, itor%i_temp, iphys%i_temp,                       &
     &    b_trns%i_temp, trns_back)
!      end if
!      if(b_trns%i_light .eq. 0) then
      call add_field_name_4_sph_trns_snap(fhd_light, n_scalar,          &
     &    ipol%i_light, itor%i_light, iphys%i_light,                    &
     &    b_trns%i_light, trns_back)
!      end if
!
      call add_field_name_4_sph_trns_snap(fhd_press, n_scalar,          &
     &    ipol%i_press, itor%i_press, iphys%i_press,                    &
     &    b_trns%i_press, trns_back)
      call add_field_name_4_sph_trns_nofld(fhd_part_temp, n_scalar,     &
     &    ipol%i_per_temp, itor%i_per_temp, iphys%i_per_temp,           &
     &    b_trns%i_per_temp, trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (filter_temperature%name, filter_temperature%n_comp,           &
     &    ipol%i_filter_temp, itor%i_filter_temp, iphys%i_filter_temp,  &
     &    b_trns%i_filter_temp, trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (thermal_diffusion%name, thermal_diffusion%n_comp,             &
     &    ipol%diffusion%i_t_diffuse, itor%diffusion%i_t_diffuse,       &
     &    iphys%diffusion%i_t_diffuse, b_trns%diffusion%i_t_diffuse,    &
     &    trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (composition_diffusion%name, composition_diffusion%n_comp,     &
     &    ipol%diffusion%i_c_diffuse, itor%diffusion%i_c_diffuse,       &
     &    iphys%diffusion%i_c_diffuse, b_trns%diffusion%i_c_diffuse,    &
     &    trns_back)
!
      call add_field_name_4_sph_trns_snap                               &
     &   (heat_advect%name, heat_advect%n_comp,                         &
     &    ipol%forces%i_h_advect, itor%forces%i_h_advect,               &
     &    iphys%forces%i_h_advect, b_trns%forces%i_h_advect, trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (composition_advect%name, composition_advect%n_comp,           &
     &    ipol%forces%i_c_advect, itor%forces%i_c_advect,               &
     &    iphys%forces%i_c_advect, b_trns%forces%i_c_advect, trns_back)
!
      call add_field_name_4_sph_trns_snap                               &
     &   (div_Coriolis_force%name, div_Coriolis_force%n_comp,           &
     &    ipol%div_forces%i_Coriolis, itor%div_forces%i_Coriolis,       &
     &    iphys%div_forces%i_Coriolis, b_trns%div_forces%i_Coriolis,    &
     &    trns_back)
!
      call add_field_name_4_sph_trns_snap                               &
     &   (div_SGS_inertia%name, div_SGS_inertia%n_comp,                 &
     &    ipol%div_SGS%i_SGS_inertia, itor%div_SGS%i_SGS_inertia,       &
     &    iphys%div_SGS%i_SGS_inertia, b_trns%div_SGS%i_SGS_inertia,    &
     &    trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (div_SGS_Lorentz%name, div_SGS_Lorentz%n_comp,                 &
     &    ipol%div_SGS%i_SGS_Lorentz, itor%div_SGS%i_SGS_Lorentz,       &
     &    iphys%div_SGS%i_SGS_Lorentz, b_trns%div_SGS%i_SGS_Lorentz,    &
     &    trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (div_SGS_h_flux%name, div_SGS_h_flux%n_comp,                   &
     &    ipol%div_SGS%i_SGS_h_flux, itor%div_SGS%i_SGS_h_flux,         &
     &    iphys%div_SGS%i_SGS_h_flux, b_trns%div_SGS%i_SGS_h_flux,      &
     &    trns_back)
      call add_field_name_4_sph_trns_snap                               &
     &   (div_SGS_c_flux%name, div_SGS_c_flux%n_comp,                   &
     &    ipol%div_SGS%i_SGS_c_flux, itor%div_SGS%i_SGS_c_flux,         &
     &    iphys%div_SGS%i_SGS_c_flux, b_trns%div_SGS%i_SGS_c_flux,      &
     &    trns_back)
      trns_back%num_scalar = trns_back%nfield - trns_back%num_vector
!
      end subroutine b_trans_address_scalar_snap
!
!-----------------------------------------------------------------------
!
      end module address_bwd_sph_trans_snap

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
!!     &         (ipol, iphys, b_trns, trns_back)
!!      subroutine b_trans_address_scalar_snap                          &
!!     &         (ipol, iphys, b_trns, trns_back)
!!        type(phys_address), intent(in) :: ipol, iphys
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
     &         (ipol, iphys, b_trns, trns_back)
!
      use m_diff_SGS_term_labels
      use m_force_w_SGS_labels
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
      trns_back%nfield = 0
      call alloc_sph_trns_field_name(trns_back)
!
!      if(b_trns%base%i_velo .eq. 0) then
      call add_field_name_4_sph_trns_snap(velocity,                     &
     &    ipol%base%i_velo, iphys%base%i_velo, b_trns%base%i_velo,      &
     &    trns_back)
!      end if
!      if(b_trns%base%i_vort .eq. 0) then
      call add_field_name_4_sph_trns_snap(vorticity,                    &
     &    ipol%base%i_vort, iphys%base%i_vort, b_trns%base%i_vort,      &
     &    trns_back)
!      end if
!      if(b_trns%base%i_magne .eq. 0) then
      call add_field_name_4_sph_trns_snap(magnetic_field,               &
     &    ipol%base%i_magne, iphys%base%i_magne, b_trns%base%i_magne,   &
     &    trns_back)
!      end if
!      if(b_trns%base%i_current .eq. 0) then
      call add_field_name_4_sph_trns_snap(current_density,              &
     &    ipol%base%i_current, iphys%base%i_current,                    &
     &    b_trns%base%i_current, trns_back)
!      end if
!
      call add_field_name_4_sph_trns_snap(viscous_diffusion,            &
     &    ipol%diffusion%i_v_diffuse, iphys%diffusion%i_v_diffuse,      &
     &    b_trns%diffusion%i_v_diffuse, trns_back)
      call add_field_name_4_sph_trns_snap(vorticity_diffusion,          &
     &    ipol%diffusion%i_w_diffuse, iphys%diffusion%i_w_diffuse,      &
     &    b_trns%diffusion%i_w_diffuse, trns_back)
      call add_field_name_4_sph_trns_snap(vector_potential_diffusion,   &
     &    ipol%diffusion%i_vp_diffuse, iphys%diffusion%i_vp_diffuse,    &
     &    b_trns%diffusion%i_vp_diffuse, trns_back)
      call add_field_name_4_sph_trns_snap(magnetic_diffusion,           &
     &    ipol%diffusion%i_b_diffuse, iphys%diffusion%i_b_diffuse,      &
     &    b_trns%diffusion%i_b_diffuse, trns_back)
!
      call add_field_name_4_sph_trns_snap(rot_inertia,                  &
     &    ipol%rot_forces%i_m_advect, iphys%rot_forces%i_m_advect,      &
     &    b_trns%rot_forces%i_m_advect, trns_back)
      call add_field_name_4_sph_trns_snap(rot_Coriolis_force,           &
     &    ipol%rot_forces%i_Coriolis, iphys%rot_forces%i_Coriolis,      &
     &    b_trns%rot_forces%i_Coriolis, trns_back)
      call add_field_name_4_sph_trns_snap(rot_Lorentz_force,            &
     &    ipol%rot_forces%i_lorentz, iphys%rot_forces%i_lorentz,        &
     &    b_trns%rot_forces%i_lorentz, trns_back)
      call add_field_name_4_sph_trns_snap(rot_buoyancy,                 &
     &    ipol%rot_forces%i_buoyancy, iphys%rot_forces%i_buoyancy,      &
     &    b_trns%rot_forces%i_buoyancy, trns_back)
      call add_field_name_4_sph_trns_snap(rot_composite_buoyancy,       &
     &    ipol%rot_forces%i_comp_buo, iphys%rot_forces%i_comp_buo,      &
     &    b_trns%rot_forces%i_comp_buo, trns_back)
!
      call add_field_name_4_sph_trns_snap(buoyancy,                     &
     &    ipol%forces%i_buoyancy, iphys%forces%i_buoyancy,              &
     &    b_trns%forces%i_buoyancy, trns_back)
      call add_field_name_4_sph_trns_snap(composite_buoyancy,           &
     &    ipol%forces%i_comp_buo, iphys%forces%i_comp_buo,              &
     &    b_trns%forces%i_comp_buo, trns_back)
!
      call add_field_name_4_sph_trns_snap(rest_of_geostrophic,          &
     &    ipol%prod_fld%i_geostrophic, iphys%prod_fld%i_geostrophic,    &
     &    b_trns%prod_fld%i_geostrophic, trns_back)
!
      call add_field_name_4_sph_trns_snap(heat_flux_w_SGS,              &
     &    ipol%frc_w_SGS%i_SGS_h_flux, iphys%frc_w_SGS%i_SGS_h_flux,    &
     &    b_trns%frc_w_SGS%i_SGS_h_flux, trns_back)
      call add_field_name_4_sph_trns_snap(compostion_flux_w_SGS,        &
     &    ipol%frc_w_SGS%i_SGS_c_flux, iphys%frc_w_SGS%i_SGS_c_flux,    &
     &    b_trns%frc_w_SGS%i_SGS_c_flux, trns_back)
      call add_field_name_4_sph_trns_snap(intertia_w_SGS,               &
     &    ipol%frc_w_SGS%i_SGS_inertia, iphys%frc_w_SGS%i_SGS_inertia,  &
     &    b_trns%frc_w_SGS%i_SGS_inertia, trns_back)
      call add_field_name_4_sph_trns_snap(Lorentz_w_SGS,                &
     &    ipol%frc_w_SGS%i_SGS_Lorentz, iphys%frc_w_SGS%i_SGS_Lorentz,  &
     &    b_trns%frc_w_SGS%i_SGS_Lorentz, trns_back)
      call add_field_name_4_sph_trns_snap(vecp_induction_w_SGS,         &
     &    ipol%frc_w_SGS%i_SGS_vp_induct,                               &
     &    iphys%frc_w_SGS%i_SGS_vp_induct,                              &
     &    b_trns%frc_w_SGS%i_SGS_vp_induct, trns_back)
      call add_field_name_4_sph_trns_snap(induction_w_SGS,              &
     &    ipol%frc_w_SGS%i_SGS_induction,                               &
     &    iphys%frc_w_SGS%i_SGS_induction,                              &
     &    b_trns%frc_w_SGS%i_SGS_induction, trns_back)
!
      call add_field_name_4_sph_trns_snap(rot_SGS_inertia,              &
     &    ipol%rot_SGS%i_SGS_inertia, iphys%rot_SGS%i_SGS_inertia,      &
     &    b_trns%rot_SGS%i_SGS_inertia, trns_back)
      call add_field_name_4_sph_trns_snap(rot_SGS_Lorentz,              &
     &    ipol%rot_SGS%i_SGS_Lorentz, iphys%rot_SGS%i_SGS_Lorentz,      &
     &    b_trns%rot_SGS%i_SGS_Lorentz, trns_back)
!
      call add_field_name_4_sph_trns(ipol%SGS_term%i_SGS_induction,     &
     &    SGS_induction%name, SGS_induction%n_comp,                     &
     &    ipol%SGS_term%i_SGS_induction,                                &
     &    iphys%SGS_term%i_SGS_induction,                               &
     &    b_trns%SGS_term%i_SGS_induction, trns_back)
!
      call add_field_name_4_sph_trns_snap(pressure_gradient,            &
     &    ipol%forces%i_press_grad, iphys%forces%i_press_grad,          &
     &    b_trns%forces%i_press_grad, trns_back)
      call add_field_name_4_sph_trns_snap(magnetic_induction,           &
     &    ipol%forces%i_induction, iphys%forces%i_induction,            &
     &    b_trns%forces%i_induction, trns_back)
!
      call add_field_name_4_sph_trns_snap(grad_temp,                    &
     &    ipol%grad_fld%i_grad_temp, iphys%grad_fld%i_grad_temp,        &
     &    b_trns%grad_fld%i_grad_temp, trns_back)
      call add_field_name_4_sph_trns_snap(grad_composition,             &
     &    ipol%grad_fld%i_grad_composit,                                &
     &    iphys%grad_fld%i_grad_composit,                               &
     &    b_trns%grad_fld%i_grad_composit, trns_back)
!
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_vx, grad_v_1%name, grad_v_1%n_comp,   &
     &    ipol%diff_vector%i_grad_vx, iphys%diff_vector%i_grad_vx,      &
     &    b_trns%diff_vector%i_grad_vx, trns_back)
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_vy, grad_v_2%name, grad_v_2%n_comp,   &
     &    ipol%diff_vector%i_grad_vy, iphys%diff_vector%i_grad_vy,      &
     &    b_trns%diff_vector%i_grad_vy, trns_back)
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_vy, grad_v_3%name, grad_v_3%n_comp,   &
     &    ipol%diff_vector%i_grad_vz, iphys%diff_vector%i_grad_vz,      &
     &    b_trns%diff_vector%i_grad_vz, trns_back)
!
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_wx, grad_w_1%name, grad_w_1%n_comp,   &
     &    ipol%diff_vector%i_grad_wx, iphys%diff_vector%i_grad_wx,      &
     &    b_trns%diff_vector%i_grad_wx, trns_back)
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_wy, grad_w_2%name, grad_w_2%n_comp,   &
     &    ipol%diff_vector%i_grad_wy, iphys%diff_vector%i_grad_wy,      &
     &    b_trns%diff_vector%i_grad_wy, trns_back)
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_wz, grad_w_3%name, grad_w_3%n_comp,   &
     &    ipol%diff_vector%i_grad_wz, iphys%diff_vector%i_grad_wz,      &
     &    b_trns%diff_vector%i_grad_wz, trns_back)
!
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_ax, grad_a_1%name, grad_a_1%n_comp,   &
     &    ipol%diff_vector%i_grad_ax, iphys%diff_vector%i_grad_ax,      &
     &    b_trns%diff_vector%i_grad_ax, trns_back)
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_ay, grad_a_2%name, grad_a_2%n_comp,   &
     &    ipol%diff_vector%i_grad_ay, iphys%diff_vector%i_grad_ay,      &
     &    b_trns%diff_vector%i_grad_ay, trns_back)
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_az, grad_a_3%name, grad_a_3%n_comp,   &
     &    ipol%diff_vector%i_grad_az, iphys%diff_vector%i_grad_az,      &
     &    b_trns%diff_vector%i_grad_az, trns_back)
!
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_bx, grad_b_1%name, grad_b_1%n_comp,   &
     &    ipol%diff_vector%i_grad_bx, iphys%diff_vector%i_grad_bx,      &
     &    b_trns%diff_vector%i_grad_bx, trns_back)
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_by, grad_b_2%name, grad_b_2%n_comp,   &
     &    ipol%diff_vector%i_grad_by, iphys%diff_vector%i_grad_by,      &
     &    b_trns%diff_vector%i_grad_by, trns_back)
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_bz, grad_b_3%name, grad_b_3%n_comp,   &
     &    ipol%diff_vector%i_grad_bz, iphys%diff_vector%i_grad_bz,      &
     &    b_trns%diff_vector%i_grad_bz, trns_back)
!
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_jx, grad_j_1%name, grad_j_1%n_comp,   &
     &    ipol%diff_vector%i_grad_jx, iphys%diff_vector%i_grad_jx,      &
     &    b_trns%diff_vector%i_grad_jx, trns_back)
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_jy, grad_j_2%name, grad_j_2%n_comp,   &
     &    ipol%diff_vector%i_grad_jy, iphys%diff_vector%i_grad_jy,      &
     &    b_trns%diff_vector%i_grad_jy, trns_back)
      call add_field_name_4_sph_trns                                    &
     &   (ipol%diff_vector%i_grad_jz, grad_j_3%name, grad_j_3%n_comp,   &
     &    ipol%diff_vector%i_grad_jz, iphys%diff_vector%i_grad_jz,      &
     &    b_trns%diff_vector%i_grad_jz, trns_back)
!
      call add_field_name_4_sph_trns_snap(truncated_magnetic_field,     &
     &    ipol%prod_fld%i_truncated_B, iphys%prod_fld%i_truncated_B,    &
     &    b_trns%prod_fld%i_truncated_B, trns_back)
!
      trns_back%num_vector = trns_back%nfield
!
      end subroutine b_trans_address_vector_snap
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_scalar_snap                            &
     &         (ipol, iphys, b_trns, trns_back)
!
      use m_diff_SGS_term_labels
      use m_filtered_field_labels
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
!      if(b_trns%base%i_temp.eq.0 .or. ipol%base%i_per_temp.gt.0) then
      call add_field_name_4_sph_trns_snap(temperature,                  &
     &    ipol%base%i_temp, iphys%base%i_temp, b_trns%base%i_temp,      &
     &    trns_back)
!      end if
!      if(b_trns%base%i_light .eq. 0) then
      call add_field_name_4_sph_trns_snap(composition,                  &
     &    ipol%base%i_light, iphys%base%i_light, b_trns%base%i_light,   &
     &    trns_back)
!      end if
!
      call add_field_name_4_sph_trns_snap(pressure,                     &
     &    ipol%base%i_press, iphys%base%i_press, b_trns%base%i_press,   &
     &    trns_back)
      call add_field_name_4_sph_trns_nofld                              &
     &   (perturbation_temp%name, perturbation_temp%n_comp,             &
     &    ipol%base%i_per_temp, iphys%base%i_per_temp,                  &
     &    b_trns%base%i_per_temp, trns_back)
      call add_field_name_4_sph_trns_snap(filter_temperature,           &
     &    ipol%filter_fld%i_temp, iphys%filter_fld%i_temp,              &
     &    b_trns%filter_fld%i_temp, trns_back)
      call add_field_name_4_sph_trns_snap(thermal_diffusion,            &
     &    ipol%diffusion%i_t_diffuse, iphys%diffusion%i_t_diffuse,      &
     &    b_trns%diffusion%i_t_diffuse, trns_back)
      call add_field_name_4_sph_trns_snap(composition_diffusion,        &
     &    ipol%diffusion%i_c_diffuse, iphys%diffusion%i_c_diffuse,      &
     &    b_trns%diffusion%i_c_diffuse, trns_back)
!
      call add_field_name_4_sph_trns_snap(heat_advect,                  &
     &    ipol%forces%i_h_advect, iphys%forces%i_h_advect,              &
     &    b_trns%forces%i_h_advect, trns_back)
      call add_field_name_4_sph_trns_snap(composition_advect,           &
     &    ipol%forces%i_c_advect, iphys%forces%i_c_advect,              &
     &    b_trns%forces%i_c_advect, trns_back)
!
      call add_field_name_4_sph_trns_snap(div_Coriolis_force,           &
     &    ipol%div_forces%i_Coriolis, iphys%div_forces%i_Coriolis,      &
     &    b_trns%div_forces%i_Coriolis, trns_back)
!
      call add_field_name_4_sph_trns_snap(div_SGS_inertia,              &
     &    ipol%div_SGS%i_SGS_inertia, iphys%div_SGS%i_SGS_inertia,      &
     &    b_trns%div_SGS%i_SGS_inertia, trns_back)
      call add_field_name_4_sph_trns_snap(div_SGS_Lorentz,              &
     &    ipol%div_SGS%i_SGS_Lorentz, iphys%div_SGS%i_SGS_Lorentz,      &
     &    b_trns%div_SGS%i_SGS_Lorentz, trns_back)
      call add_field_name_4_sph_trns_snap(div_SGS_h_flux,               &
     &    ipol%div_SGS%i_SGS_h_flux, iphys%div_SGS%i_SGS_h_flux,        &
     &    b_trns%div_SGS%i_SGS_h_flux, trns_back)
      call add_field_name_4_sph_trns_snap(div_SGS_c_flux,               &
     &    ipol%div_SGS%i_SGS_c_flux, iphys%div_SGS%i_SGS_c_flux,        &
     &    b_trns%div_SGS%i_SGS_c_flux, trns_back)
      trns_back%num_scalar = trns_back%nfield - trns_back%num_vector
!
      end subroutine b_trans_address_scalar_snap
!
!-----------------------------------------------------------------------
!
      end module address_bwd_sph_trans_snap

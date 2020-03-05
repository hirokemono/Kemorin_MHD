!>@file   address_fwd_sph_trans_snap.f90
!!@brief  module address_fwd_sph_trans_snap
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine f_trans_address_vector_snap                          &
!!     &         (ipol, itor, iphys, f_trns, trns_fwd)
!!      subroutine f_trans_address_scalar_snap                          &
!!     &         (ipol, itor, iphys, f_trns, trns_fwd)
!!        type(phys_address), intent(in) :: ipol, itor, iphys
!!        type(address_each_sph_trans), intent(inout) :: trns_fwd
!!        type(phys_address), intent(inout) :: f_trns
!!@endverbatim
!
      module address_fwd_sph_trans_snap
!
      use m_precision
!
      use m_phys_labels
      use m_phys_constants
      use t_phys_address
      use t_addresses_sph_transform
      use t_control_parameter
      use t_physical_property
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_vector_snap                            &
     &         (ipol, itor, iphys, f_trns, trns_fwd)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_fwd
      type(phys_address), intent(inout) :: f_trns
!
!
      trns_fwd%nfield = 0
      call alloc_sph_trns_field_name(trns_fwd)
!
      call add_field_name_4_sph_trns_snap                               &
     &   (Coriolis_force%name, Coriolis_force%n_comp,                   &
     &    ipol%forces%i_coriolis, itor%forces%i_coriolis,               &
     &    iphys%forces%i_coriolis, f_trns%forces%i_coriolis, trns_fwd)
      call add_field_name_4_sph_trns_snap(fhd_e_field, n_vector,        &
     &    ipol%i_electric, itor%i_electric, iphys%i_electric,           &
     &    f_trns%i_electric, trns_fwd)
      call add_field_name_4_sph_trns_snap(fhd_poynting, n_vector,       &
     &    ipol%i_poynting, itor%i_poynting, iphys%i_poynting,           &
     &    f_trns%i_poynting, trns_fwd)
      call add_field_name_4_sph_trns_snap                               &
     &   (magnetic_stretch%name, magnetic_stretch%n_comp,               &
     &    ipol%forces%i_mag_stretch, itor%forces%i_mag_stretch,         &
     &    iphys%forces%i_mag_stretch, f_trns%forces%i_mag_stretch,      &
     &    trns_fwd)
!
      call add_field_name_4_sph_trns_snap(fhd_square_v, n_vector,       &
     &    ipol%i_square_v, itor%i_square_v, iphys%i_square_v,           &
     &    f_trns%i_square_v, trns_fwd)
      call add_field_name_4_sph_trns_snap(fhd_square_w, n_vector,       &
     &    ipol%i_square_w, itor%i_square_w, iphys%i_square_w,           &
     &    f_trns%i_square_w, trns_fwd)
      call add_field_name_4_sph_trns_snap(fhd_square_b, n_vector,       &
     &    ipol%i_square_b, itor%i_square_b, iphys%i_square_b,           &
     &    f_trns%i_square_b, trns_fwd)
      call add_field_name_4_sph_trns_snap(fhd_square_a, n_vector,       &
     &    ipol%i_square_a, itor%i_square_a, iphys%i_square_a,           &
     &    f_trns%i_square_a, trns_fwd)
      call add_field_name_4_sph_trns_snap(fhd_square_j, n_vector,       &
     &    ipol%i_square_j, itor%i_square_j, iphys%i_square_j,           &
     &    f_trns%i_square_j, trns_fwd)
!
      trns_fwd%num_vector = trns_fwd%nfield
!
      end subroutine f_trans_address_vector_snap
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_scalar_snap                            &
     &         (ipol, itor, iphys, f_trns, trns_fwd)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_fwd
      type(phys_address), intent(inout) :: f_trns
!
!
      call add_field_name_4_sph_trns_snap                               &
     &   (magnetic_ene_generation%name, magnetic_ene_generation%n_comp, &
     &    ipol%i_me_gen, itor%i_me_gen, iphys%i_me_gen,                 &
     &    f_trns%i_me_gen, trns_fwd)
      call add_field_name_4_sph_trns_snap                               &
     &   (Lorentz_work%name, Lorentz_work%n_comp,                       &
     &    ipol%i_ujb, itor%i_ujb, iphys%i_ujb, f_trns%i_ujb, trns_fwd)
      call add_field_name_4_sph_trns_snap                               &
     &   (work_against_Lorentz%name, work_against_Lorentz%n_comp,       &
     &    ipol%i_nega_ujb, itor%i_nega_ujb, iphys%i_nega_ujb,           &
     &    f_trns%i_nega_ujb, trns_fwd)
      call add_field_name_4_sph_trns_snap                               &
     &   (buoyancy_flux%name, buoyancy_flux%n_comp,                     &
     &    ipol%i_buo_gen, itor%i_buo_gen, iphys%i_buo_gen,              &
     &    f_trns%i_buo_gen, trns_fwd)
      call add_field_name_4_sph_trns_snap                               &
     &   (composite_buoyancy_flux%name, composite_buoyancy_flux%n_comp, &
     &    ipol%i_c_buo_gen, itor%i_c_buo_gen, iphys%i_c_buo_gen,        &
     &    f_trns%i_c_buo_gen, trns_fwd)
      call add_field_name_4_sph_trns_snap                               &
     &   (filtered_buoyancy_flux%name, n_scalar,                        &
     &    ipol%eflux_by_filter%i_buo_gen,                               &
     &    itor%eflux_by_filter%i_buo_gen,                               &
     &    iphys%eflux_by_filter%i_buo_gen,                              &
     &    f_trns%eflux_by_filter%i_buo_gen, trns_fwd)
      call add_field_name_4_sph_trns_snap                               &
     &   (filtered_comp_buoyancy_flux%name, n_scalar,                   &
     &    ipol%eflux_by_filter%i_c_buo_gen,                             &
     &    itor%eflux_by_filter%i_c_buo_gen,                             &
     &    iphys%eflux_by_filter%i_c_buo_gen,                            &
     &    f_trns%eflux_by_filter%i_c_buo_gen, trns_fwd)
!
      call add_field_name_4_sph_trns_snap                               &
     &   (fhd_magnetic_helicity, n_scalar,                              &
     &    ipol%i_k_heli, itor%i_k_heli, iphys%i_k_heli,                 &
     &    f_trns%i_k_heli, trns_fwd)
      call add_field_name_4_sph_trns_snap                               &
     &   (fhd_current_helicity, n_scalar,                               &
     &    ipol%i_c_heli, itor%i_c_heli, iphys%i_c_heli,                 &
     &    f_trns%i_c_heli, trns_fwd)
      call add_field_name_4_sph_trns_snap                               &
     &   (fhd_cross_helicity, n_scalar,                                 &
     &    ipol%i_x_heli, itor%i_x_heli, iphys%i_x_heli,                 &
     &    f_trns%i_x_heli, trns_fwd)
!
      call add_field_name_4_sph_trns_snap                               &
     &   (Reynolds_work%name, Reynolds_work%n_comp,                     &
     &    ipol%SGS_ene_flux%i_reynolds_wk,                              &
     &    itor%SGS_ene_flux%i_reynolds_wk,                              &
     &    iphys%SGS_ene_flux%i_reynolds_wk,                             &
     &    f_trns%SGS_ene_flux%i_reynolds_wk, trns_fwd)
!
      call add_field_name_4_sph_trns_snap                               &
     &   (SGS_Lorentz_work%name, SGS_Lorentz_work%n_comp,               &
     &    ipol%SGS_ene_flux%i_SGS_Lor_wk,                               &
     &    itor%SGS_ene_flux%i_SGS_Lor_wk,                               &
     &    iphys%SGS_ene_flux%i_SGS_Lor_wk,                              &
     &    f_trns%SGS_ene_flux%i_SGS_Lor_wk, trns_fwd)
      call add_field_name_4_sph_trns_snap                               &
     &   (SGS_mag_induction_flux%name, SGS_mag_induction_flux%n_comp,   &
     &    ipol%SGS_ene_flux%i_SGS_me_gen,                               &
     &    itor%SGS_ene_flux%i_SGS_me_gen,                               &
     &    iphys%SGS_ene_flux%i_SGS_me_gen,                              &
     &    f_trns%SGS_ene_flux%i_SGS_me_gen, trns_fwd)
!
      call add_field_name_4_sph_trns_snap                               &
     &   (SGS_buoyancy_flux%name, SGS_buoyancy_flux%n_comp,             &
     &    ipol%SGS_ene_flux%i_SGS_buo_wk,                               &
     &    itor%SGS_ene_flux%i_SGS_buo_wk,                               &
     &    iphys%SGS_ene_flux%i_SGS_buo_wk,                              &
     &    f_trns%SGS_ene_flux%i_SGS_buo_wk, trns_fwd)
      call add_field_name_4_sph_trns_snap                               &
     &   (SGS_comp_buoyancy_flux%name, SGS_comp_buoyancy_flux%n_comp,   &
     &    ipol%SGS_ene_flux%i_SGS_comp_buo_wk,                          &
     &    itor%SGS_ene_flux%i_SGS_comp_buo_wk,                          &
     &    iphys%SGS_ene_flux%i_SGS_comp_buo_wk,                         &
     &    f_trns%SGS_ene_flux%i_SGS_comp_buo_wk, trns_fwd)
!
      call add_field_name_4_sph_trns_snap                               &
     &   (Csim_SGS_heat_flux%name, Csim_SGS_heat_flux%n_comp,           &
     &    ipol%Csim%i_SGS_h_flux, itor%Csim%i_SGS_h_flux,               &
     &    iphys%Csim%i_SGS_h_flux, f_trns%Csim%i_SGS_h_flux, trns_fwd)
      call add_field_name_4_sph_trns_snap                               &
     &   (Csim_SGS_composit_flux%name, Csim_SGS_composit_flux%n_comp,   &
     &    ipol%Csim%i_SGS_c_flux, itor%Csim%i_SGS_c_flux,               &
     &    iphys%Csim%i_SGS_c_flux, f_trns%Csim%i_SGS_c_flux, trns_fwd)
      call add_field_name_4_sph_trns_snap                               &
     &   (Csim_SGS_inertia%name, Csim_SGS_inertia%n_comp,               &
     &    ipol%Csim%i_SGS_m_flux, itor%Csim%i_SGS_m_flux,               &
     &    iphys%Csim%i_SGS_m_flux, f_trns%Csim%i_SGS_m_flux, trns_fwd)
      call add_field_name_4_sph_trns_snap                               &
     &   (Csim_SGS_Lorentz%name, Csim_SGS_Lorentz%n_comp,               &
     &    ipol%Csim%i_SGS_Lorentz, itor%Csim%i_SGS_Lorentz,             &
     &    iphys%Csim%i_SGS_Lorentz, f_trns%Csim%i_SGS_Lorentz,          &
     &    trns_fwd)
      call add_field_name_4_sph_trns_snap                               &
     &   (Csim_SGS_induction%name, Csim_SGS_induction%n_comp,           &
     &    ipol%Csim%i_SGS_vp_induct, itor%Csim%i_SGS_vp_induct,         &
     &    iphys%Csim%i_SGS_vp_induct, f_trns%Csim%i_SGS_vp_induct,      &
     &    trns_fwd)
      call add_field_name_4_sph_trns_snap                               &
     &   (Csim_SGS_buoyancy%name, Csim_SGS_buoyancy%n_comp,             &
     &    ipol%Csim%i_SGS_buoyancy, itor%Csim%i_SGS_buoyancy,           &
     &    iphys%Csim%i_SGS_buoyancy, f_trns%Csim%i_SGS_buoyancy,        &
     &    trns_fwd)
      call add_field_name_4_sph_trns_snap                               &
     &   (Csim_SGS_composit_buo%name, Csim_SGS_composit_buo%n_comp,     &
     &    ipol%Csim%i_SGS_comp_buo, itor%Csim%i_SGS_comp_buo,           &
     &    iphys%Csim%i_SGS_comp_buo, f_trns%Csim%i_SGS_comp_buo,        &
     &    trns_fwd)
!
      call add_field_name_4_sph_trns_snap(fhd_velocity_scale, n_scalar, &
     &    ipol%i_velo_scale, itor%i_velo_scale, iphys%i_velo_scale,     &
     &    f_trns%i_velo_scale, trns_fwd)
      call add_field_name_4_sph_trns_snap(fhd_magnetic_scale, n_scalar, &
     &    ipol%i_magne_scale, itor%i_magne_scale, iphys%i_magne_scale,  &
     &    f_trns%i_magne_scale, trns_fwd)
      call add_field_name_4_sph_trns_snap(fhd_temp_scale, n_scalar,     &
     &    ipol%i_temp_scale, itor%i_temp_scale, iphys%i_temp_scale,     &
     &    f_trns%i_temp_scale, trns_fwd)
      call add_field_name_4_sph_trns_snap                               &
     &   (fhd_composition_scale, n_scalar,                              &
     &    ipol%i_comp_scale, itor%i_comp_scale, iphys%i_comp_scale,     &
     &    f_trns%i_comp_scale, trns_fwd)
!
      call add_field_name_4_sph_trns_snap(fhd_square_t, n_scalar,       &
     &    ipol%i_square_t, itor%i_square_t, iphys%i_square_t,           &
     &    f_trns%i_square_t, trns_fwd)
      call add_field_name_4_sph_trns_snap(fhd_square_c, n_scalar,       &
     &    ipol%i_square_c, itor%i_square_c, iphys%i_square_c,           &
     &    f_trns%i_square_c, trns_fwd)
!
      trns_fwd%num_scalar = trns_fwd%nfield - trns_fwd%num_vector
!
      end subroutine f_trans_address_scalar_snap
!
!-----------------------------------------------------------------------
!
      end module address_fwd_sph_trans_snap

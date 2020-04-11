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
!!     &         (ipol, iphys, f_trns, trns_fwd)
!!      subroutine f_trans_address_scalar_snap                          &
!!     &         (ipol, iphys, f_trns, trns_fwd)
!!        type(phys_address), intent(in) :: ipol, iphys
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
     &         (ipol, iphys, f_trns, trns_fwd)
!
      use add_field_to_sph_trans_list
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_fwd
      type(phys_address), intent(inout) :: f_trns
!
!
      trns_fwd%nfield = 0
      call alloc_sph_trns_field_name(trns_fwd)
!
      call add_field_name_4_sph_trns_snap(Coriolis_force,               &
     &    ipol%forces%i_coriolis, iphys%forces%i_coriolis,              &
     &    f_trns%forces%i_coriolis, trns_fwd)
      call add_field_name_4_sph_trns_snap(electric_field,               &
     &    ipol%prod_fld%i_electric, iphys%prod_fld%i_electric,          &
     &    f_trns%prod_fld%i_electric, trns_fwd)
      call add_field_name_4_sph_trns_snap(poynting_flux,                &
     &    ipol%prod_fld%i_poynting, iphys%prod_fld%i_poynting,          &
     &    f_trns%prod_fld%i_poynting, trns_fwd)
      call add_field_name_4_sph_trns_snap(magnetic_stretch,             &
     &    ipol%forces%i_mag_stretch, iphys%forces%i_mag_stretch,        &
     &    f_trns%forces%i_mag_stretch, trns_fwd)
!
      call add_field_name_4_sph_trns_snap(square_velocity,              &
     &    ipol%prod_fld%i_square_v, iphys%prod_fld%i_square_v,          &
     &    f_trns%prod_fld%i_square_v, trns_fwd)
      call add_field_name_4_sph_trns_snap(square_vorticity,             &
     &    ipol%prod_fld%i_square_w, iphys%prod_fld%i_square_w,          &
     &    f_trns%prod_fld%i_square_w, trns_fwd)
      call add_field_name_4_sph_trns_snap(square_magne,                 &
     &    ipol%prod_fld%i_square_b, iphys%prod_fld%i_square_b,          &
     &    f_trns%prod_fld%i_square_b, trns_fwd)
      call add_field_name_4_sph_trns_snap(square_vector_potential,      &
     &    ipol%prod_fld%i_square_a, iphys%prod_fld%i_square_a,          &
     &    f_trns%prod_fld%i_square_a, trns_fwd)
      call add_field_name_4_sph_trns_snap(square_current,               &
     &    ipol%prod_fld%i_square_j, iphys%prod_fld%i_square_j,          &
     &    f_trns%prod_fld%i_square_j, trns_fwd)
!
      trns_fwd%num_vector = trns_fwd%nfield
!
      end subroutine f_trans_address_vector_snap
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_scalar_snap                            &
     &         (ipol, iphys, f_trns, trns_fwd)
!
      use m_filtered_ene_flux_labels
      use add_field_to_sph_trans_list
      use add_SGS_term_to_sph_trans
      use add_Csim_4_sph_trns
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_fwd
      type(phys_address), intent(inout) :: f_trns
!
!
      call add_field_name_4_sph_trns_snap(magnetic_ene_generation,      &
     &    ipol%ene_flux%i_me_gen, iphys%ene_flux%i_me_gen,              &
     &    f_trns%ene_flux%i_me_gen, trns_fwd)
      call add_field_name_4_sph_trns_snap(Lorentz_work,                 &
     &    ipol%ene_flux%i_ujb, iphys%ene_flux%i_ujb,                    &
     &    f_trns%ene_flux%i_ujb, trns_fwd)
      call add_field_name_4_sph_trns_snap(work_against_Lorentz,         &
     &    ipol%ene_flux%i_nega_ujb, iphys%ene_flux%i_nega_ujb,          &
     &    f_trns%ene_flux%i_nega_ujb, trns_fwd)
      call add_field_name_4_sph_trns_snap(buoyancy_flux,                &
     &    ipol%ene_flux%i_buo_gen, iphys%ene_flux%i_buo_gen,            &
     &    f_trns%ene_flux%i_buo_gen, trns_fwd)
      call add_field_name_4_sph_trns_snap(composite_buoyancy_flux,      &
     &    ipol%ene_flux%i_c_buo_gen, iphys%ene_flux%i_c_buo_gen,        &
     &    f_trns%ene_flux%i_c_buo_gen, trns_fwd)
      call add_field_name_4_sph_trns_snap(filtered_buoyancy_flux,       &
     &    ipol%eflux_by_filter%i_buo_gen,                               &
     &    iphys%eflux_by_filter%i_buo_gen,                              &
     &    f_trns%eflux_by_filter%i_buo_gen, trns_fwd)
      call add_field_name_4_sph_trns_snap(filtered_comp_buoyancy_flux,  &
     &    ipol%eflux_by_filter%i_c_buo_gen,                             &
     &    iphys%eflux_by_filter%i_c_buo_gen,                            &
     &    f_trns%eflux_by_filter%i_c_buo_gen, trns_fwd)
!
      call add_field_name_4_sph_trns_snap(kinetic_helicity,             &
     &    ipol%prod_fld%i_k_heli, iphys%prod_fld%i_k_heli,              &
     &    f_trns%prod_fld%i_k_heli, trns_fwd)
      call add_field_name_4_sph_trns_snap(magnetic_helicity,            &
     &    ipol%prod_fld%i_m_heli, iphys%prod_fld%i_m_heli,              &
     &    f_trns%prod_fld%i_m_heli, trns_fwd)
      call add_field_name_4_sph_trns_snap(current_helicity,             &
     &    ipol%prod_fld%i_c_heli, iphys%prod_fld%i_c_heli,              &
     &    f_trns%prod_fld%i_c_heli, trns_fwd)
      call add_field_name_4_sph_trns_snap(cross_helicity,               &
     &    ipol%prod_fld%i_x_heli, iphys%prod_fld%i_x_heli,              &
     &    f_trns%prod_fld%i_x_heli, trns_fwd)
!
!
      call add_SGS_eflux_sph_trns_snap                                  &
     &   (ipol%SGS_ene_flux, iphys%SGS_ene_flux, f_trns%SGS_ene_flux,   &
     &    trns_fwd)
!
!
      call add_Csim_4_sph_trns_snap                                     &
     &   (ipol%Csim, iphys%Csim, f_trns%Csim, trns_fwd)
!
      call add_field_name_4_sph_trns_snap(velocity_scale,               &
     &    ipol%prod_fld%i_velo_scale, iphys%prod_fld%i_velo_scale,      &
     &    f_trns%prod_fld%i_velo_scale, trns_fwd)
      call add_field_name_4_sph_trns_snap(magnetic_scale,               &
     &    ipol%prod_fld%i_magne_scale, iphys%prod_fld%i_magne_scale,    &
     &    f_trns%prod_fld%i_magne_scale, trns_fwd)
      call add_field_name_4_sph_trns_snap(temperature_scale,            &
     &    ipol%prod_fld%i_temp_scale, iphys%prod_fld%i_temp_scale,      &
     &    f_trns%prod_fld%i_temp_scale, trns_fwd)
      call add_field_name_4_sph_trns_snap(composition_scale,            &
     &    ipol%prod_fld%i_comp_scale, iphys%prod_fld%i_comp_scale,      &
     &    f_trns%prod_fld%i_comp_scale, trns_fwd)
!
      call add_field_name_4_sph_trns_snap(square_temperature,           &
     &    ipol%prod_fld%i_square_t, iphys%prod_fld%i_square_t,          &
     &    f_trns%prod_fld%i_square_t, trns_fwd)
      call add_field_name_4_sph_trns_snap(square_composition,           &
     &    ipol%prod_fld%i_square_c, iphys%prod_fld%i_square_c,          &
     &    f_trns%prod_fld%i_square_c, trns_fwd)
!
      trns_fwd%num_scalar = trns_fwd%nfield - trns_fwd%num_vector
!
      end subroutine f_trans_address_scalar_snap
!
!-----------------------------------------------------------------------
!
      end module address_fwd_sph_trans_snap

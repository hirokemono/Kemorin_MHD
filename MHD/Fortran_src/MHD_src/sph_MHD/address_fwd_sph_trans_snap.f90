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
!!     &         (fl_prop, cd_prop, ht_prop, cp_prop, ipol, trns_snap)
!!      subroutine f_trans_address_scalar_snap(fl_prop, trns_snap)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in)  :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_address), intent(inout) :: b_trns
!!
!!      subroutine set_f_trans_vector_field_snap                        &
!!     &         (icou, ipol, iphys, trns_snap)
!!      subroutine set_f_trans_scalar_field_snap                        &
!!     &         (icou, ipol, iphys, trns_snap)
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_snap
!!@endverbatim
!
      module address_fwd_sph_trans_snap
!
      use m_precision
!
      use m_phys_labels
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
      subroutine f_trans_address_vector_snap(ipol, iphys, trns_snap)
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_4_sph_trans), intent(inout) :: trns_snap
!
!
      trns_snap%nvector_rtp_2_rj = 0
      call add_vec_trans_flag_snap(ipol%i_coriolis, iphys%i_coriolis,   &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%f_trns%i_coriolis)
      call add_vec_trans_flag_snap(ipol%i_electric, iphys%i_electric,   &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%f_trns%i_electric)
      call add_vec_trans_flag_snap(ipol%i_poynting, iphys%i_poynting,   &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%f_trns%i_poynting)
      call add_vec_trans_flag_snap                                      &
     &   (ipol%i_mag_stretch, iphys%i_mag_stretch,                      &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%f_trns%i_mag_stretch)
!
      end subroutine f_trans_address_vector_snap
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_scalar_snap(ipol, iphys, trns_snap)
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_4_sph_trans), intent(inout) :: trns_snap
!
!
      trns_snap%nscalar_rtp_2_rj = 0
      call add_scl_trans_flag_snap(ipol%i_me_gen, iphys%i_me_gen,       &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%nscalar_rtp_2_rj,       &
     &    trns_snap%f_trns%i_me_gen)
      call add_scl_trans_flag_snap(ipol%i_ujb, iphys%i_ujb,             &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%nscalar_rtp_2_rj,       &
     &    trns_snap%f_trns%i_ujb)
      call add_scl_trans_flag_snap(ipol%i_nega_ujb, iphys%i_nega_ujb,   &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%nscalar_rtp_2_rj,       &
     &    trns_snap%f_trns%i_nega_ujb)
!
      call add_scl_trans_flag_snap(ipol%i_buo_gen, iphys%i_buo_gen,     &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%nscalar_rtp_2_rj,       &
     &    trns_snap%f_trns%i_buo_gen)
      call add_scl_trans_flag_snap(ipol%i_c_buo_gen, iphys%i_c_buo_gen, &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%nscalar_rtp_2_rj,       &
     &    trns_snap%f_trns%i_c_buo_gen)
      call add_scl_trans_flag_snap(ipol%i_f_buo_gen, iphys%i_f_buo_gen, &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%nscalar_rtp_2_rj,       &
     &    trns_snap%f_trns%i_f_buo_gen)
!
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_reynolds_wk, iphys%i_reynolds_wk,                      &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%nscalar_rtp_2_rj,       &
     &    trns_snap%f_trns%i_reynolds_wk)
!
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_SGS_Lor_wk, iphys%i_SGS_Lor_wk,                        &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%nscalar_rtp_2_rj,       &
     &    trns_snap%f_trns%i_SGS_Lor_wk)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_SGS_me_gen, iphys%i_SGS_me_gen,                        &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%nscalar_rtp_2_rj,       &
     &    trns_snap%f_trns%i_SGS_me_gen)
!
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_SGS_buo_wk, iphys%i_SGS_buo_wk,                        &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%nscalar_rtp_2_rj,       &
     &    trns_snap%f_trns%i_SGS_buo_wk)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_SGS_comp_buo_wk, iphys%i_SGS_comp_buo_wk,              &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%nscalar_rtp_2_rj,       &
     &    trns_snap%f_trns%i_SGS_comp_buo_wk)
!
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_Csim_SGS_h_flux, iphys%i_Csim_SGS_h_flux,              &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%nscalar_rtp_2_rj,       &
     &    trns_snap%f_trns%i_Csim_SGS_h_flux)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_Csim_SGS_c_flux, iphys%i_Csim_SGS_c_flux,              &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%nscalar_rtp_2_rj,       &
     &    trns_snap%f_trns%i_Csim_SGS_c_flux)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_Csim_SGS_m_flux, iphys%i_Csim_SGS_m_flux,              &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%nscalar_rtp_2_rj,       &
     &    trns_snap%f_trns%i_Csim_SGS_m_flux)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_Csim_SGS_Lorentz, iphys%i_Csim_SGS_Lorentz,            &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%nscalar_rtp_2_rj,       &
     &    trns_snap%f_trns%i_Csim_SGS_Lorentz)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_Csim_SGS_induction, iphys%i_Csim_SGS_induction,        &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%nscalar_rtp_2_rj,       &
     &    trns_snap%f_trns%i_Csim_SGS_induction)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_Csim_SGS_buoyancy, iphys%i_Csim_SGS_buoyancy,          &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%nscalar_rtp_2_rj,       &
     &    trns_snap%f_trns%i_Csim_SGS_buoyancy)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_Csim_SGS_comp_buo, iphys%i_Csim_SGS_comp_buo,          &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%nscalar_rtp_2_rj,       &
     &    trns_snap%f_trns%i_Csim_SGS_comp_buo)
!
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_velo_scale, iphys%i_velo_scale,                        &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%nscalar_rtp_2_rj,       &
     &    trns_snap%f_trns%i_velo_scale)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_magne_scale, iphys%i_magne_scale,                      &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%nscalar_rtp_2_rj,       &
     &    trns_snap%f_trns%i_magne_scale)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_temp_scale, iphys%i_temp_scale,                        &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%nscalar_rtp_2_rj,       &
     &    trns_snap%f_trns%i_temp_scale)
      call add_scl_trans_flag_snap                                      &
     &   (ipol%i_comp_scale, iphys%i_comp_scale,                        &
     &    trns_snap%nvector_rtp_2_rj, trns_snap%nscalar_rtp_2_rj,       &
     &    trns_snap%f_trns%i_comp_scale)
!
      end subroutine f_trans_address_scalar_snap
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_f_trans_vector_field_snap                          &
     &         (icou, ipol, iphys, trns_snap)
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_4_sph_trans), intent(inout) :: trns_snap
      integer(kind = kint), intent(inout) :: icou
!
!
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_Coriolis, trns_snap%f_trns%i_coriolis,                    &
     &    ipol%i_coriolis, iphys%i_coriolis, icou, trns_snap)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_e_field, trns_snap%f_trns%i_electric,                     &
     &    ipol%i_electric, iphys%i_electric, icou, trns_snap)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_poynting, trns_snap%f_trns%i_poynting,                    &
     &    ipol%i_poynting, iphys%i_poynting, icou, trns_snap)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_mag_stretch, trns_snap%f_trns%i_mag_stretch,              &
     &    ipol%i_mag_stretch, iphys%i_mag_stretch, icou, trns_snap)
!
      end subroutine set_f_trans_vector_field_snap
!
!-----------------------------------------------------------------------
!
      subroutine set_f_trans_scalar_field_snap                          &
     &         (icou, ipol, iphys, trns_snap)
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_4_sph_trans), intent(inout) :: trns_snap
      integer(kind = kint), intent(inout) :: icou
!
!
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_mag_ene_gen, trns_snap%f_trns%i_me_gen,                   &
     &    ipol%i_me_gen, iphys%i_me_gen, icou, trns_snap)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_Lorentz_work, trns_snap%f_trns%i_ujb,                     &
     &    ipol%i_ujb, iphys%i_ujb, icou, trns_snap)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_work_agst_Lorentz, trns_snap%f_trns%i_nega_ujb,           &
     &    ipol%i_nega_ujb, iphys%i_nega_ujb, icou, trns_snap)
!
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_buoyancy_flux, trns_snap%f_trns%i_buo_gen,                &
     &    ipol%i_buo_gen, iphys%i_buo_gen, icou, trns_snap)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_comp_buo_flux, trns_snap%f_trns%i_c_buo_gen,              &
     &    ipol%i_c_buo_gen, iphys%i_c_buo_gen, icou, trns_snap)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_filter_buo_flux, trns_snap%f_trns%i_f_buo_gen,            &
     &    ipol%i_f_buo_gen, iphys%i_f_buo_gen, icou, trns_snap)
!
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_Reynolds_work, trns_snap%f_trns%i_reynolds_wk,            &
     &    ipol%i_reynolds_wk, iphys%i_reynolds_wk, icou, trns_snap)
!
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_SGS_Lorentz_work, trns_snap%f_trns%i_SGS_Lor_wk,          &
     &    ipol%i_SGS_Lor_wk, iphys%i_SGS_Lor_wk, icou, trns_snap)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_SGS_m_ene_gen, trns_snap%f_trns%i_SGS_me_gen,             &
     &    ipol%i_SGS_me_gen, iphys%i_SGS_me_gen, icou, trns_snap)
!
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_SGS_buo_flux, trns_snap%f_trns%i_SGS_buo_wk,              &
     &    ipol%i_SGS_buo_wk, iphys%i_SGS_buo_wk, icou, trns_snap)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_SGS_comp_buo_flux, trns_snap%f_trns%i_SGS_comp_buo_wk,    &
     &    ipol%i_SGS_comp_buo_wk, iphys%i_SGS_comp_buo_wk,              &
     &    icou, trns_snap)
!
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_Csim_SGS_h_flux, trns_snap%f_trns%i_Csim_SGS_h_flux,      &
     &    ipol%i_Csim_SGS_h_flux, iphys%i_Csim_SGS_h_flux,              &
     &    icou, trns_snap)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_Csim_SGS_c_flux, trns_snap%f_trns%i_Csim_SGS_c_flux,      &
     &    ipol%i_Csim_SGS_c_flux, iphys%i_Csim_SGS_c_flux,              &
     &    icou, trns_snap)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_Csim_SGS_m_flux, trns_snap%f_trns%i_Csim_SGS_m_flux,      &
     &    ipol%i_Csim_SGS_m_flux, iphys%i_Csim_SGS_m_flux,              &
     &    icou, trns_snap)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_Csim_SGS_Lorentz, trns_snap%f_trns%i_Csim_SGS_Lorentz,    &
     &    ipol%i_Csim_SGS_Lorentz, iphys%i_Csim_SGS_Lorentz,            &
     &    icou, trns_snap)
      call set_field_name_4_fwd_trns(fhd_Csim_SGS_induction,            &
     &    trns_snap%f_trns%i_Csim_SGS_induction,                        &
     &    ipol%i_Csim_SGS_induction, iphys%i_Csim_SGS_induction,        &
     &    icou, trns_snap)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_Csim_SGS_buoyancy, trns_snap%f_trns%i_Csim_SGS_buoyancy,  &
     &    ipol%i_Csim_SGS_buoyancy, iphys%i_Csim_SGS_buoyancy,          &
     &    icou, trns_snap)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_Csim_SGS_comp_buo, trns_snap%f_trns%i_Csim_SGS_comp_buo,  &
     &    ipol%i_Csim_SGS_comp_buo, iphys%i_Csim_SGS_comp_buo,          &
      &   icou, trns_snap)
!
      end subroutine set_f_trans_scalar_field_snap
!
!-----------------------------------------------------------------------
!
      end module address_fwd_sph_trans_snap

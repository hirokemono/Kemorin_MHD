!>@file   address_bwd_sph_trans_SGS.f90
!!@brief  module address_bwd_sph_trans_SGS
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine b_trans_vector_similarity                            &
!!     &         (ipol, itor, iphys, b_trns, trns_back)
!!      subroutine b_trans_vector_wide_filter_fld                       &
!!     &         (ipol, itor, iphys, b_trns, trns_back)
!!      subroutine b_trans_vector_wide_similarity                       &
!!     &         (ipol, itor, iphys, b_trns, trns_back)
!!
!!      subroutine b_trans_vector_filtered_SGS                          &
!!     &         (ipol, itor, iphys, b_trns, trns_back)
!!
!!      subroutine b_trans_scalar_similarity                            &
!!     &         (ipol, itor, iphys, b_trns, trns_back)
!!      subroutine b_trans_scalar_wide_filter_fld                       &
!!     &         (ipol, itor, iphys, b_trns, trns_back)
!!        type(phys_address), intent(in) :: ipol, itor, iphys
!!        type(address_each_sph_trans), intent(inout) :: trns_back
!!        type(phys_address), intent(inout) :: b_trns
!!@endverbatim
!
      module address_bwd_sph_trans_SGS
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
      subroutine b_trans_vector_similarity                              &
     &         (ipol, itor, iphys, b_trns, trns_back)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
!   filtered velocity
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_filter_velo, fhd_filter_velo, n_vector,                &
     &    ipol%i_filter_velo, itor%i_filter_velo, iphys%i_filter_velo,  &
     &    b_trns%i_filter_velo, trns_back)
!   filtered vorticity
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_filter_vort, fhd_filter_vort, n_vector,                &
     &    ipol%i_filter_vort, itor%i_filter_vort, iphys%i_filter_vort,  &
     &    b_trns%i_filter_vort, trns_back)
!   filtered magnetic field
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_filter_magne, fhd_filter_magne, n_vector,              &
     &    ipol%i_filter_magne, itor%i_filter_magne,                     &
     &    iphys%i_filter_magne, b_trns%i_filter_magne, trns_back)
!   filtered current density
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_filter_current, fhd_filter_current, n_vector,          &
     &    ipol%i_filter_current, itor%i_filter_current,                 &
     &    iphys%i_filter_current, b_trns%i_filter_current, trns_back)
!
!   filtered Inertia
      call add_field_name_4_sph_trns(ipol%SGS_term%i_SGS_inertia,       &
     &    SGS_inertia%name, SGS_inertia%n_comp,                         &
     &    ipol%SGS_term%i_SGS_inertia, itor%SGS_term%i_SGS_inertia,     &
     &    iphys%SGS_term%i_SGS_inertia, b_trns%SGS_term%i_SGS_inertia,  &
     &    trns_back)
!   filtered Lorentz force
      call add_field_name_4_sph_trns(ipol%SGS_term%i_SGS_Lorentz,       &
     &    SGS_Lorentz%name, SGS_Lorentz%n_comp,                         &
     &    ipol%SGS_term%i_SGS_Lorentz, itor%SGS_term%i_SGS_Lorentz,     &
     &    iphys%SGS_term%i_SGS_Lorentz, b_trns%SGS_term%i_SGS_Lorentz,  &
     &    trns_back)
!   filtered induction
      call add_field_name_4_sph_trns(ipol%SGS_term%i_SGS_vp_induct,     &
     &    SGS_vecp_induction%name, SGS_vecp_induction%n_comp,           &
     &    ipol%SGS_term%i_SGS_vp_induct, itor%SGS_term%i_SGS_vp_induct, &
     &    iphys%SGS_term%i_SGS_vp_induct,                               &
     &    b_trns%SGS_term%i_SGS_vp_induct, trns_back)
!   filtered heat flux
      call add_field_name_4_sph_trns(ipol%SGS_term%i_SGS_h_flux,        &
     &    SGS_heat_flux%name, SGS_heat_flux%n_comp,                     &
     &    ipol%SGS_term%i_SGS_h_flux, itor%SGS_term%i_SGS_h_flux,       &
     &    iphys%SGS_term%i_SGS_h_flux, b_trns%SGS_term%i_SGS_h_flux,    &
     &    trns_back)
!   filtered composition flux
      call add_field_name_4_sph_trns(ipol%SGS_term%i_SGS_c_flux,        &
     &    SGS_composit_flux%name, SGS_composit_flux%n_comp,             &
     &    ipol%SGS_term%i_SGS_c_flux, itor%SGS_term%i_SGS_c_flux,       &
     &    iphys%SGS_term%i_SGS_c_flux, b_trns%SGS_term%i_SGS_c_flux,    &
     &    trns_back)
!
      end subroutine b_trans_vector_similarity
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_vector_wide_filter_fld                         &
     &         (ipol, itor, iphys, b_trns, trns_back)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!   wide filtered velocity
      call add_field_name_4_sph_trns(ipol%wide_filter_fld%i_velo,       &
     &    wide_filter_velocity%name, wide_filter_velocity%n_comp,       &
     &    ipol%wide_filter_fld%i_velo, itor%wide_filter_fld%i_velo,     &
     &    iphys%wide_filter_fld%i_velo, b_trns%wide_filter_fld%i_velo,  &
     &    trns_back)
!   wide filtered vorticity
      call add_field_name_4_sph_trns(ipol%wide_filter_fld%i_vort,       &
     &    wide_filter_vorticity%name, wide_filter_vorticity%n_comp,     &
     &    ipol%wide_filter_fld%i_vort, itor%wide_filter_fld%i_vort,     &
     &    iphys%wide_filter_fld%i_vort, b_trns%wide_filter_fld%i_vort,  &
     &    trns_back)
!   wide filtered magnetic field
      call add_field_name_4_sph_trns(ipol%wide_filter_fld%i_magne,      &
     &    wide_filter_magne%name, wide_filter_magne%n_comp,             &
     &    ipol%wide_filter_fld%i_magne, itor%wide_filter_fld%i_magne,   &
     &    iphys%wide_filter_fld%i_magne,                                &
     &    b_trns%wide_filter_fld%i_magne, trns_back)
!   wide filtered current density
      call add_field_name_4_sph_trns(ipol%wide_filter_fld%i_current,    &
     &    wide_filter_current%name, wide_filter_current%n_comp,         &
     &    ipol%wide_filter_fld%i_current,                               &
     &    itor%wide_filter_fld%i_current,                               &
     &    iphys%wide_filter_fld%i_current,                              &
     &    b_trns%wide_filter_fld%i_current, trns_back)
!
      end subroutine b_trans_vector_wide_filter_fld
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_vector_wide_similarity                         &
     &         (ipol, itor, iphys, b_trns, trns_back)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
!   wide filtered Inertia
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_wide_SGS_inertia, fhd_wide_SGS_inertia, n_vector,      &
     &    ipol%i_wide_SGS_inertia, itor%i_wide_SGS_inertia,             &
     &    iphys%i_wide_SGS_inertia, b_trns%i_wide_SGS_inertia,          &
     &    trns_back)
!   wide filtered Lorentz force
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_wide_SGS_Lorentz, fhd_wide_SGS_Lorentz, n_vector,      &
     &    ipol%i_wide_SGS_Lorentz, itor%i_wide_SGS_Lorentz,             &
     &    iphys%i_wide_SGS_Lorentz, b_trns%i_wide_SGS_Lorentz,          &
     &    trns_back)
!   wide filtered induction
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_wide_SGS_vp_induct, fhd_wide_SGS_vp_induct, n_vector,  &
     &    ipol%i_wide_SGS_vp_induct, itor%i_wide_SGS_vp_induct,         &
     &    iphys%i_wide_SGS_vp_induct, b_trns%i_wide_SGS_vp_induct,      &
     &    trns_back)
!   wide filtered heat flux
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_wide_SGS_h_flux, fhd_wide_SGS_h_flux, n_vector,        &
     &    ipol%i_wide_SGS_h_flux, itor%i_wide_SGS_h_flux,               &
     &    iphys%i_wide_SGS_h_flux, b_trns%i_wide_SGS_h_flux, trns_back)
!   wide filtered composition flux
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_wide_SGS_c_flux, fhd_wide_SGS_c_flux, n_vector,        &
     &    ipol%i_wide_SGS_c_flux, itor%i_wide_SGS_c_flux,               &
     &    iphys%i_wide_SGS_c_flux, b_trns%i_wide_SGS_c_flux, trns_back)
!
      end subroutine b_trans_vector_wide_similarity
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_vector_filtered_SGS                            &
     &         (ipol, itor, iphys, b_trns, trns_back)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
!   dual filtered Inertia
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_dbl_SGS_inertia, fhd_dbl_SGS_inertia, n_vector,        &
     &    ipol%i_dbl_SGS_inertia, itor%i_dbl_SGS_inertia,               &
     &    iphys%i_dbl_SGS_inertia, b_trns%i_dbl_SGS_inertia, trns_back)
!   dual filtered Lorentz force
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_dbl_SGS_Lorentz, fhd_dbl_SGS_Lorentz, n_vector,        &
     &    ipol%i_dbl_SGS_Lorentz, itor%i_dbl_SGS_Lorentz,               &
     &    iphys%i_dbl_SGS_Lorentz, b_trns%i_dbl_SGS_Lorentz, trns_back)
!   dual filtered induction
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_dbl_SGS_vp_induct, fhd_dbl_SGS_vp_induct, n_vector,    &
     &    ipol%i_dbl_SGS_vp_induct, itor%i_dbl_SGS_vp_induct,           &
     &    iphys%i_dbl_SGS_vp_induct, b_trns%i_dbl_SGS_vp_induct,        &
     &    trns_back)
!   dual filtered heat flux
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_dbl_SGS_h_flux, fhd_dbl_SGS_h_flux, n_vector,          &
     &    ipol%i_dbl_SGS_h_flux, itor%i_dbl_SGS_h_flux,                 &
     &    iphys%i_dbl_SGS_h_flux, b_trns%i_dbl_SGS_h_flux, trns_back)
!   dual filtered composition flux
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_dbl_SGS_c_flux, fhd_dbl_SGS_c_flux, n_vector,          &
     &    ipol%i_dbl_SGS_c_flux, itor%i_dbl_SGS_c_flux,                 &
     &    iphys%i_dbl_SGS_c_flux, b_trns%i_dbl_SGS_c_flux, trns_back)
!
      end subroutine b_trans_vector_filtered_SGS
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine b_trans_scalar_similarity                              &
     &         (ipol, itor, iphys, b_trns, trns_back)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
!   filtered temperature
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_filter_temp, fhd_filter_temp, n_scalar,                &
     &    ipol%i_filter_temp, itor%i_filter_temp, iphys%i_filter_temp,  &
     &    b_trns%i_filter_temp, trns_back)
!   filtered composition
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_filter_comp, fhd_filter_comp, n_scalar,                &
     &    ipol%i_filter_comp, itor%i_filter_comp, iphys%i_filter_comp,  &
     &    b_trns%i_filter_comp, trns_back)
!
      end subroutine b_trans_scalar_similarity
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_scalar_wide_filter_fld                         &
     &         (ipol, itor, iphys, b_trns, trns_back)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
!   wide filtered temperature
      call add_field_name_4_sph_trns(ipol%wide_filter_fld%i_temp,       &
     &    wide_filter_temp%name, wide_filter_temp%n_comp,               &
     &    ipol%wide_filter_fld%i_temp, itor%wide_filter_fld%i_temp,     &
     &    iphys%wide_filter_fld%i_temp, b_trns%wide_filter_fld%i_temp,  &
     &    trns_back)
!   wide filtered composition
      call add_field_name_4_sph_trns(ipol%wide_filter_fld%i_light,      &
     &    wide_filter_composition%name,                                 &
     &    wide_filter_composition%n_comp,                               &
     &    ipol%wide_filter_fld%i_light, itor%wide_filter_fld%i_light,   &
     &    iphys%wide_filter_fld%i_light,                                &
     &    b_trns%wide_filter_fld%i_light, trns_back)
!
      end subroutine b_trans_scalar_wide_filter_fld
!
!-----------------------------------------------------------------------
!
      end module address_bwd_sph_trans_SGS

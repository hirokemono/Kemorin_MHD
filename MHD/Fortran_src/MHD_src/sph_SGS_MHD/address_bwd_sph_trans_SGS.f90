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
!!     &         (ipol, iphys, b_trns, trns_back)
!!      subroutine b_trans_vector_wide_filter_fld                       &
!!     &         (ipol, iphys, b_trns, trns_back)
!!      subroutine b_trans_vector_wide_similarity                       &
!!     &         (ipol, iphys, b_trns, trns_back)
!!
!!      subroutine b_trans_vector_filtered_SGS                          &
!!     &         (ipol, iphys, b_trns, trns_back)
!!
!!      subroutine b_trans_scalar_similarity                            &
!!     &         (ipol, iphys, b_trns, trns_back)
!!      subroutine b_trans_scalar_wide_filter_fld                       &
!!     &         (ipol, iphys, b_trns, trns_back)
!!        type(phys_address), intent(in) :: ipol, iphys
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
     &         (ipol, iphys, b_trns, trns_back)
!
      use m_filtered_field_labels
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
!   filtered velocity
      call add_field_4_sph_trns_by_pol(filter_velocity,                 &
     &    ipol%filter_fld%i_velo, iphys%filter_fld%i_velo,              &
     &    b_trns%filter_fld%i_velo, trns_back)
!   filtered vorticity
      call add_field_4_sph_trns_by_pol(filter_vorticity,                &
     &    ipol%filter_fld%i_vort, iphys%filter_fld%i_vort,              &
     &    b_trns%filter_fld%i_vort, trns_back)
!   filtered magnetic field
      call add_field_4_sph_trns_by_pol(filter_magne,                    &
     &    ipol%filter_fld%i_magne, iphys%filter_fld%i_magne,            &
     &    b_trns%filter_fld%i_magne, trns_back)
!   filtered current density
      call add_field_4_sph_trns_by_pol(filter_current,                  &
     &    ipol%filter_fld%i_current, iphys%filter_fld%i_current,        &
     &    b_trns%filter_fld%i_current, trns_back)
!
!   filtered Inertia
      call add_field_4_sph_trns_by_pol(SGS_inertia,                     &
     &    ipol%SGS_term%i_SGS_inertia, iphys%SGS_term%i_SGS_inertia,    &
     &    b_trns%SGS_term%i_SGS_inertia, trns_back)
!   filtered Lorentz force
      call add_field_4_sph_trns_by_pol(SGS_Lorentz,                     &
     &    ipol%SGS_term%i_SGS_Lorentz, iphys%SGS_term%i_SGS_Lorentz,    &
     &    b_trns%SGS_term%i_SGS_Lorentz, trns_back)
!   filtered induction
      call add_field_4_sph_trns_by_pol(SGS_vecp_induction,              &
     &    ipol%SGS_term%i_SGS_vp_induct,                                &
     &    iphys%SGS_term%i_SGS_vp_induct,                               &
     &    b_trns%SGS_term%i_SGS_vp_induct, trns_back)
!   filtered heat flux
      call add_field_4_sph_trns_by_pol(SGS_heat_flux,                   &
     &    ipol%SGS_term%i_SGS_h_flux, iphys%SGS_term%i_SGS_h_flux,      &
     &    b_trns%SGS_term%i_SGS_h_flux, trns_back)
!   filtered composition flux
      call add_field_4_sph_trns_by_pol(SGS_composit_flux,               &
     &    ipol%SGS_term%i_SGS_c_flux, iphys%SGS_term%i_SGS_c_flux,      &
     &    b_trns%SGS_term%i_SGS_c_flux, trns_back)
!
      end subroutine b_trans_vector_similarity
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_vector_wide_filter_fld                         &
     &         (ipol, iphys, b_trns, trns_back)
!
      use m_wide_filter_field_labels
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!   wide filtered velocity
      call add_field_4_sph_trns_by_pol(wide_filter_velocity,            &
     &    ipol%wide_filter_fld%i_velo, iphys%wide_filter_fld%i_velo,    &
     &    b_trns%wide_filter_fld%i_velo, trns_back)
!   wide filtered vorticity
      call add_field_4_sph_trns_by_pol(wide_filter_vorticity,           &
     &    ipol%wide_filter_fld%i_vort, iphys%wide_filter_fld%i_vort,    &
     &    b_trns%wide_filter_fld%i_vort, trns_back)
!   wide filtered magnetic field
      call add_field_4_sph_trns_by_pol(wide_filter_magne,               &
     &    ipol%wide_filter_fld%i_magne, iphys%wide_filter_fld%i_magne,  &
     &    b_trns%wide_filter_fld%i_magne, trns_back)
!   wide filtered current density
      call add_field_4_sph_trns_by_pol(wide_filter_current,             &
     &    ipol%wide_filter_fld%i_current,                               &
     &    iphys%wide_filter_fld%i_current,                              &
     &    b_trns%wide_filter_fld%i_current, trns_back)
!
      end subroutine b_trans_vector_wide_filter_fld
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_vector_wide_similarity                         &
     &         (ipol, iphys, b_trns, trns_back)
!
      use m_wide_SGS_term_labels
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
!   wide filtered Inertia
      call add_field_4_sph_trns_by_pol(wide_SGS_inertia,                &
     &    ipol%wide_SGS%i_SGS_inertia, iphys%wide_SGS%i_SGS_inertia,    &
     &    b_trns%wide_SGS%i_SGS_inertia, trns_back)
!   wide filtered Lorentz force
      call add_field_4_sph_trns_by_pol(wide_SGS_Lorentz,                &
     &    ipol%wide_SGS%i_SGS_Lorentz, iphys%wide_SGS%i_SGS_Lorentz,    &
     &    b_trns%wide_SGS%i_SGS_Lorentz, trns_back)
!   wide filtered induction
      call add_field_4_sph_trns_by_pol(wide_SGS_vp_induction,           &
     &    ipol%wide_SGS%i_SGS_vp_induct,                                &
     &    iphys%wide_SGS%i_SGS_vp_induct,                               &
     &    b_trns%wide_SGS%i_SGS_vp_induct, trns_back)
!   wide filtered heat flux
      call add_field_4_sph_trns_by_pol(wide_SGS_heat_flux,              &
     &    ipol%wide_SGS%i_SGS_h_flux, iphys%wide_SGS%i_SGS_h_flux,      &
     &    b_trns%wide_SGS%i_SGS_h_flux, trns_back)
!   wide filtered composition flux
      call add_field_4_sph_trns_by_pol(wide_SGS_composit_flux,          &
     &    ipol%wide_SGS%i_SGS_c_flux, iphys%wide_SGS%i_SGS_c_flux,      &
     &    b_trns%wide_SGS%i_SGS_c_flux, trns_back)
!
      end subroutine b_trans_vector_wide_similarity
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_vector_filtered_SGS                            &
     &         (ipol, iphys, b_trns, trns_back)
!
      use m_wide_SGS_term_labels
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
!   dual filtered Inertia
      call add_field_4_sph_trns_by_pol(double_SGS_inertia,              &
     &    ipol%dble_SGS%i_SGS_inertia, iphys%dble_SGS%i_SGS_inertia,    &
     &    b_trns%dble_SGS%i_SGS_inertia, trns_back)
!   dual filtered Lorentz force
      call add_field_4_sph_trns_by_pol(double_SGS_Lorentz,              &
     &    ipol%dble_SGS%i_SGS_Lorentz, iphys%dble_SGS%i_SGS_Lorentz,    &
     &    b_trns%dble_SGS%i_SGS_Lorentz, trns_back)
!   dual filtered induction
      call add_field_4_sph_trns_by_pol(double_SGS_vp_induction,         &
     &    ipol%dble_SGS%i_SGS_vp_induct,                                &
     &    iphys%dble_SGS%i_SGS_vp_induct,                               &
     &    b_trns%dble_SGS%i_SGS_vp_induct, trns_back)
!   dual filtered heat flux
      call add_field_4_sph_trns_by_pol(double_SGS_heat_flux,            &
     &    ipol%dble_SGS%i_SGS_h_flux, iphys%dble_SGS%i_SGS_h_flux,      &
     &    b_trns%dble_SGS%i_SGS_h_flux, trns_back)
!   dual filtered composition flux
      call add_field_4_sph_trns_by_pol(double_SGS_composit_flux,        &
     &    ipol%dble_SGS%i_SGS_c_flux, iphys%dble_SGS%i_SGS_c_flux,      &
     &    b_trns%dble_SGS%i_SGS_c_flux, trns_back)
!
      end subroutine b_trans_vector_filtered_SGS
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine b_trans_scalar_similarity                              &
     &         (ipol, iphys, b_trns, trns_back)
!
      use m_filtered_field_labels
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
!   filtered temperature
      call add_field_4_sph_trns_by_pol(filter_temperature,              &
     &    ipol%filter_fld%i_temp, iphys%filter_fld%i_temp,              &
     &    b_trns%filter_fld%i_temp, trns_back)
!   filtered composition
      call add_field_4_sph_trns_by_pol(filter_composition,              &
     &    ipol%filter_fld%i_light, iphys%filter_fld%i_light,            &
     &    b_trns%filter_fld%i_light, trns_back)
!
      end subroutine b_trans_scalar_similarity
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_scalar_wide_filter_fld                         &
     &         (ipol, iphys, b_trns, trns_back)
!
      use m_wide_filter_field_labels
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
!   wide filtered temperature
      call add_field_4_sph_trns_by_pol(wide_filter_temp,                &
     &    ipol%wide_filter_fld%i_temp, iphys%wide_filter_fld%i_temp,    &
     &    b_trns%wide_filter_fld%i_temp, trns_back)
!   wide filtered composition
      call add_field_4_sph_trns_by_pol(wide_filter_composition,         &
     &    ipol%wide_filter_fld%i_light, iphys%wide_filter_fld%i_light,  &
     &    b_trns%wide_filter_fld%i_light, trns_back)
!
      end subroutine b_trans_scalar_wide_filter_fld
!
!-----------------------------------------------------------------------
!
      end module address_bwd_sph_trans_SGS

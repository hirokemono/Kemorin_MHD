!>@file   address_fwd_sph_trans_SGS.f90
!!@brief  module address_fwd_sph_trans_SGS
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine f_trans_vector_SGS_terms                             &
!!     &         (ipol, iphys, f_trns, trns_fwd)
!!      subroutine f_trans_address_SGS_works                            &
!!     &         (ipol, iphys, f_trns, trns_fwd)
!!        type(phys_address), intent(in) :: ipol, iphys
!!        type(address_each_sph_trans), intent(inout) :: trns_fwd
!!        type(phys_address), intent(inout) :: f_trns
!!@endverbatim
!
      module address_fwd_sph_trans_SGS
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
      subroutine f_trans_vector_SGS_terms                               &
     &         (ipol, iphys, f_trns, trns_fwd)
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_fwd
      type(phys_address), intent(inout) :: f_trns
!
!
!   SGS advection flag
      call add_field_4_sph_trns_by_pol(SGS_inertia,                     &
     &    ipol%SGS_term%i_SGS_inertia, iphys%SGS_term%i_SGS_inertia,    &
     &    f_trns%SGS_term%i_SGS_inertia, trns_fwd)
!   SGS Lorentz force flag
      call add_field_4_sph_trns_by_pol(SGS_Lorentz,                     &
     &    ipol%SGS_term%i_SGS_Lorentz, iphys%SGS_term%i_SGS_Lorentz,    &
     &    f_trns%SGS_term%i_SGS_Lorentz, trns_fwd)
!   SGS induction flag
      call add_field_4_sph_trns_by_pol(SGS_vecp_induction,              &
     &    ipol%SGS_term%i_SGS_vp_induct,                                &
     &    iphys%SGS_term%i_SGS_vp_induct,                               &
     &    f_trns%SGS_term%i_SGS_vp_induct, trns_fwd)
!   SGS heat flux flag
      call add_field_4_sph_trns_by_pol(SGS_heat_flux,                   &
     &    ipol%SGS_term%i_SGS_h_flux, iphys%SGS_term%i_SGS_h_flux,      &
     &    f_trns%SGS_term%i_SGS_h_flux, trns_fwd)
!   SGS composition flux flag
      call add_field_4_sph_trns_by_pol(SGS_composit_flux,               &
     &    ipol%SGS_term%i_SGS_c_flux, iphys%SGS_term%i_SGS_c_flux,      &
     &    f_trns%SGS_term%i_SGS_c_flux, trns_fwd)
!
      end subroutine f_trans_vector_SGS_terms
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_SGS_works                              &
     &         (ipol, iphys, f_trns, trns_fwd)
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_fwd
      type(phys_address), intent(inout) :: f_trns
!
!
!   work of Reynolds stress
      call add_field_4_sph_trns_by_pol(Reynolds_work,                   &
     &    ipol%SGS_ene_flux%i_reynolds_wk,                              &
     &    iphys%SGS_ene_flux%i_reynolds_wk,                             &
     &    f_trns%SGS_ene_flux%i_reynolds_wk, trns_fwd)
!   work of SGS buoyancy
      call add_field_4_sph_trns_by_pol(SGS_buoyancy_flux,               &
     &    ipol%SGS_ene_flux%i_SGS_buo_wk,                               &
     &    iphys%SGS_ene_flux%i_SGS_buo_wk,                              &
     &    f_trns%SGS_ene_flux%i_SGS_buo_wk, trns_fwd)
!   work of SGS compositional buoyancy
      call add_field_4_sph_trns_by_pol(SGS_comp_buoyancy_flux,          &
     &    ipol%SGS_ene_flux%i_SGS_comp_buo_wk,                          &
     &    iphys%SGS_ene_flux%i_SGS_comp_buo_wk,                         &
     &    f_trns%SGS_ene_flux%i_SGS_comp_buo_wk, trns_fwd)
!
      end subroutine f_trans_address_SGS_works
!
!-----------------------------------------------------------------------
!
      end module address_fwd_sph_trans_SGS

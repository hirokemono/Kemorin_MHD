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
!!      subroutine b_trans_scalar_similarity                            &
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
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_SGS_inertia, fhd_SGS_inertia, n_vector,                &
     &    ipol%i_SGS_inertia, itor%i_SGS_inertia,                       &
     &    iphys%i_SGS_inertia, b_trns%i_SGS_inertia, trns_back)
!   filtered Lorentz force
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_SGS_Lorentz, fhd_SGS_Lorentz, n_vector,                &
     &    ipol%i_SGS_Lorentz, itor%i_SGS_Lorentz,                       &
     &    iphys%i_SGS_Lorentz, b_trns%i_SGS_Lorentz, trns_back)
!   filtered induction
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_SGS_vp_induct, fhd_SGS_vp_induct, n_vector,            &
     &    ipol%i_SGS_vp_induct, itor%i_SGS_vp_induct,                   &
     &    iphys%i_SGS_vp_induct, b_trns%i_SGS_vp_induct, trns_back)
!   filtered heat flux
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_SGS_h_flux, fhd_SGS_h_flux, n_vector,                  &
     &    ipol%i_SGS_h_flux, itor%i_SGS_h_flux, iphys%i_SGS_h_flux,     &
     &    b_trns%i_SGS_h_flux, trns_back)
!   filtered composition flux
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_SGS_c_flux, fhd_SGS_c_flux, n_vector,                  &
     &    ipol%i_SGS_c_flux, itor%i_SGS_c_flux, iphys%i_SGS_c_flux,     &
     &    b_trns%i_SGS_c_flux, trns_back)
!
      end subroutine b_trans_vector_similarity
!
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
      end module address_bwd_sph_trans_SGS

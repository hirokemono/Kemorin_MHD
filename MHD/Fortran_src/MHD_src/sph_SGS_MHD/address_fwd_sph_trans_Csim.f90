!>@file   address_fwd_sph_trans_Csim.f90
!!@brief  module address_fwd_sph_trans_Csim
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine f_trans_address_scalar_Csim                          &
!!     &         (ipol, itor, iphys, f_trns, trns_fwd)
!!        type(phys_address), intent(in) :: ipol, itor, iphys
!!        type(address_each_sph_trans), intent(inout) :: trns_fwd
!!        type(phys_address), intent(inout) :: f_trns
!!@endverbatim
!
      module address_fwd_sph_trans_Csim
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
      subroutine f_trans_address_scalar_Csim                            &
     &         (ipol, itor, iphys, f_trns, trns_fwd)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_fwd
      type(phys_address), intent(inout) :: f_trns
!
!
!   SGS advection flag
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_Csim_SGS_m_flux, fhd_Csim_SGS_m_flux, n_scalar,        &
     &    ipol%i_Csim_SGS_m_flux, itor%i_Csim_SGS_m_flux,               &
     &    iphys%i_Csim_SGS_m_flux, f_trns%i_Csim_SGS_m_flux, trns_fwd)
!   SGS Lorentz force flag
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_Csim_SGS_Lorentz, fhd_Csim_SGS_Lorentz, n_scalar,      &
     &    ipol%i_Csim_SGS_Lorentz, itor%i_Csim_SGS_Lorentz,             &
     &    iphys%i_Csim_SGS_Lorentz, f_trns%i_Csim_SGS_Lorentz,          &
     &    trns_fwd)
!   SGS induction flag
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_Csim_SGS_induction, fhd_Csim_SGS_induction, n_scalar,  &
     &    ipol%i_Csim_SGS_induction, itor%i_Csim_SGS_induction,         &
     &    iphys%i_Csim_SGS_induction, f_trns%i_Csim_SGS_induction,      &
     &    trns_fwd)
!   SGS heat flux flag
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_Csim_SGS_h_flux, fhd_Csim_SGS_h_flux, n_scalar,        &
     &    ipol%i_Csim_SGS_h_flux, itor%i_Csim_SGS_h_flux,               &
     &    iphys%i_Csim_SGS_h_flux, f_trns%i_Csim_SGS_h_flux, trns_fwd)
!   SGS composition flux flag
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_Csim_SGS_c_flux, fhd_Csim_SGS_c_flux, n_scalar,        &
     &    ipol%i_Csim_SGS_c_flux, itor%i_Csim_SGS_c_flux,               &
     &    iphys%i_Csim_SGS_c_flux, f_trns%i_Csim_SGS_c_flux, trns_fwd)
!
!   SGS buoyancy
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_Csim_SGS_buoyancy, fhd_Csim_SGS_buoyancy, n_scalar,    &
     &    ipol%i_Csim_SGS_buoyancy, itor%i_Csim_SGS_buoyancy,           &
     &    iphys%i_Csim_SGS_buoyancy, f_trns%i_Csim_SGS_buoyancy,        &
     &    trns_fwd)
!   SGS compostional buoyancy
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_Csim_SGS_comp_buo, fhd_Csim_SGS_comp_buo, n_scalar,    &
     &    ipol%i_Csim_SGS_comp_buo, itor%i_Csim_SGS_comp_buo,           &
     &    iphys%i_Csim_SGS_comp_buo, f_trns%i_Csim_SGS_comp_buo,        &
     &    trns_fwd)
!
      end subroutine f_trans_address_scalar_Csim
!
!-----------------------------------------------------------------------
!
      end module address_fwd_sph_trans_Csim

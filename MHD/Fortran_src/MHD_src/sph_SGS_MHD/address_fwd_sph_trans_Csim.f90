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
!!     &         (ipol, iphys, f_trns, trns_fwd)
!!        type(phys_address), intent(in) :: ipol, iphys
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
     &         (ipol, iphys, f_trns, trns_fwd)
!
      use add_field_to_sph_trans_list
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_each_sph_trans), intent(inout) :: trns_fwd
      type(phys_address), intent(inout) :: f_trns
!
!
!   SGS advection flag
      call add_field_4_sph_trns_by_pol(Csim_SGS_inertia,                &
     &    ipol%Csim%i_SGS_m_flux, iphys%Csim%i_SGS_m_flux,              &
     &    f_trns%Csim%i_SGS_m_flux, trns_fwd)
!   SGS Lorentz force flag
      call add_field_4_sph_trns_by_pol(Csim_SGS_Lorentz,                &
     &    ipol%Csim%i_SGS_Lorentz, iphys%Csim%i_SGS_Lorentz,            &
     &    f_trns%Csim%i_SGS_Lorentz, trns_fwd)
!   SGS induction flag
      call add_field_4_sph_trns_by_pol(Csim_SGS_induction,              &
     &    ipol%Csim%i_SGS_vp_induct, iphys%Csim%i_SGS_vp_induct,        &
     &    f_trns%Csim%i_SGS_vp_induct, trns_fwd)
!   SGS heat flux flag
      call add_field_4_sph_trns_by_pol(Csim_SGS_heat_flux,              &
     &    ipol%Csim%i_SGS_h_flux, iphys%Csim%i_SGS_h_flux,              &
     &    f_trns%Csim%i_SGS_h_flux, trns_fwd)
!   SGS composition flux flag
      call add_field_4_sph_trns_by_pol(Csim_SGS_composit_flux,          &
     &    ipol%Csim%i_SGS_c_flux, iphys%Csim%i_SGS_c_flux,              &
     &    f_trns%Csim%i_SGS_c_flux, trns_fwd)
!
!   SGS buoyancy
      call add_field_4_sph_trns_by_pol(Csim_SGS_buoyancy,               &
     &    ipol%Csim%i_SGS_buoyancy, iphys%Csim%i_SGS_buoyancy,          &
     &    f_trns%Csim%i_SGS_buoyancy, trns_fwd)
!   SGS compostional buoyancy
      call add_field_4_sph_trns_by_pol(Csim_SGS_composit_buo,           &
     &    ipol%Csim%i_SGS_comp_buo, iphys%Csim%i_SGS_comp_buo,          &
     &    f_trns%Csim%i_SGS_comp_buo, trns_fwd)
!
      end subroutine f_trans_address_scalar_Csim
!
!-----------------------------------------------------------------------
!
      end module address_fwd_sph_trans_Csim

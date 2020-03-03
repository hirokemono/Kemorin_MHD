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
      call add_field_name_4_sph_trns(ipol%Csim%i_SGS_m_flux,            &
     &    Csim_SGS_inertia%name, Csim_SGS_inertia%n_comp,               &
     &    ipol%Csim%i_SGS_m_flux, itor%Csim%i_SGS_m_flux,               &
     &    iphys%Csim%i_SGS_m_flux, f_trns%Csim%i_SGS_m_flux, trns_fwd)
!   SGS Lorentz force flag
      call add_field_name_4_sph_trns(ipol%Csim%i_SGS_Lorentz,           &
     &    Csim_SGS_Lorentz%name, Csim_SGS_Lorentz%n_comp,               &
     &    ipol%Csim%i_SGS_Lorentz, itor%Csim%i_SGS_Lorentz,             &
     &    iphys%Csim%i_SGS_Lorentz, f_trns%Csim%i_SGS_Lorentz,          &
     &    trns_fwd)
!   SGS induction flag
      call add_field_name_4_sph_trns(ipol%Csim%i_SGS_induction,         &
     &    Csim_SGS_induction%name, Csim_SGS_induction%n_comp,           &
     &    ipol%Csim%i_SGS_induction, itor%Csim%i_SGS_induction,         &
     &    iphys%Csim%i_SGS_induction, f_trns%Csim%i_SGS_induction,      &
     &    trns_fwd)
!   SGS heat flux flag
      call add_field_name_4_sph_trns(ipol%Csim%i_SGS_h_flux,            &
     &    Csim_SGS_heat_flux%name, Csim_SGS_heat_flux%n_comp,           &
     &    ipol%Csim%i_SGS_h_flux, itor%Csim%i_SGS_h_flux,               &
     &    iphys%Csim%i_SGS_h_flux, f_trns%Csim%i_SGS_h_flux, trns_fwd)
!   SGS composition flux flag
      call add_field_name_4_sph_trns(ipol%Csim%i_SGS_c_flux,            &
     &    Csim_SGS_composit_flux%name, Csim_SGS_composit_flux%n_comp,   &
     &    ipol%Csim%i_SGS_c_flux, itor%Csim%i_SGS_c_flux,               &
     &    iphys%Csim%i_SGS_c_flux, f_trns%Csim%i_SGS_c_flux, trns_fwd)
!
!   SGS buoyancy
      call add_field_name_4_sph_trns(ipol%Csim%i_SGS_buoyancy,          &
     &    Csim_SGS_buoyancy%name, Csim_SGS_buoyancy%n_comp,             &
     &    ipol%Csim%i_SGS_buoyancy, itor%Csim%i_SGS_buoyancy,           &
     &    iphys%Csim%i_SGS_buoyancy, f_trns%Csim%i_SGS_buoyancy,        &
     &    trns_fwd)
!   SGS compostional buoyancy
      call add_field_name_4_sph_trns(ipol%i_Csim_SGS_comp_buo,          &
     &    Csim_SGS_composit_buo%name, Csim_SGS_composit_buo%n_comp,     &
     &    ipol%i_Csim_SGS_comp_buo, itor%i_Csim_SGS_comp_buo,           &
     &    iphys%i_Csim_SGS_comp_buo, f_trns%i_Csim_SGS_comp_buo,        &
     &    trns_fwd)
!
      end subroutine f_trans_address_scalar_Csim
!
!-----------------------------------------------------------------------
!
      end module address_fwd_sph_trans_Csim

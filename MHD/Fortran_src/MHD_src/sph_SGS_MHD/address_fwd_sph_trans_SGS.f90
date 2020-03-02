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
!!     &         (ipol, itor, iphys, f_trns, trns_fwd)
!!      subroutine f_trans_address_SGS_works                            &
!!     &         (ipol, itor, iphys, f_trns, trns_fwd)
!!        type(phys_address), intent(in) :: ipol, itor, iphys
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
     &         (ipol, itor, iphys, f_trns, trns_fwd)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_fwd
      type(phys_address), intent(inout) :: f_trns
!
!
!   SGS advection flag
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_SGS_inertia, SGS_inertia%name, SGS_inertia%n_comp,     &
     &    ipol%i_SGS_inertia, itor%i_SGS_inertia, iphys%i_SGS_inertia,  &
     &    f_trns%i_SGS_inertia, trns_fwd)
!   SGS Lorentz force flag
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_SGS_Lorentz, SGS_Lorentz%name, SGS_Lorentz%n_comp,     &
     &    ipol%i_SGS_Lorentz, itor%i_SGS_Lorentz, iphys%i_SGS_Lorentz,  &
     &    f_trns%i_SGS_Lorentz, trns_fwd)
!   SGS induction flag
      call add_field_name_4_sph_trns(ipol%i_SGS_vp_induct,              &
     &    SGS_vecp_induction%name, SGS_vecp_induction%n_comp,           &
     &    ipol%i_SGS_vp_induct, itor%i_SGS_vp_induct,                   &
     &    iphys%i_SGS_vp_induct, f_trns%i_SGS_vp_induct, trns_fwd)
!   SGS heat flux flag
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_SGS_h_flux, SGS_heat_flux%name, SGS_heat_flux%n_comp,  &
     &    ipol%i_SGS_h_flux, itor%i_SGS_h_flux, iphys%i_SGS_h_flux,     &
     &    f_trns%i_SGS_h_flux, trns_fwd)
!   SGS composition flux flag
      call add_field_name_4_sph_trns(ipol%i_SGS_c_flux,                 &
     &    SGS_composit_flux%name, SGS_composit_flux%n_comp,             &
     &    ipol%i_SGS_c_flux, itor%i_SGS_c_flux, iphys%i_SGS_c_flux,     &
     &    f_trns%i_SGS_c_flux, trns_fwd)
!
      end subroutine f_trans_vector_SGS_terms
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_SGS_works                              &
     &         (ipol, itor, iphys, f_trns, trns_fwd)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_fwd
      type(phys_address), intent(inout) :: f_trns
!
!
!   work of Reynolds stress
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_reynolds_wk, fhd_Reynolds_work, n_scalar,              &
     &    ipol%i_reynolds_wk, itor%i_reynolds_wk,                       &
     &    iphys%i_reynolds_wk, f_trns%i_reynolds_wk, trns_fwd)
!   work of SGS buoyancy
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_SGS_buo_wk, fhd_SGS_buo_flux, n_scalar,                &
     &    ipol%i_SGS_buo_wk, itor%i_SGS_buo_wk,                         &
     &    iphys%i_SGS_buo_wk, f_trns%i_SGS_buo_wk, trns_fwd)
!   work of SGS compositional buoyancy
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_SGS_comp_buo_wk, fhd_SGS_comp_buo_flux, n_scalar,      &
     &    ipol%i_SGS_comp_buo_wk, itor%i_SGS_comp_buo_wk,               &
     &    iphys%i_SGS_comp_buo_wk, f_trns%i_SGS_comp_buo_wk, trns_fwd)
!
      end subroutine f_trans_address_SGS_works
!
!-----------------------------------------------------------------------
!
      end module address_fwd_sph_trans_SGS

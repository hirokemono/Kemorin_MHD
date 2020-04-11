!>@file   add_Csim_4_sph_trns.f90
!!@brief  module add_Csim_4_sph_trns
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine add_Csim_4_sph_trns_by_pol                           &
!!     &         (ipol_Csim, iphys_Csim, f_trns_Csim, trns)
!!      subroutine add_Csim_4_sph_trns_snap                             &
!!     &         (ipol_Csim, iphys_Csim, f_trns_Csim, trns)
!!        type(SGS_term_address), intent(in) :: ipol_Csim, iphys_Csim
!!        type(SGS_term_address), intent(inout) :: f_trns_Csim
!!        type(address_each_sph_trans), intent(inout) :: trns
!!@endverbatim
!
      module add_Csim_4_sph_trns
!
      use m_precision
!
      use t_SGS_term_labels
      use t_addresses_sph_transform
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine add_Csim_4_sph_trns_by_pol                             &
     &         (ipol_Csim, iphys_Csim, f_trns_Csim, trns)
!
      use add_field_to_sph_trans_list
!
      type(SGS_term_address), intent(in) :: ipol_Csim, iphys_Csim
      type(SGS_term_address), intent(inout) :: f_trns_Csim
      type(address_each_sph_trans), intent(inout) :: trns
!
!
!   SGS advection flag
      call add_field_4_sph_trns_by_pol(Csim_SGS_inertia,                &
     &    ipol_Csim%i_SGS_m_flux, iphys_Csim%i_SGS_m_flux,              &
     &    f_trns_Csim%i_SGS_m_flux, trns)
!   SGS Lorentz force flag
      call add_field_4_sph_trns_by_pol(Csim_SGS_Lorentz,                &
     &    ipol_Csim%i_SGS_Lorentz, iphys_Csim%i_SGS_Lorentz,            &
     &    f_trns_Csim%i_SGS_Lorentz, trns)
!   SGS induction flag
      call add_field_4_sph_trns_by_pol(Csim_SGS_induction,              &
     &    ipol_Csim%i_SGS_vp_induct, iphys_Csim%i_SGS_vp_induct,        &
     &    f_trns_Csim%i_SGS_vp_induct, trns)
!   SGS heat flux flag
      call add_field_4_sph_trns_by_pol(Csim_SGS_heat_flux,              &
     &    ipol_Csim%i_SGS_h_flux, iphys_Csim%i_SGS_h_flux,              &
     &    f_trns_Csim%i_SGS_h_flux, trns)
!   SGS composition flux flag
      call add_field_4_sph_trns_by_pol(Csim_SGS_composit_flux,          &
     &    ipol_Csim%i_SGS_c_flux, iphys_Csim%i_SGS_c_flux,              &
     &    f_trns_Csim%i_SGS_c_flux, trns)
!
!   SGS buoyancy
      call add_field_4_sph_trns_by_pol(Csim_SGS_buoyancy,               &
     &    ipol_Csim%i_SGS_buoyancy, iphys_Csim%i_SGS_buoyancy,          &
     &    f_trns_Csim%i_SGS_buoyancy, trns)
!   SGS compostional buoyancy
      call add_field_4_sph_trns_by_pol(Csim_SGS_composit_buo,           &
     &    ipol_Csim%i_SGS_comp_buo, iphys_Csim%i_SGS_comp_buo,          &
     &    f_trns_Csim%i_SGS_comp_buo, trns)
!
      end subroutine add_Csim_4_sph_trns_by_pol
!
!-----------------------------------------------------------------------
!
      subroutine add_Csim_4_sph_trns_snap                               &
     &         (ipol_Csim, iphys_Csim, f_trns_Csim, trns)
!
      use add_field_to_sph_trans_list
!
      type(SGS_term_address), intent(in) :: ipol_Csim, iphys_Csim
      type(SGS_term_address), intent(inout) :: f_trns_Csim
      type(address_each_sph_trans), intent(inout) :: trns
!
!
!   SGS advection flag
      call add_field_name_4_sph_trns_snap(Csim_SGS_inertia,             &
     &    ipol_Csim%i_SGS_m_flux, iphys_Csim%i_SGS_m_flux,              &
     &    f_trns_Csim%i_SGS_m_flux, trns)
!   SGS Lorentz force flag
      call add_field_name_4_sph_trns_snap(Csim_SGS_Lorentz,             &
     &    ipol_Csim%i_SGS_Lorentz, iphys_Csim%i_SGS_Lorentz,            &
     &    f_trns_Csim%i_SGS_Lorentz, trns)
!   SGS induction flag
      call add_field_name_4_sph_trns_snap(Csim_SGS_induction,           &
     &    ipol_Csim%i_SGS_vp_induct, iphys_Csim%i_SGS_vp_induct,        &
     &    f_trns_Csim%i_SGS_vp_induct, trns)
!   SGS heat flux flag
      call add_field_name_4_sph_trns_snap(Csim_SGS_heat_flux,           &
     &    ipol_Csim%i_SGS_h_flux, iphys_Csim%i_SGS_h_flux,              &
     &    f_trns_Csim%i_SGS_h_flux, trns)
!   SGS composition flux flag
      call add_field_name_4_sph_trns_snap(Csim_SGS_composit_flux,       &
     &    ipol_Csim%i_SGS_c_flux, iphys_Csim%i_SGS_c_flux,              &
     &    f_trns_Csim%i_SGS_c_flux, trns)
!
!   SGS buoyancy
      call add_field_name_4_sph_trns_snap(Csim_SGS_buoyancy,            &
     &    ipol_Csim%i_SGS_buoyancy, iphys_Csim%i_SGS_buoyancy,          &
     &    f_trns_Csim%i_SGS_buoyancy, trns)
!   SGS compostional buoyancy
      call add_field_name_4_sph_trns_snap(Csim_SGS_composit_buo,        &
     &    ipol_Csim%i_SGS_comp_buo, iphys_Csim%i_SGS_comp_buo,          &
     &    f_trns_Csim%i_SGS_comp_buo, trns)
!
      end subroutine add_Csim_4_sph_trns_snap
!
!-----------------------------------------------------------------------
!
      end module add_Csim_4_sph_trns

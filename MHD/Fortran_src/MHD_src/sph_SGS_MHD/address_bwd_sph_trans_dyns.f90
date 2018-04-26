!>@file   address_bwd_sph_trans_dyns.f90
!!@brief  module address_bwd_sph_trans_dyns
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine b_trans_address_vector_DYNS                          &
!!     &         (ipol, itor, iphys, b_trns, trns_back)
!!      subroutine b_trans_address_scalar_DYNS                          &
!!     &         (ipol, itor, iphys, b_trns, trns_back)
!!        type(phys_address), intent(in) :: ipol, itor, iphys
!!        type(address_each_sph_trans), intent(inout) :: trns_back
!!        type(phys_address), intent(inout) :: b_trns
!!@endverbatim
!
      module address_bwd_sph_trans_dyns
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
      subroutine b_trans_address_vector_DYNS                            &
     &         (ipol, itor, iphys, b_trns, trns_back)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
      trns_back%nfield = 0
      call alloc_sph_trns_field_name(trns_back)
!
!   wide filtered velocity
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_wide_fil_velo, fhd_w_filter_velo, n_vector,            &
     &    ipol%i_wide_fil_velo, itor%i_wide_fil_velo,                   &
     &    iphys%i_wide_fil_velo, b_trns%i_wide_fil_velo, trns_back)
!   wide filtered vorticity
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_wide_fil_vort, fhd_w_filter_vort, n_vector,            &
     &    ipol%i_wide_fil_vort, itor%i_wide_fil_vort,                   &
     &    iphys%i_wide_fil_vort, b_trns%i_wide_fil_vort, trns_back)
!   wide filtered magnetic field
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_wide_fil_magne, fhd_w_filter_magne, n_vector,          &
     &    ipol%i_wide_fil_magne, itor%i_wide_fil_magne,                 &
     &    iphys%i_wide_fil_magne, b_trns%i_wide_fil_magne, trns_back)
!   wide filtered current density
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_wide_fil_current, fhd_w_filter_current, n_vector,      &
     &    ipol%i_wide_fil_current, itor%i_wide_fil_current,             &
     &    iphys%i_wide_fil_current, b_trns%i_wide_fil_current,          &
     &    trns_back)
!
!   dual filtered velocity
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_dbl_fil_velo, fhd_d_filter_velo, n_vector,             &
     &    ipol%i_dbl_fil_velo, itor%i_dbl_fil_velo,                     &
     &    iphys%i_dbl_fil_velo, b_trns%i_dbl_fil_velo, trns_back)
!   dual filtered vorticity
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_dbl_fil_vort, fhd_d_filter_vort, n_vector,             &
     &    ipol%i_dbl_fil_vort, itor%i_dbl_fil_vort,                     &
     &    iphys%i_dbl_fil_vort, b_trns%i_dbl_fil_vort, trns_back)
!   dual filtered magnetic field
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_dbl_fil_magne, fhd_d_filter_magne, n_vector,           &
     &    ipol%i_dbl_fil_magne, itor%i_dbl_fil_magne,                   &
     &    iphys%i_dbl_fil_magne, b_trns%i_dbl_fil_magne, trns_back)
!   dual filtered current density
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_dbl_fil_current, fhd_d_filter_current, n_vector,       &
     &    ipol%i_dbl_fil_current, itor%i_dbl_fil_current,               &
     &    iphys%i_dbl_fil_current, b_trns%i_dbl_fil_current, trns_back)
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
      trns_back%num_vector = trns_back%nfield
!
      end subroutine b_trans_address_vector_DYNS
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_scalar_DYNS                            &
     &         (ipol, itor, iphys, b_trns, trns_back)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
!
!
!   wide filtered temperature
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_wide_fil_temp, fhd_w_filter_temp, n_scalar,            &
     &    ipol%i_wide_fil_temp, itor%i_wide_fil_temp,                   &
     &    iphys%i_wide_fil_temp, b_trns%i_wide_fil_temp, trns_back)
!   wide filtered composition
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_wide_fil_comp, fhd_w_filter_comp, n_scalar,            &
     &    ipol%i_wide_fil_comp, itor%i_wide_fil_comp,                   &
     &    iphys%i_wide_fil_comp, b_trns%i_wide_fil_comp, trns_back)
      trns_back%num_scalar = trns_back%nfield - trns_back%num_vector
!
      end subroutine b_trans_address_scalar_DYNS
!
!-----------------------------------------------------------------------
!
      end module address_bwd_sph_trans_dyns

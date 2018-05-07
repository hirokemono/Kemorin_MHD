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
      subroutine b_trans_vector_wide_filter_fld                         &
     &         (ipol, itor, iphys, b_trns, trns_back)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_back
      type(phys_address), intent(inout) :: b_trns
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
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_wide_fil_temp, fhd_w_filter_temp, n_scalar,            &
     &    ipol%i_wide_fil_temp, itor%i_wide_fil_temp,                   &
     &    iphys%i_wide_fil_temp, b_trns%i_wide_fil_temp, trns_back)
!   wide filtered composition
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_wide_fil_comp, fhd_w_filter_comp, n_scalar,            &
     &    ipol%i_wide_fil_comp, itor%i_wide_fil_comp,                   &
     &    iphys%i_wide_fil_comp, b_trns%i_wide_fil_comp, trns_back)
!
      end subroutine b_trans_scalar_wide_filter_fld
!
!-----------------------------------------------------------------------
!
      end module address_bwd_sph_trans_SGS

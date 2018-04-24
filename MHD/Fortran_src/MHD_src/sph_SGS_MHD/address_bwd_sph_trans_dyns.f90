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
!!      subroutine b_trans_address_vector_DYNS(ipol, trns_DYNS)
!!      subroutine b_trans_address_scalar_DYNS(ipol, trns_DYNS)
!!        type(phys_address), intent(in) :: ipol
!!        type(address_4_sph_trans), intent(inout) :: trns_DYNS
!!
!!      subroutine set_b_trans_vector_field_DYNS                        &
!!     &         (icou, ipol, itor, iphys, trns_DYNS)
!!      subroutine set_b_trans_scalar_field_dyns                        &
!!     &         (icou, ipol, itor, iphys, trns_DYNS)
!!        type(phys_address), intent(in) :: ipol, itor, iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_DYNS
!!@endverbatim
!
      module address_bwd_sph_trans_dyns
!
      use m_precision
!
      use m_phys_labels
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
      subroutine b_trans_address_vector_DYNS(ipol, trns_DYNS)
!
      type(phys_address), intent(in) :: ipol
      type(address_4_sph_trans), intent(inout) :: trns_DYNS
!
!
      trns_DYNS%nvector_rj_2_rtp = 0
!
!   wide filtered velocity
      call add_vector_trans_flag(ipol%i_wide_fil_velo,                  &
     &    trns_DYNS%nvector_rj_2_rtp, trns_DYNS%b_trns%i_wide_fil_velo)
!   wide filtered vorticity
      call add_vector_trans_flag(ipol%i_wide_fil_vort,                  &
     &    trns_DYNS%nvector_rj_2_rtp, trns_DYNS%b_trns%i_wide_fil_vort)
!   wide filtered magnetic field
      call add_vector_trans_flag(ipol%i_wide_fil_magne,                 &
     &   trns_DYNS%nvector_rj_2_rtp, trns_DYNS%b_trns%i_wide_fil_magne)
!   wide filtered current density
      call add_vector_trans_flag                                        &
     &   (ipol%i_wide_fil_current, trns_DYNS%nvector_rj_2_rtp,          &
     &    trns_DYNS%b_trns%i_wide_fil_current)
!
!   dual filtered velocity
      call add_vector_trans_flag(ipol%i_dbl_fil_velo,                   &
     &    trns_DYNS%nvector_rj_2_rtp, trns_DYNS%b_trns%i_dbl_fil_velo)
!   dual filtered vorticity
      call add_vector_trans_flag(ipol%i_dbl_fil_vort,                   &
     &    trns_DYNS%nvector_rj_2_rtp, trns_DYNS%b_trns%i_dbl_fil_vort)
!   dual filtered magnetic field
      call add_vector_trans_flag(ipol%i_dbl_fil_magne,                  &
     &    trns_DYNS%nvector_rj_2_rtp, trns_DYNS%b_trns%i_dbl_fil_magne)
!   dual filtered current density
      call add_vector_trans_flag                                        &
     &   (ipol%i_dbl_fil_current, trns_DYNS%nvector_rj_2_rtp,           &
     &    trns_DYNS%b_trns%i_dbl_fil_current)
!
!
!   wide filtered Inertia
      call add_vector_trans_flag                                        &
     &   (ipol%i_wide_SGS_inertia, trns_DYNS%nvector_rj_2_rtp,          &
     &    trns_DYNS%b_trns%i_wide_SGS_inertia)
!   wide filtered Lorentz force
      call add_vector_trans_flag                                        &
     &   (ipol%i_wide_SGS_Lorentz, trns_DYNS%nvector_rj_2_rtp,          &
     &    trns_DYNS%b_trns%i_wide_SGS_Lorentz)
!   wide filtered induction
      call add_vector_trans_flag                                        &
     &   (ipol%i_wide_SGS_vp_induct, trns_DYNS%nvector_rj_2_rtp,        &
     &    trns_DYNS%b_trns%i_wide_SGS_vp_induct)
!   wide filtered heat flux
      call add_vector_trans_flag                                        &
     &   (ipol%i_wide_SGS_h_flux, trns_DYNS%nvector_rj_2_rtp,           &
     &    trns_DYNS%b_trns%i_wide_SGS_h_flux)
!   wide filtered composition flux
      call add_vector_trans_flag                                        &
     &   (ipol%i_wide_SGS_c_flux, trns_DYNS%nvector_rj_2_rtp,           &
     &    trns_DYNS%b_trns%i_wide_SGS_c_flux)
!
!   dual filtered Inertia
      call add_vector_trans_flag                                        &
     &   (ipol%i_dbl_SGS_inertia, trns_DYNS%nvector_rj_2_rtp,           &
     &    trns_DYNS%b_trns%i_dbl_SGS_inertia)
!   dual filtered Lorentz force
      call add_vector_trans_flag                                        &
     &   (ipol%i_dbl_SGS_Lorentz, trns_DYNS%nvector_rj_2_rtp,           &
     &    trns_DYNS%b_trns%i_dbl_SGS_Lorentz)
!   dual filtered induction
      call add_vector_trans_flag                                        &
     &   (ipol%i_dbl_SGS_vp_induct, trns_DYNS%nvector_rj_2_rtp,         &
     &    trns_DYNS%b_trns%i_dbl_SGS_vp_induct)
!   dual filtered heat flux
      call add_vector_trans_flag(ipol%i_dbl_SGS_h_flux,                 &
     &   trns_DYNS%nvector_rj_2_rtp, trns_DYNS%b_trns%i_dbl_SGS_h_flux)
!   dual filtered composition flux
      call add_vector_trans_flag(ipol%i_dbl_SGS_c_flux,                 &
     &   trns_DYNS%nvector_rj_2_rtp, trns_DYNS%b_trns%i_dbl_SGS_c_flux)
!
      end subroutine b_trans_address_vector_DYNS
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_scalar_DYNS(ipol, trns_DYNS)
!
      type(phys_address), intent(in) :: ipol
      type(address_4_sph_trans), intent(inout) :: trns_DYNS
!
!
      trns_DYNS%nscalar_rj_2_rtp = 0
!   wide filtered temperature
      call add_scalar_trans_flag(ipol%i_wide_fil_temp,                  &
     &    trns_DYNS%nvector_rj_2_rtp, trns_DYNS%nscalar_rj_2_rtp,       &
     &    trns_DYNS%b_trns%i_wide_fil_temp)
!   wide filtered composition
      call add_scalar_trans_flag(ipol%i_wide_fil_comp,                  &
     &    trns_DYNS%nvector_rj_2_rtp, trns_DYNS%nscalar_rj_2_rtp,       &
     &    trns_DYNS%b_trns%i_wide_fil_comp)
!
      end subroutine b_trans_address_scalar_DYNS
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_b_trans_vector_field_DYNS                          &
     &         (icou, ipol, itor, iphys, trns_DYNS)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_4_sph_trans), intent(inout) :: trns_DYNS
      integer(kind = kint), intent(inout) :: icou
!
!      wide filtered force
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_w_filter_velo, trns_DYNS%b_trns%i_wide_fil_velo,          &
     &    ipol%i_wide_fil_velo, itor%i_wide_fil_velo,                   &
     &    iphys%i_wide_fil_velo, icou, trns_DYNS)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_w_filter_vort, trns_DYNS%b_trns%i_wide_fil_vort,          &
     &    ipol%i_wide_fil_vort, itor%i_wide_fil_vort,                   &
     &    iphys%i_wide_fil_vort, icou, trns_DYNS)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_w_filter_magne, trns_DYNS%b_trns%i_wide_fil_magne,        &
     &    ipol%i_wide_fil_magne, itor%i_wide_fil_magne,                 &
     &    iphys%i_wide_fil_magne, icou, trns_DYNS)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_w_filter_current, trns_DYNS%b_trns%i_wide_fil_current,    &
     &    ipol%i_wide_fil_current, itor%i_wide_fil_current,             &
     &    iphys%i_wide_fil_current, icou, trns_DYNS)
!
!      double filtered force
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_d_filter_velo, trns_DYNS%b_trns%i_dbl_fil_velo,           &
     &    ipol%i_dbl_fil_velo, itor%i_dbl_fil_velo,                     &
     &    iphys%i_dbl_fil_velo, icou, trns_DYNS)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_d_filter_vort, trns_DYNS%b_trns%i_dbl_fil_vort,           &
     &    ipol%i_dbl_fil_vort, itor%i_dbl_fil_vort,                     &
     &    iphys%i_dbl_fil_vort, icou, trns_DYNS)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_d_filter_magne, trns_DYNS%b_trns%i_dbl_fil_magne,         &
     &    ipol%i_dbl_fil_magne, itor%i_dbl_fil_magne,                   &
     &    iphys%i_dbl_fil_magne, icou, trns_DYNS)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_d_filter_current, trns_DYNS%b_trns%i_dbl_fil_current,     &
     &    ipol%i_dbl_fil_current, itor%i_dbl_fil_current,               &
     &    iphys%i_dbl_fil_current, icou, trns_DYNS)
!
!       wide filtered Inertia
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_wide_SGS_inertia, trns_DYNS%b_trns%i_wide_SGS_inertia,    &
     &    ipol%i_wide_SGS_inertia, itor%i_wide_SGS_inertia,             &
     &    iphys%i_wide_SGS_inertia, icou, trns_DYNS)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_wide_SGS_Lorentz, trns_DYNS%b_trns%i_wide_SGS_Lorentz,    &
     &    ipol%i_wide_SGS_Lorentz, itor%i_wide_SGS_Lorentz,             &
     &    iphys%i_wide_SGS_Lorentz, icou, trns_DYNS)
      call set_field_name_4_bwd_trns                                    &
     &  (fhd_wide_SGS_vp_induct, trns_DYNS%b_trns%i_wide_SGS_vp_induct, &
     &   ipol%i_wide_SGS_vp_induct, itor%i_wide_SGS_vp_induct,          &
     &   iphys%i_wide_SGS_vp_induct, icou, trns_DYNS)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_wide_SGS_h_flux, trns_DYNS%b_trns%i_wide_SGS_h_flux,      &
     &    ipol%i_wide_SGS_h_flux, itor%i_wide_SGS_h_flux,               &
     &    iphys%i_wide_SGS_h_flux, icou, trns_DYNS)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_wide_SGS_c_flux, trns_DYNS%b_trns%i_wide_SGS_c_flux,      &
     &    ipol%i_wide_SGS_c_flux, itor%i_wide_SGS_c_flux,               &
     &    iphys%i_wide_SGS_c_flux, icou, trns_DYNS)
!
!       double filtered Inertia
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_dbl_SGS_inertia, trns_DYNS%b_trns%i_dbl_SGS_inertia,      &
     &    ipol%i_dbl_SGS_inertia, itor%i_dbl_SGS_inertia,               &
     &    iphys%i_dbl_SGS_inertia, icou, trns_DYNS)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_dbl_SGS_Lorentz, trns_DYNS%b_trns%i_dbl_SGS_Lorentz,      &
     &    ipol%i_dbl_SGS_Lorentz, itor%i_dbl_SGS_Lorentz,               &
     &    iphys%i_dbl_SGS_Lorentz, icou, trns_DYNS)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_dbl_SGS_vp_induct, trns_DYNS%b_trns%i_dbl_SGS_vp_induct,  &
     &    ipol%i_dbl_SGS_vp_induct, itor%i_dbl_SGS_vp_induct,           &
     &     iphys%i_dbl_SGS_vp_induct, icou, trns_DYNS)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_dbl_SGS_h_flux, trns_DYNS%b_trns%i_dbl_SGS_h_flux,        &
     &    ipol%i_dbl_SGS_h_flux, itor%i_dbl_SGS_h_flux,                 &
     &    iphys%i_dbl_SGS_h_flux, icou, trns_DYNS)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_dbl_SGS_c_flux, trns_DYNS%b_trns%i_dbl_SGS_c_flux,        &
     &    ipol%i_dbl_SGS_c_flux, itor%i_dbl_SGS_c_flux,                 &
     &    iphys%i_dbl_SGS_c_flux,icou, trns_DYNS)
!
      end subroutine set_b_trans_vector_field_DYNS
!
!-----------------------------------------------------------------------
!
      subroutine set_b_trans_scalar_field_DYNS                          &
     &         (icou, ipol, itor, iphys, trns_DYNS)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_4_sph_trans), intent(inout) :: trns_DYNS
      integer(kind = kint), intent(inout) :: icou
!
!
!   wide filtered temperature
      call set_field_name_4_bwd_trns(fhd_w_filter_temp,                 &
     &    trns_DYNS%b_trns%i_wide_fil_temp, ipol%i_wide_fil_temp,       &
     &    itor%i_wide_fil_temp, iphys%i_wide_fil_temp,                  &
     &    icou, trns_DYNS)
      call set_field_name_4_bwd_trns(fhd_w_filter_comp,                 &
     &    trns_DYNS%b_trns%i_wide_fil_comp, ipol%i_wide_fil_comp,       &
     &    itor%i_wide_fil_comp, iphys%i_wide_fil_comp,                  &
     &    icou, trns_DYNS)
!
!   double filtered temperature
      call set_field_name_4_bwd_trns(fhd_d_filter_temp,                 &
     &    trns_DYNS%b_trns%i_dbl_fil_temp, ipol%i_dbl_fil_temp,         &
     &    itor%i_dbl_fil_temp, iphys%i_dbl_fil_temp, icou, trns_DYNS)
      call set_field_name_4_bwd_trns(fhd_d_filter_comp,                 &
     &    trns_DYNS%b_trns%i_dbl_fil_comp, ipol%i_dbl_fil_comp,         &
     &    itor%i_dbl_fil_comp, iphys%i_dbl_fil_comp, icou, trns_DYNS)
!
      end subroutine set_b_trans_scalar_field_DYNS
!
!-----------------------------------------------------------------------
!
      end module address_bwd_sph_trans_dyns

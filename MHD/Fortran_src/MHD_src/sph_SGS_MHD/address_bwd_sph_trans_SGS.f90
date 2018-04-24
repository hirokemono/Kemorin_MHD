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
!!      subroutine b_trans_address_vector_SGS(ipol, trns_SGS)
!!      subroutine b_trans_address_scalar_SGS(ipol, trns_SGS)
!!        type(phys_address), intent(in) :: ipol
!!        type(address_4_sph_trans), intent(inout) :: trns_SGS
!!
!!      subroutine set_b_trans_vector_field_SGS                         &
!!     &         (icou, ipol, itor, iphys, trns_SGS)
!!      subroutine set_b_trans_scalar_field_SGS                         &
!!     &         (icou, ipol, itor, iphys, trns_SGS)
!!        type(phys_address), intent(in) :: ipol, itor, iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_SGS
!!@endverbatim
!
      module address_bwd_sph_trans_SGS
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
      subroutine b_trans_address_vector_SGS(ipol, trns_SGS)
!
      type(phys_address), intent(in) :: ipol
      type(address_4_sph_trans), intent(inout) :: trns_SGS
!
!
      trns_SGS%nvector_rj_2_rtp = 0
!
!   filtered velocity
      call add_vector_trans_flag(ipol%i_filter_velo,                    &
     &    trns_SGS%nvector_rj_2_rtp, trns_SGS%b_trns%i_filter_velo)
!   filtered vorticity
      call add_vector_trans_flag(ipol%i_filter_vort,                    &
     &    trns_SGS%nvector_rj_2_rtp, trns_SGS%b_trns%i_filter_vort)
!   filtered magnetic field
      call add_vector_trans_flag(ipol%i_filter_magne,                   &
     &    trns_SGS%nvector_rj_2_rtp, trns_SGS%b_trns%i_filter_magne)
!   filtered current density
      call add_vector_trans_flag(ipol%i_filter_current,                 &
     &    trns_SGS%nvector_rj_2_rtp, trns_SGS%b_trns%i_filter_current)
!
!   filtered Inertia
      call add_vector_trans_flag(ipol%i_SGS_inertia,                    &
     &    trns_SGS%nvector_rj_2_rtp, trns_SGS%b_trns%i_SGS_inertia)
!   filtered Lorentz force
      call add_vector_trans_flag(ipol%i_SGS_Lorentz,                    &
     &    trns_SGS%nvector_rj_2_rtp, trns_SGS%b_trns%i_SGS_Lorentz)
!   filtered induction
      call add_vector_trans_flag(ipol%i_SGS_vp_induct,                  &
     &    trns_SGS%nvector_rj_2_rtp, trns_SGS%b_trns%i_SGS_vp_induct)
!   filtered heat flux
      call add_vector_trans_flag(ipol%i_SGS_h_flux,                     &
     &    trns_SGS%nvector_rj_2_rtp, trns_SGS%b_trns%i_SGS_h_flux)
!   filtered composition flux
      call add_vector_trans_flag(ipol%i_SGS_c_flux,                     &
     &    trns_SGS%nvector_rj_2_rtp, trns_SGS%b_trns%i_SGS_c_flux)
!
      end subroutine b_trans_address_vector_SGS
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_scalar_SGS(ipol, trns_SGS)
!
      type(phys_address), intent(in) :: ipol
      type(address_4_sph_trans), intent(inout) :: trns_SGS
!
!
      trns_SGS%nscalar_rj_2_rtp = 0
!   filtered temperature
      call add_scalar_trans_flag(ipol%i_filter_temp,                    &
     &    trns_SGS%nvector_rj_2_rtp, trns_SGS%nscalar_rj_2_rtp,         &
     &    trns_SGS%b_trns%i_filter_temp)
!   filtered composition
      call add_scalar_trans_flag(ipol%i_filter_comp,                    &
     &    trns_SGS%nvector_rj_2_rtp, trns_SGS%nscalar_rj_2_rtp,         &
     &    trns_SGS%b_trns%i_filter_comp)
!
      end subroutine b_trans_address_scalar_SGS
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_b_trans_vector_field_SGS                           &
     &         (icou, ipol, itor, iphys, trns_SGS)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_4_sph_trans), intent(inout) :: trns_SGS
      integer(kind = kint), intent(inout) :: icou
!
!   filtered fields
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_filter_velo, trns_SGS%b_trns%i_filter_velo,               &
     &    ipol%i_filter_velo, itor%i_filter_velo, iphys%i_filter_velo,  &
     &    icou, trns_SGS)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_filter_vort, trns_SGS%b_trns%i_filter_vort,               &
     &    ipol%i_filter_vort, itor%i_filter_vort, iphys%i_filter_vort,  &
     &     icou, trns_SGS)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_filter_magne, trns_SGS%b_trns%i_filter_magne,             &
     &    ipol%i_filter_magne, itor%i_filter_magne,                     &
     &    iphys%i_filter_magne, icou, trns_SGS)
      call set_field_name_4_bwd_trns(fhd_filter_current,                &
     &    trns_SGS%b_trns%i_filter_current, ipol%i_filter_current,      &
     &    itor%i_filter_current, iphys%i_filter_current,                &
     &    icou, trns_SGS)
!
!      filtered force
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_SGS_inertia, trns_SGS%b_trns%i_SGS_inertia,               &
     &    ipol%i_SGS_inertia, itor%i_SGS_inertia,                       &
     &    iphys%i_SGS_inertia, icou, trns_SGS)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_SGS_Lorentz, trns_SGS%b_trns%i_SGS_Lorentz,               &
     &    ipol%i_SGS_Lorentz, itor%i_SGS_Lorentz,                       &
     &    iphys%i_SGS_Lorentz, icou, trns_SGS)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_SGS_vp_induct, trns_SGS%b_trns%i_SGS_vp_induct,           &
     &    ipol%i_SGS_vp_induct, itor%i_SGS_vp_induct,                   &
     &    iphys%i_SGS_vp_induct, icou, trns_SGS)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_SGS_h_flux, trns_SGS%b_trns%i_SGS_h_flux,                 &
     &    ipol%i_SGS_h_flux, itor%i_SGS_h_flux, iphys%i_SGS_h_flux,     &
     &     icou, trns_SGS)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_SGS_c_flux, trns_SGS%b_trns%i_SGS_c_flux,                 &
     &    ipol%i_SGS_c_flux, itor%i_SGS_c_flux, iphys%i_SGS_c_flux,     &
     &    icou, trns_SGS)
!
      end subroutine set_b_trans_vector_field_SGS
!
!-----------------------------------------------------------------------
!
      subroutine set_b_trans_scalar_field_SGS                          &
     &         (icou, ipol, itor, iphys, trns_SGS)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_4_sph_trans), intent(inout) :: trns_SGS
      integer(kind = kint), intent(inout) :: icou
!
!
!   filtered field
      call set_field_name_4_bwd_trns(fhd_filter_temp,                   &
     &    trns_SGS%b_trns%i_filter_temp, ipol%i_filter_temp,            &
     &    itor%i_filter_temp, iphys%i_filter_temp, icou, trns_SGS)
      call set_field_name_4_bwd_trns(fhd_filter_comp,                   &
     &    trns_SGS%b_trns%i_filter_comp, ipol%i_filter_comp,            &
     &    itor%i_filter_comp, iphys%i_filter_comp, icou, trns_SGS)
!
      end subroutine set_b_trans_scalar_field_SGS
!
!-----------------------------------------------------------------------
!
      end module address_bwd_sph_trans_SGS

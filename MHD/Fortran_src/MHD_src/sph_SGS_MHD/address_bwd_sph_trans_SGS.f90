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
!!      subroutine b_trans_address_scalar_SGS(trns_SGS)
!!        type(phys_address), intent(in) :: ipol
!!        type(address_4_sph_trans), intent(inout) :: trns_SGS
!!
!!      subroutine set_b_trans_vector_field_SGS                         &
!!     &         (icou, ipol, iphys, trns_SGS)
!!      subroutine set_b_trans_scalar_field_SGS                         &
!!     &         (icou, ipol, iphys, trns_SGS)
!!        type(phys_address), intent(in) :: ipol, iphys
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
      subroutine b_trans_address_scalar_SGS(trns_SGS)
!
      type(address_4_sph_trans), intent(inout) :: trns_SGS
!
!
      trns_SGS%nscalar_rj_2_rtp = 0
!      call add_scalar_trans_flag(ipol%i_wide_fil_temp,                 &
!     &    trns_SGS%nvector_rj_2_rtp, trns_SGS%nscalar_rj_2_rtp,        &
!     &    trns_SGS%b_trns%i_wide_fil_temp)
!
      end subroutine b_trans_address_scalar_SGS
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_b_trans_vector_field_SGS                           &
     &         (icou, ipol, iphys, trns_SGS)
!
      type(phys_address), intent(in) :: ipol, iphys
      type(address_4_sph_trans), intent(inout) :: trns_SGS
      integer(kind = kint), intent(inout) :: icou
!
!      filtered force
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_SGS_inertia, trns_SGS%b_trns%i_SGS_inertia,               &
     &    ipol%i_SGS_inertia, iphys%i_SGS_inertia, icou, trns_SGS)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_SGS_Lorentz, trns_SGS%b_trns%i_SGS_Lorentz,               &
     &    ipol%i_SGS_Lorentz, iphys%i_SGS_Lorentz, icou, trns_SGS)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_SGS_vp_induct, trns_SGS%b_trns%i_SGS_vp_induct,           &
     &    ipol%i_SGS_vp_induct, iphys%i_SGS_vp_induct, icou, trns_SGS)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_SGS_h_flux, trns_SGS%b_trns%i_SGS_h_flux,                 &
     &    ipol%i_SGS_h_flux, iphys%i_SGS_h_flux, icou, trns_SGS)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_SGS_c_flux, trns_SGS%b_trns%i_SGS_c_flux,                 &
     &    ipol%i_SGS_c_flux, iphys%i_SGS_c_flux, icou, trns_SGS)
!
!
!   filtered fields
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_filter_velo, trns_SGS%b_trns%i_filter_velo,               &
     &     ipol%i_filter_velo, iphys%i_filter_velo, icou, trns_SGS)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_filter_vort, trns_SGS%b_trns%i_filter_vort,               &
     &    ipol%i_filter_vort, iphys%i_filter_vort, icou, trns_SGS)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_filter_magne, trns_SGS%b_trns%i_filter_magne,             &
     &    ipol%i_filter_magne, iphys%i_filter_magne, icou, trns_SGS)
      call set_field_name_4_bwd_trns(fhd_filter_current,                &
     &    trns_SGS%b_trns%i_filter_current, ipol%i_filter_current,      &
     &    iphys%i_filter_current, icou, trns_SGS)
!
      end subroutine set_b_trans_vector_field_SGS
!
!-----------------------------------------------------------------------
!
!      subroutine set_b_trans_scalar_field_SGS                          &
!     &         (icou, ipol, iphys, trns_SGS)
!
!      type(phys_address), intent(in) :: ipol, iphys
!      type(address_4_sph_trans), intent(inout) :: trns_SGS
!      integer(kind = kint), intent(inout) :: icou
!
!
!      call set_field_name_4_bwd_trns(fhd_filter_temp,                  &
!     &    trns_SGS%b_trns%i_filter_temp, ipol%i_filter_temp,           &
!     &    iphys%i_filter_temp, icou, trns_SGS)
!
!      end subroutine set_b_trans_scalar_field_SGS
!
!-----------------------------------------------------------------------
!
      end module address_bwd_sph_trans_SGS

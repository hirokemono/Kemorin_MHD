!>@file   address_fwd_sph_trans_MHD.f90
!!@brief  module address_fwd_sph_trans_MHD
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine f_trans_address_vector_MHD                          &
!!     &         (fl_prop, cd_prop, ht_prop, cp_prop, ipol, trns_MHD)
!!      subroutine f_trans_address_scalar_MHD(fl_prop, trns_MHD)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in)  :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_address), intent(inout) :: b_trns
!!
!!      subroutine set_f_trans_vector_field_MHD                         &
!!     &         (icou, ipol, itor, iphys, trns_MHD)
!!      subroutine set_f_trans_scalar_field_MHD                         &
!!     &         (icou, ipol, itor, iphys, trns_MHD)
!!        type(phys_address), intent(in) :: ipol, itor, iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_MHD
!!@endverbatim
!
      module address_fwd_sph_trans_MHD
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
      subroutine f_trans_address_vector_MHD                             &
     &         (fl_prop, cd_prop, ht_prop, cp_prop, ipol, trns_MHD)
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in)  :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(phys_address), intent(in) :: ipol
      type(address_4_sph_trans), intent(inout) :: trns_MHD
!
!
      trns_MHD%nvector_rtp_2_rj = 0
!   advection flag
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
        call add_vector_trans_flag(ipol%i_m_advect,                     &
     &      trns_MHD%nvector_rtp_2_rj, trns_MHD%f_trns%i_m_advect)
!   Coriolis flag
        if(fl_prop%iflag_4_coriolis .gt. id_turn_OFF) then
          call add_vector_trans_flag(ipol%i_coriolis,                   &
     &        trns_MHD%nvector_rtp_2_rj, trns_MHD%f_trns%i_coriolis)
        end if
        if(fl_prop%iflag_4_coriolis .gt. id_turn_OFF) then
          call add_vector_trans_flag(ipol%i_rot_Coriolis,               &
     &       trns_MHD%nvector_rtp_2_rj, trns_MHD%f_trns%i_rot_Coriolis)
        end if
!   Lorentz flag
        if(fl_prop%iflag_4_lorentz .gt. id_turn_OFF) then
          call add_vector_trans_flag(ipol%i_lorentz,                    &
     &        trns_MHD%nvector_rtp_2_rj, trns_MHD%f_trns%i_lorentz)
        end if
      end if
!
!   induction flag
      if(cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        call add_vector_trans_flag(ipol%i_vp_induct,                    &
     &      trns_MHD%nvector_rtp_2_rj, trns_MHD%f_trns%i_vp_induct)
      end if
!
!   heat flux flag
      if(ht_prop%iflag_scheme .gt. id_no_evolution) then
        call add_vector_trans_flag(ipol%i_h_flux,                       &
     &      trns_MHD%nvector_rtp_2_rj, trns_MHD%f_trns%i_h_flux)
      end if
!
!   composition flux flag
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        call add_vector_trans_flag(ipol%i_c_flux,                       &
     &      trns_MHD%nvector_rtp_2_rj, trns_MHD%f_trns%i_c_flux)
      end if
!
!
!   filtered advection flag
      call add_vector_trans_flag(ipol%i_SGS_inertia,                    &
     &    trns_MHD%nvector_rtp_2_rj, trns_MHD%f_trns%i_SGS_inertia)
!
!   filtered Lorentz force flag
      call add_vector_trans_flag(ipol%i_SGS_Lorentz,                    &
     &    trns_MHD%nvector_rtp_2_rj, trns_MHD%f_trns%i_SGS_Lorentz)
!
!   filtered induction flag
      call add_vector_trans_flag(ipol%i_SGS_vp_induct,                  &
     &    trns_MHD%nvector_rtp_2_rj, trns_MHD%f_trns%i_SGS_vp_induct)
!
!   filtered heat flux flag
      call add_vector_trans_flag(ipol%i_SGS_h_flux,                     &
     &    trns_MHD%nvector_rtp_2_rj, trns_MHD%f_trns%i_SGS_h_flux)
!
!   filtered composition flux flag
      call add_vector_trans_flag(ipol%i_SGS_c_flux,                     &
     &    trns_MHD%nvector_rtp_2_rj, trns_MHD%f_trns%i_SGS_c_flux)
!
      end subroutine f_trans_address_vector_MHD
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_scalar_MHD(fl_prop, trns_MHD)
!
      type(fluid_property), intent(in) :: fl_prop
      type(address_4_sph_trans), intent(inout) :: trns_MHD
!
!
      trns_MHD%nscalar_rtp_2_rj = 0
!   divergence of Coriolis flux flag
      call add_scalar_trans_flag(fl_prop%iflag_4_coriolis,              &
     &    trns_MHD%nvector_rtp_2_rj, trns_MHD%nscalar_rtp_2_rj,         &
     &    trns_MHD%f_trns%i_div_Coriolis)
!
      end subroutine f_trans_address_scalar_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_f_trans_vector_field_MHD                           &
     &         (icou, ipol, itor, iphys, trns_MHD)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      integer(kind = kint), intent(inout) :: icou
!
!
!   advection flag
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_inertia, trns_MHD%f_trns%i_m_advect, ipol%i_m_advect,     &
     &    itor%i_m_advect, iphys%i_m_advect, icou, trns_MHD)
!
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_Coriolis, trns_MHD%f_trns%i_coriolis, ipol%i_coriolis,    &
     &    itor%i_coriolis, iphys%i_coriolis, icou, trns_MHD)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_rot_Coriolis, trns_MHD%f_trns%i_rot_Coriolis,             &
     &    ipol%i_rot_Coriolis, itor%i_rot_Coriolis,                     &
     &    iphys%i_rot_Coriolis, icou, trns_MHD)
!
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_Lorentz, trns_MHD%f_trns%i_lorentz, ipol%i_lorentz,       &
     &    itor%i_lorentz, iphys%i_lorentz, icou, trns_MHD)
!
!   induction flag
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_vp_induct, trns_MHD%f_trns%i_vp_induct, ipol%i_vp_induct, &
     &    itor%i_vp_induct, iphys%i_vp_induct, icou, trns_MHD)
!
!   heat flux flag
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_h_flux, trns_MHD%f_trns%i_h_flux, ipol%i_h_flux,          &
     &    itor%i_h_flux, iphys%i_h_flux, icou, trns_MHD)
!
!   composition flux flag
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_c_flux, trns_MHD%f_trns%i_c_flux, ipol%i_c_flux,          &
     &    itor%i_c_flux, iphys%i_c_flux, icou, trns_MHD)
!
!
!   SGS advection flag
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_SGS_inertia, trns_MHD%f_trns%i_SGS_inertia,               &
     &    ipol%i_SGS_inertia, itor%i_SGS_inertia, iphys%i_SGS_inertia,  &
     &    icou, trns_MHD)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_SGS_Lorentz, trns_MHD%f_trns%i_SGS_Lorentz,               &
     &    ipol%i_SGS_Lorentz, itor%i_SGS_Lorentz, iphys%i_SGS_Lorentz,  &
     &    icou, trns_MHD)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_SGS_vp_induct, trns_MHD%f_trns%i_SGS_vp_induct,           &
     &    ipol%i_SGS_vp_induct, itor%i_SGS_vp_induct,                   &
     &    iphys%i_SGS_vp_induct, icou, trns_MHD)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_SGS_h_flux, trns_MHD%f_trns%i_SGS_h_flux,                 &
     &    ipol%i_SGS_h_flux, itor%i_SGS_h_flux, iphys%i_SGS_h_flux,     &
     &    icou, trns_MHD)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_SGS_c_flux, trns_MHD%f_trns%i_SGS_c_flux,                 &
     &    ipol%i_SGS_c_flux, itor%i_SGS_c_flux, iphys%i_SGS_c_flux,     &
     &    icou, trns_MHD)
!
      end subroutine set_f_trans_vector_field_MHD
!
!-----------------------------------------------------------------------
!
      subroutine set_f_trans_scalar_field_MHD                           &
     &         (icou, ipol, itor, iphys, trns_MHD)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      integer(kind = kint), intent(inout) :: icou
!
!
!   divergence of Coriolis flux flag
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_div_Coriolis, trns_MHD%f_trns%i_div_Coriolis,             &
     &    ipol%i_div_Coriolis, itor%i_div_Coriolis,                     &
     &    iphys%i_div_Coriolis, icou, trns_MHD)
!
      end subroutine set_f_trans_scalar_field_MHD
!
!-----------------------------------------------------------------------
!
      end module address_fwd_sph_trans_MHD

!>@file   address_bwd_sph_trans_MHD.f90
!!@brief  module address_bwd_sph_trans_MHD
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine b_trans_address_vector_MHD                           &
!!     &         (fl_prop, cd_prop, ht_prop, cp_prop, ipol, trns_MHD)
!!      subroutine b_trans_address_scalar_MHD                           &
!!     &         (ht_prop, cp_prop, ipol, trns_MHD)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in)  :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(phys_address), intent(in) :: ipol
!!        type(phys_address), intent(inout) :: b_trns
!!
!!      subroutine set_b_trans_vector_field_MHD                         &
!!     &         (icou, ipol, itor, iphys, trns_MHD)
!!      subroutine set_b_trans_scalar_field_MHD                         &
!!     &         (icou, ipol, itor, iphys, trns_MHD)
!!        type(phys_address), intent(in) :: ipol, itor, iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_MHD
!!@endverbatim
!
      module address_bwd_sph_trans_MHD
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
      subroutine b_trans_address_vector_MHD                             &
     &         (fl_prop, cd_prop, ht_prop, cp_prop, ipol, trns_MHD)
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in)  :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(phys_address), intent(in) :: ipol
      type(address_4_sph_trans), intent(inout) :: trns_MHD
!
      trns_MHD%nvector_rj_2_rtp = 0
!   velocity flag
      if(       fl_prop%iflag_scheme .gt. id_no_evolution               &
     &     .or. cd_prop%iflag_Bevo_scheme .gt. id_no_evolution          &
     &     .or. ht_prop%iflag_scheme .gt. id_no_evolution               &
     &     .or. cp_prop%iflag_scheme .gt. id_no_evolution) then
        call add_vector_trans_flag(ipol%i_velo,                         &
     &    trns_MHD%nvector_rj_2_rtp, trns_MHD%b_trns%i_velo)
      end if
!   vorticity flag
      if(       fl_prop%iflag_scheme .gt. id_no_evolution) then
        call add_vector_trans_flag(ipol%i_vort,                         &
     &      trns_MHD%nvector_rj_2_rtp, trns_MHD%b_trns%i_vort)
      end if
!   magnetic field flag
      if(       cd_prop%iflag_Bevo_scheme .gt. id_no_evolution          &
     &     .or. fl_prop%iflag_4_lorentz .gt.     id_turn_OFF) then
        call add_vector_trans_flag(ipol%i_magne,                        &
     &      trns_MHD%nvector_rj_2_rtp, trns_MHD%b_trns%i_magne)
      end if
!   current density flag
      if(fl_prop%iflag_4_lorentz .gt. id_turn_OFF) then
        call add_vector_trans_flag(ipol%i_current,                      &
     &     trns_MHD%nvector_rj_2_rtp, trns_MHD%b_trns%i_current)
      end if
!
      end subroutine b_trans_address_vector_MHD
!
!-----------------------------------------------------------------------
!
      subroutine b_trans_address_scalar_MHD                             &
     &         (ht_prop, cp_prop, ipol, trns_MHD)
!
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(phys_address), intent(in) :: ipol
      type(address_4_sph_trans), intent(inout) :: trns_MHD
!
!
      trns_MHD%nscalar_rj_2_rtp = 0
!   temperature flag
      if(ht_prop%iflag_scheme .gt. id_no_evolution) then
        call add_scalar_trans_flag(ipol%i_temp,                         &
     &      trns_MHD%nvector_rj_2_rtp, trns_MHD%nscalar_rj_2_rtp,       &
     &      trns_MHD%b_trns%i_temp)
      end if
!   composition flag
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        call add_scalar_trans_flag(ipol%i_light,                        &
     &      trns_MHD%nvector_rj_2_rtp, trns_MHD%nscalar_rj_2_rtp,       &
     &      trns_MHD%b_trns%i_light)
      end if
!
      end subroutine b_trans_address_scalar_MHD
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_b_trans_vector_field_MHD                           &
     &         (icou, ipol, itor, iphys, trns_MHD)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      integer(kind = kint), intent(inout) :: icou
!
!   velocity flag
      call set_field_name_4_bwd_trns(fhd_velo, trns_MHD%b_trns%i_velo,  &
     &    ipol%i_velo, itor%i_velo, iphys%i_velo, icou, trns_MHD)
      call set_field_name_4_bwd_trns(fhd_vort, trns_MHD%b_trns%i_vort,  &
     &    ipol%i_vort, itor%i_vort, iphys%i_vort, icou, trns_MHD)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_magne, trns_MHD%b_trns%i_magne, ipol%i_magne,             &
     &    itor%i_magne, iphys%i_magne, icou, trns_MHD)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_current, trns_MHD%b_trns%i_current, ipol%i_current,       &
     &    itor%i_current, iphys%i_current, icou, trns_MHD)
!
      end subroutine set_b_trans_vector_field_MHD
!
!-----------------------------------------------------------------------
!
      subroutine set_b_trans_scalar_field_MHD                           &
     &         (icou, ipol, itor, iphys, trns_MHD)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      integer(kind = kint), intent(inout) :: icou
!
!
!   scalar field
      call set_field_name_4_bwd_trns(fhd_temp, trns_MHD%b_trns%i_temp,  &
     &    ipol%i_temp, itor%i_temp, iphys%i_temp, icou, trns_MHD)
      call set_field_name_4_bwd_trns                                    &
     &   (fhd_light, trns_MHD%b_trns%i_light,                           &
     &    ipol%i_light, itor%i_light, iphys%i_light, icou, trns_MHD)
!
      end subroutine set_b_trans_scalar_field_MHD
!
!-----------------------------------------------------------------------
!
      end module address_bwd_sph_trans_MHD

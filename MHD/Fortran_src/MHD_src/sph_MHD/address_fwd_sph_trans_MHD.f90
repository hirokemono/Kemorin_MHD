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
!!      subroutine f_trans_address_vector_MHD                           &
!!     &         (fl_prop, cd_prop, ht_prop, cp_prop, ipol, itor, iphys,&
!!     &          f_trns, trns_fwd)
!!      subroutine f_trans_address_scalar_MHD                           &
!!     &         (ipol, itor, iphys, f_trns, trns_fwd)
!!        type(fluid_property), intent(in) :: fl_prop
!!        type(conductive_property), intent(in)  :: cd_prop
!!        type(scalar_property), intent(in) :: ht_prop, cp_prop
!!        type(phys_address), intent(in) :: ipol, itor, iphys
!!        type(address_each_sph_trans), intent(inout) :: trns_fwd
!!        type(phys_address), intent(inout) :: f_trns
!!@endverbatim
!
      module address_fwd_sph_trans_MHD
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
      subroutine f_trans_address_vector_MHD                             &
     &         (fl_prop, cd_prop, ht_prop, cp_prop, ipol, itor, iphys,  &
     &          f_trns, trns_fwd)
!
      type(fluid_property), intent(in) :: fl_prop
      type(conductive_property), intent(in)  :: cd_prop
      type(scalar_property), intent(in) :: ht_prop, cp_prop
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_fwd
      type(phys_address), intent(inout) :: f_trns
!
!
      trns_fwd%nfield = 0
      call alloc_sph_trns_field_name(trns_fwd)
!
!   advection flag
      if(fl_prop%iflag_scheme .gt. id_no_evolution) then
        call add_field_name_4_sph_trns                                  &
     &     (ipol%forces%i_m_advect, inertia%name, inertia%n_comp,       &
     &      ipol%forces%i_m_advect, itor%forces%i_m_advect,             &
     &      iphys%forces%i_m_advect, f_trns%forces%i_m_advect,          &
     &      trns_fwd)
!   Coriolis flag
        if(fl_prop%iflag_4_coriolis .gt. id_turn_OFF) then
          call add_field_name_4_sph_trns(ipol%forces%i_coriolis,        &
     &        Coriolis_force%name, Coriolis_force%n_comp,               &
     &        ipol%forces%i_coriolis, itor%forces%i_coriolis,           &
     &        iphys%forces%i_coriolis, f_trns%forces%i_coriolis,        &
     &        trns_fwd)
        end if
        if(fl_prop%iflag_4_coriolis .gt. id_turn_OFF) then
          call add_field_name_4_sph_trns(ipol%rot_forces%i_Coriolis,    &
     &        rot_Coriolis_force%name, rot_Coriolis_force%n_comp,       &
     &        ipol%rot_forces%i_Coriolis, itor%rot_forces%i_Coriolis,   &
     &        iphys%rot_forces%i_Coriolis,                              &
     &        f_trns%rot_forces%i_Coriolis, trns_fwd)
        end if
!   Lorentz flag
        if(fl_prop%iflag_4_lorentz .gt. id_turn_OFF) then
          call add_field_name_4_sph_trns(ipol%forces%i_lorentz,         &
     &        Lorentz_force%name, Lorentz_force%n_comp,                 &
     &        ipol%forces%i_lorentz, itor%forces%i_lorentz,             &
     &        iphys%forces%i_lorentz, f_trns%forces%i_lorentz,          &
     &        trns_fwd)
        end if
      end if
!
!   induction flag
      if(cd_prop%iflag_Bevo_scheme .gt. id_no_evolution) then
        call add_field_name_4_sph_trns(ipol%forces%i_vp_induct,         &
     &      vecp_induction%name, vecp_induction%n_comp,                 &
     &      ipol%forces%i_vp_induct, itor%forces%i_vp_induct,           &
     &      iphys%forces%i_vp_induct, f_trns%forces%i_vp_induct,        &
     &      trns_fwd)
      end if
!
!   heat flux flag
      if(ht_prop%iflag_scheme .gt. id_no_evolution) then
        call add_field_name_4_sph_trns                                  &
     &     (ipol%forces%i_h_flux, heat_flux%name, heat_flux%n_comp,     &
     &      ipol%forces%i_h_flux, itor%forces%i_h_flux,                 &
     &      iphys%forces%i_h_flux, f_trns%forces%i_h_flux, trns_fwd)
      end if
!
!   composition flux flag
      if(cp_prop%iflag_scheme .gt. id_no_evolution) then
        call add_field_name_4_sph_trns(ipol%forces%i_c_flux,            &
     &      composite_flux%name, composite_flux%n_comp,                 &
     &      ipol%forces%i_c_flux, itor%forces%i_c_flux,                 &
     &      iphys%forces%i_c_flux, f_trns%forces%i_c_flux, trns_fwd)
      end if
      trns_fwd%num_vector = trns_fwd%nfield
!
      end subroutine f_trans_address_vector_MHD
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_scalar_MHD                             &
     &         (ipol, itor, iphys, f_trns, trns_fwd)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_fwd
      type(phys_address), intent(inout) :: f_trns
!
!
!   divergence of Coriolis flux flag
      call add_field_name_4_sph_trns(ipol%i_div_Coriolis,               &
     &    div_Coriolis_force%name, div_Coriolis_force%n_comp,           &
     &    ipol%i_div_Coriolis, itor%i_div_Coriolis,                     &
     &    iphys%i_div_Coriolis, f_trns%i_div_Coriolis, trns_fwd)
      trns_fwd%num_scalar = trns_fwd%nfield - trns_fwd%num_vector
!
      end subroutine f_trans_address_scalar_MHD
!
!-----------------------------------------------------------------------
!
      end module address_fwd_sph_trans_MHD

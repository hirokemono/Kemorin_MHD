!>@file   address_fwd_sph_trans_Csim.f90
!!@brief  module address_fwd_sph_trans_Csim
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine f_trans_address_vector_Csim(trns_Csim)
!!      subroutine f_trans_address_scalar_Csim(ipol, trns_Csim)
!!        type(phys_address), intent(in) :: ipol
!!        type(address_4_sph_trans), intent(inout) :: trns_Csim
!!
!!      subroutine set_f_trans_vector_field_Csim                        &
!!     &         (icou, ipol, itor, iphys, trns_Csim)
!!      subroutine set_f_trans_scalar_field_Csim                        &
!!     &         (icou, ipol, itor, iphys, trns_Csim)
!!        type(phys_address), intent(in) :: ipol, itor, iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_Csim
!!@endverbatim
!
      module address_fwd_sph_trans_Csim
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
      subroutine f_trans_address_vector_Csim(trns_Csim)
!
      type(address_4_sph_trans), intent(inout) :: trns_Csim
!
!
      trns_Csim%forward%num_vector = 0
!   SGS advection flag
!      call add_vector_trans_flag(ipol%i_SGS_inertia,                   &
!     &    trns_Csim%forward%num_vector, trns_Csim%f_trns%i_SGS_inertia)
!
      end subroutine f_trans_address_vector_Csim
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_scalar_Csim(ipol, trns_Csim)
!
      type(phys_address), intent(in) :: ipol
      type(address_4_sph_trans), intent(inout) :: trns_Csim
!
!
      trns_Csim%forward%num_scalar = 0
!   SGS advection flag
      call add_scalar_trans_flag(ipol%i_Csim_SGS_m_flux,                &
     &    trns_Csim%forward%num_vector, trns_Csim%forward%num_scalar,       &
     &    trns_Csim%f_trns%i_Csim_SGS_m_flux)
!   SGS Lorentz force flag
      call add_scalar_trans_flag(ipol%i_Csim_SGS_Lorentz,               &
     &    trns_Csim%forward%num_vector, trns_Csim%forward%num_scalar,       &
     &    trns_Csim%f_trns%i_Csim_SGS_Lorentz)
!   SGS induction flag
      call add_scalar_trans_flag(ipol%i_Csim_SGS_induction,             &
     &    trns_Csim%forward%num_vector, trns_Csim%forward%num_scalar,       &
     &    trns_Csim%f_trns%i_Csim_SGS_induction)
!   SGS heat flux flag
      call add_scalar_trans_flag(ipol%i_Csim_SGS_h_flux,                &
     &    trns_Csim%forward%num_vector, trns_Csim%forward%num_scalar,       &
     &    trns_Csim%f_trns%i_Csim_SGS_h_flux)
!   SGS composition flux flag
      call add_scalar_trans_flag(ipol%i_Csim_SGS_c_flux,                &
     &    trns_Csim%forward%num_vector, trns_Csim%forward%num_scalar,       &
     &    trns_Csim%f_trns%i_Csim_SGS_c_flux)
!
!   SGS buoyancy
      call add_scalar_trans_flag(ipol%i_Csim_SGS_buoyancy,              &
     &    trns_Csim%forward%num_vector, trns_Csim%forward%num_scalar,       &
     &    trns_Csim%f_trns%i_Csim_SGS_buoyancy)
!   SGS compostional buoyancy
      call add_scalar_trans_flag(ipol%i_Csim_SGS_comp_buo,              &
     &    trns_Csim%forward%num_vector, trns_Csim%forward%num_scalar,       &
     &    trns_Csim%f_trns%i_Csim_SGS_comp_buo)
!
!
      end subroutine f_trans_address_scalar_Csim
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_f_trans_vector_field_Csim                          &
     &         (icou, ipol, itor, iphys, trns_Csim)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_4_sph_trans), intent(inout) :: trns_Csim
      integer(kind = kint), intent(inout) :: icou
!
!
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_SGS_inertia, trns_Csim%f_trns%i_SGS_inertia,              &
     &    ipol%i_SGS_inertia, itor%i_SGS_inertia, iphys%i_SGS_inertia,  &
     &    icou, trns_Csim)
!
      end subroutine set_f_trans_vector_field_Csim
!
!-----------------------------------------------------------------------
!
      subroutine set_f_trans_scalar_field_Csim                          &
     &         (icou, ipol, itor, iphys, trns_Csim)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_4_sph_trans), intent(inout) :: trns_Csim
      integer(kind = kint), intent(inout) :: icou
!
!
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_Csim_SGS_m_flux, trns_Csim%f_trns%i_Csim_SGS_m_flux,      &
     &    ipol%i_Csim_SGS_m_flux, itor%i_Csim_SGS_m_flux,               &
     &    iphys%i_Csim_SGS_m_flux, icou, trns_Csim)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_Csim_SGS_Lorentz, trns_Csim%f_trns%i_Csim_SGS_Lorentz,    &
     &    ipol%i_Csim_SGS_Lorentz, itor%i_Csim_SGS_Lorentz,             &
     &    iphys%i_Csim_SGS_Lorentz, icou, trns_Csim)
      call set_field_name_4_fwd_trns                                    &
     &  (fhd_Csim_SGS_induction, trns_Csim%f_trns%i_Csim_SGS_induction, &
     &   ipol%i_Csim_SGS_induction, itor%i_Csim_SGS_induction,          &
     &   iphys%i_Csim_SGS_induction, icou, trns_Csim)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_Csim_SGS_h_flux, trns_Csim%f_trns%i_Csim_SGS_h_flux,      &
     &    ipol%i_Csim_SGS_h_flux, itor%i_Csim_SGS_h_flux,               &
     &    iphys%i_Csim_SGS_h_flux, icou, trns_Csim)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_Csim_SGS_c_flux, trns_Csim%f_trns%i_Csim_SGS_c_flux,      &
     &    ipol%i_Csim_SGS_c_flux, itor%i_Csim_SGS_c_flux,               &
     &    iphys%i_Csim_SGS_c_flux, icou, trns_Csim)
!
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_Csim_SGS_buoyancy, trns_Csim%f_trns%i_Csim_SGS_buoyancy,  &
     &    ipol%i_Csim_SGS_buoyancy, itor%i_Csim_SGS_buoyancy,           &
     &    iphys%i_Csim_SGS_buoyancy, icou, trns_Csim)
      call set_field_name_4_fwd_trns                                    &
     &   (fhd_Csim_SGS_comp_buo, trns_Csim%f_trns%i_Csim_SGS_comp_buo,  &
     &    ipol%i_Csim_SGS_comp_buo, itor%i_Csim_SGS_comp_buo,           &
     &    iphys%i_Csim_SGS_comp_buo, icou, trns_Csim)
!
      end subroutine set_f_trans_scalar_field_Csim
!
!-----------------------------------------------------------------------
!
      end module address_fwd_sph_trans_Csim

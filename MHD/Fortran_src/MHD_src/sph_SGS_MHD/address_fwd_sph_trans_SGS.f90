!>@file   address_fwd_sph_trans_SGS.f90
!!@brief  module address_fwd_sph_trans_SGS
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine f_trans_address_vector_SGS                           &
!!     &         (ipol, itor, iphys, f_trns, trns_SGS)
!!      subroutine f_trans_address_scalar_SGS(trns_fwd)
!!        type(phys_address), intent(in) :: ipol, itor, iphys
!!        type(address_each_sph_trans), intent(inout) :: trns_fwd
!!        type(phys_address), intent(inout) :: f_trns
!!@endverbatim
!
      module address_fwd_sph_trans_SGS
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
      subroutine f_trans_address_vector_SGS                             &
     &         (ipol, itor, iphys, f_trns, trns_fwd)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_fwd
      type(phys_address), intent(inout) :: f_trns
!
!
      trns_fwd%nfield = 0
      call alloc_sph_trns_field_name(trns_fwd)
!
!   SGS advection flag
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_SGS_inertia, fhd_SGS_inertia, n_vector,                &
     &    ipol%i_SGS_inertia, itor%i_SGS_inertia, iphys%i_SGS_inertia,  &
     &    f_trns%i_SGS_inertia, trns_fwd)
!   SGS Lorentz force flag
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_SGS_Lorentz, fhd_SGS_Lorentz, n_vector,                &
     &    ipol%i_SGS_Lorentz, itor%i_SGS_Lorentz, iphys%i_SGS_Lorentz,  &
     &    f_trns%i_SGS_Lorentz, trns_fwd)
!   SGS induction flag
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_SGS_vp_induct, fhd_SGS_vp_induct, n_vector,            &
     &    ipol%i_SGS_vp_induct, itor%i_SGS_vp_induct,                   &
     &    iphys%i_SGS_vp_induct, f_trns%i_SGS_vp_induct, trns_fwd)
!   SGS heat flux flag
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_SGS_h_flux, fhd_SGS_h_flux, n_vector,                  &
     &    ipol%i_SGS_h_flux, itor%i_SGS_h_flux, iphys%i_SGS_h_flux,     &
     &    f_trns%i_SGS_h_flux, trns_fwd)
!   SGS composition flux flag
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_SGS_c_flux, fhd_SGS_c_flux, n_vector,                  &
     &    ipol%i_SGS_c_flux, itor%i_SGS_c_flux, iphys%i_SGS_c_flux,     &
     &    f_trns%i_SGS_c_flux, trns_fwd)
      trns_fwd%num_vector = trns_fwd%nfield
!
      end subroutine f_trans_address_vector_SGS
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_scalar_SGS(trns_fwd)
!
!      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_fwd
!      type(phys_address), intent(inout) :: f_trns
!
!
      trns_fwd%num_scalar = 0
!
      end subroutine f_trans_address_scalar_SGS
!
!-----------------------------------------------------------------------
!
      end module address_fwd_sph_trans_SGS

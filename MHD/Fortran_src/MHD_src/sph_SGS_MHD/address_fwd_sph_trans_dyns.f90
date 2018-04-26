!>@file   address_fwd_sph_trans_dyns.f90
!!@brief  module address_fwd_sph_trans_dyns
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2010
!
!>@brief Field addresses for spherical harmonics transform
!!       in MHD dynamo simulation
!!
!!@verbatim
!!      subroutine f_trans_address_vector_DYNS(trns_fwd)
!!      subroutine f_trans_address_scalar_DYNS                          &
!!     &         (ipol, itor, iphys, f_trns, trns_fwd)
!!        type(phys_address), intent(in) :: ipol, itor, iphys
!!        type(address_each_sph_trans), intent(inout) :: trns_fwd
!!        type(phys_address), intent(inout) :: f_trns
!!@endverbatim
!
      module address_fwd_sph_trans_dyns
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
      subroutine f_trans_address_vector_DYNS(trns_fwd)
!
!      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_fwd
!      type(phys_address), intent(inout) :: f_trns
!
!
      trns_fwd%nfield = 0
      call alloc_sph_trns_field_name(trns_fwd)
!
      trns_fwd%num_vector = 0
!
      end subroutine f_trans_address_vector_DYNS
!
!-----------------------------------------------------------------------
!
      subroutine f_trans_address_scalar_DYNS                            &
     &         (ipol, itor, iphys, f_trns, trns_fwd)
!
      type(phys_address), intent(in) :: ipol, itor, iphys
      type(address_each_sph_trans), intent(inout) :: trns_fwd
      type(phys_address), intent(inout) :: f_trns
!
!
!   work of Reynolds stress
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_reynolds_wk, fhd_Reynolds_work, n_scalar,              &
     &    ipol%i_reynolds_wk, itor%i_reynolds_wk,                       &
     &    iphys%i_reynolds_wk, f_trns%i_reynolds_wk, trns_fwd)
!   work of SGS buoyancy
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_SGS_buo_wk, fhd_SGS_buo_flux, n_scalar,                &
     &    ipol%i_SGS_buo_wk, itor%i_SGS_buo_wk,                         &
     &    iphys%i_SGS_buo_wk, f_trns%i_SGS_buo_wk, trns_fwd)
!   work of SGS compositional buoyancy
      call add_field_name_4_sph_trns                                    &
     &   (ipol%i_SGS_comp_buo_wk, fhd_SGS_comp_buo_flux, n_scalar,      &
     &    ipol%i_SGS_comp_buo_wk, itor%i_SGS_comp_buo_wk,               &
     &    iphys%i_SGS_comp_buo_wk, f_trns%i_SGS_comp_buo_wk, trns_fwd)
      trns_fwd%num_scalar = trns_fwd%nfield - trns_fwd%num_vector
!
      end subroutine f_trans_address_scalar_DYNS
!
!-----------------------------------------------------------------------
!
      end module address_fwd_sph_trans_dyns

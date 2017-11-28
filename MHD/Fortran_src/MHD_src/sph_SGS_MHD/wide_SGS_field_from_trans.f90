!>@file   wide_SGS_field_from_trans.f90
!!@brief  module wide_SGS_field_from_trans
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Copy data from/to sphrical transform buffer for snapshots
!!
!!@verbatim
!!      subroutine copy_wide_SGS_field_from_trans                       &
!!     &         (sph_params, sph_rtp, trns_SGS, node, iphys, nod_fld)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(address_4_sph_trans), intent(in) :: trns_MHD
!!        type(address_4_sph_trans), intent(in) :: trns_SGS
!!        type(address_4_sph_trans), intent(in) :: trns_snap
!!        type(node_data), intent(in) :: node
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module wide_SGS_field_from_trans
!
      use m_precision
      use m_machine_parameter
!
      use t_geometry_data
      use t_phys_address
      use t_phys_data
      use t_spheric_parameter
      use t_addresses_sph_transform
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_wide_SGS_field_from_trans                         &
     &         (sph_params, sph_rtp, trns_SGS, node, iphys, nod_fld)
!
      use copy_fields_from_sph_trans
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(in) :: trns_SGS
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_SGS%b_trns%i_wide_SGS_inertia, iphys%i_wide_SGS_inertia, &
     &    sph_params%m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_SGS%b_trns%i_wide_SGS_Lorentz, iphys%i_wide_SGS_Lorentz, &
     &    sph_params%m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_SGS%b_trns%i_wide_SGS_vp_induct,                         &
     &    iphys%i_wide_SGS_vp_induct,                                   &
     &    sph_params%m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_SGS%b_trns%i_wide_SGS_h_flux, iphys%i_wide_SGS_h_flux,   &
     &    sph_params%m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_SGS%b_trns%i_wide_SGS_c_flux, iphys%i_wide_SGS_c_flux,   &
     &    sph_params%m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
!
      end subroutine copy_wide_SGS_field_from_trans
!
!-----------------------------------------------------------------------
!
      end module wide_SGS_field_from_trans

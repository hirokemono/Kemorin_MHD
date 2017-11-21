!>@file   filtered_forces_to_snap.f90
!!@brief  module filtered_forces_to_snap
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Copy spectrum data and field data to spherical transform buffer
!!       for dynamo simulation
!!
!!@verbatim
!!  routines for backward transform
!!      subroutine copy_filtered_forces_to_snap                         &
!!     &          (sph_params, sph_rtp, trns_MHD, node, iphys, nod_fld)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(address_4_sph_trans), intent(in) :: trns_MHD
!!        type(node_data), intent(in) :: node
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!      subroutine zmean_filtered_forces_to_snap(sph_params, sph_rtp,   &
!!     &          trns_MHD, node, iphys, nod_fld)
!!      subroutine zrms_filtered_forces_to_snap(sph_params, sph_rtp,    &
!!     &          trns_MHD, node, iphys, nod_fld)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(node_data), intent(in) :: node
!!        type(phys_address), intent(in) :: iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_MHD
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module filtered_forces_to_snap
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
      subroutine copy_filtered_forces_to_snap                           &
     &          (sph_params, sph_rtp, trns_MHD, node, iphys, nod_fld)
!
      use copy_fields_from_sph_trans
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(in) :: trns_MHD
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
!
      type(phys_data), intent(inout) :: nod_fld
!
!
!   advection flag
      call copy_vector_from_snap_force                                  &
     &   (trns_MHD%f_trns%i_SGS_inertia, iphys%i_SGS_inertia,           &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!   Lorentz flag
      call copy_vector_from_snap_force                                  &
     &   (trns_MHD%f_trns%i_SGS_Lorentz, iphys%i_SGS_Lorentz,           &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
!   induction flag
      call copy_vector_from_snap_force                                  &
     &   (trns_MHD%f_trns%i_SGS_vp_induct, iphys%i_SGS_vp_induct,       &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
!   divergence of heat flux flag
      call copy_vector_from_snap_force                                  &
     &   (trns_MHD%f_trns%i_SGS_h_flux, iphys%i_SGS_h_flux,             &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
!   divergence of composition flux flag
      call copy_vector_from_snap_force                                  &
     &   (trns_MHD%f_trns%i_SGS_c_flux, iphys%i_SGS_c_flux,             &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      end subroutine copy_filtered_forces_to_snap
!
!-----------------------------------------------------------------------
!
      subroutine zmean_filtered_forces_to_snap(sph_params, sph_rtp,     &
     &          trns_MHD, node, iphys, nod_fld)
!
      use copy_fields_from_sph_trans
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(phys_data), intent(inout) :: nod_fld
!
!
!   advection flag
      call zmean_vector_from_snap_force                                 &
     &   (trns_MHD%f_trns%i_SGS_inertia, iphys%i_SGS_inertia,           &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!   Lorentz flag
      call zmean_vector_from_snap_force                                 &
     &   (trns_MHD%f_trns%i_SGS_Lorentz, iphys%i_SGS_Lorentz,           &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
!   induction flag
      call zmean_vector_from_snap_force                                 &
     &   (trns_MHD%f_trns%i_SGS_vp_induct, iphys%i_SGS_vp_induct,       &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
!   divergence of heat flux flag
      call zmean_vector_from_snap_force                                 &
     &   (trns_MHD%f_trns%i_SGS_h_flux, iphys%i_SGS_h_flux,             &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
!   divergence of composition flux flag
      call zmean_vector_from_snap_force                                 &
     &   (trns_MHD%f_trns%i_SGS_c_flux, iphys%i_SGS_c_flux,             &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      end subroutine zmean_filtered_forces_to_snap
!
!-----------------------------------------------------------------------
!
      subroutine zrms_filtered_forces_to_snap(sph_params, sph_rtp,      &
     &          trns_MHD, node, iphys, nod_fld)
!
      use copy_fields_from_sph_trans
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
!
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(phys_data), intent(inout) :: nod_fld
!
!
!   advection flag
      call zrms_vector_from_snap_force                                  &
     &   (trns_MHD%f_trns%i_SGS_inertia, iphys%i_SGS_inertia,           &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!   Lorentz flag
      call zrms_vector_from_snap_force                                  &
     &   (trns_MHD%f_trns%i_SGS_Lorentz, iphys%i_SGS_Lorentz,           &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
!   induction flag
      call zrms_vector_from_snap_force                                  &
     &   (trns_MHD%f_trns%i_SGS_vp_induct, iphys%i_SGS_vp_induct,       &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
!   divergence of heat flux flag
      call zrms_vector_from_snap_force                                  &
     &   (trns_MHD%f_trns%i_SGS_h_flux, iphys%i_SGS_h_flux,             &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
!   divergence of composition flux flag
      call zrms_vector_from_snap_force                                  &
     &   (trns_MHD%f_trns%i_SGS_c_flux, iphys%i_SGS_c_flux,             &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      end subroutine zrms_filtered_forces_to_snap
!
!-----------------------------------------------------------------------
!
      end module filtered_forces_to_snap

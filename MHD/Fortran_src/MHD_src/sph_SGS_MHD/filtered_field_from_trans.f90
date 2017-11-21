!>@file   filtered_field_from_trans.f90
!!@brief  module filtered_field_from_trans
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Copy data from/to sphrical transform buffer for snapshots
!!
!!@verbatim
!!      subroutine copy_filtered_field_from_trans                       &
!!     &         (sph_params, sph_rtp, trns_MHD, node, iphys, nod_fld)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(address_4_sph_trans), intent(in) :: trns_MHD
!!        type(node_data), intent(in) :: node
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!      subroutine zmean_filtered_field_from_trans(sph_params, sph_rtp, &
!!     &          trns_MHD, node, iphys, nod_fld)
!!      subroutine zrms_filtered_field_from_trans(sph_params, sph_rtp,  &
!!     &          trns_MHD, node, iphys, nod_fld)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(node_data), intent(in) :: node
!!        type(phys_address), intent(in) :: iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_MHD
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module filtered_field_from_trans
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
      subroutine copy_filtered_field_from_trans                         &
     &         (sph_params, sph_rtp, trns_MHD, node, iphys, nod_fld)
!
      use copy_fields_from_sph_trans
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(in) :: trns_MHD
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
!  Copy vectors
      call copy_vector_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_filter_velo, iphys%i_filter_velo,           &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_filter_vort, iphys%i_filter_vort,           &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_filter_magne, iphys%i_filter_magne,         &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_filter_current, iphys%i_filter_current,     &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_wide_fil_velo, iphys%i_wide_fil_velo,       &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_wide_fil_vort, iphys%i_wide_fil_vort,       &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_wide_fil_magne, iphys%i_wide_fil_magne,     &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_wide_fil_current, iphys%i_wide_fil_current, &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
!  Copy scalars
      call copy_scalar_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_filter_temp, iphys%i_filter_temp,           &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call copy_scalar_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_filter_comp, iphys%i_filter_comp,           &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
!
      call copy_scalar_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_wide_fil_temp, iphys%i_wide_fil_temp,       &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call copy_scalar_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_wide_fil_comp, iphys%i_wide_fil_comp,       &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      end subroutine copy_filtered_field_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine zmean_filtered_field_from_trans(sph_params, sph_rtp,   &
     &          trns_MHD, node, iphys, nod_fld)
!
      use copy_fields_from_sph_trans
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(phys_data), intent(inout) :: nod_fld
!
!
!  Copy vectors
      call zmean_vector_from_snap_trans                                 &
     &   (trns_MHD%b_trns%i_filter_velo, iphys%i_filter_velo,           &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call zmean_vector_from_snap_trans                                 &
     &   (trns_MHD%b_trns%i_filter_vort, iphys%i_filter_vort,           &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call zmean_vector_from_snap_trans                                 &
     &   (trns_MHD%b_trns%i_filter_magne, iphys%i_filter_magne,         &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call zmean_vector_from_snap_trans                                 &
     &   (trns_MHD%b_trns%i_filter_current, iphys%i_filter_current,     &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
!
      call zmean_vector_from_snap_trans                                 &
     &   (trns_MHD%b_trns%i_wide_fil_velo, iphys%i_wide_fil_velo,       &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call zmean_vector_from_snap_trans                                 &
     &   (trns_MHD%b_trns%i_wide_fil_vort, iphys%i_wide_fil_vort,       &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call zmean_vector_from_snap_trans                                 &
     &   (trns_MHD%b_trns%i_wide_fil_magne, iphys%i_wide_fil_magne,     &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call zmean_vector_from_snap_trans                                 &
     &   (trns_MHD%b_trns%i_wide_fil_current, iphys%i_wide_fil_current, &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
!  Copy scalars
      call zmean_scalar_from_snap_trans                                 &
     &   (trns_MHD%b_trns%i_filter_temp, iphys%i_filter_temp,           &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call zmean_scalar_from_snap_trans                                 &
     &   (trns_MHD%b_trns%i_filter_comp, iphys%i_filter_comp,           &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
!
      call zmean_scalar_from_snap_trans                                 &
     &   (trns_MHD%b_trns%i_wide_fil_temp, iphys%i_wide_fil_temp,       &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call zmean_scalar_from_snap_trans                                 &
     &   (trns_MHD%b_trns%i_wide_fil_comp, iphys%i_wide_fil_comp,       &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      end subroutine zmean_filtered_field_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine zrms_filtered_field_from_trans(sph_params, sph_rtp,    &
     &          trns_MHD, node, iphys, nod_fld)
!
      use copy_fields_from_sph_trans
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(inout) :: trns_MHD
      type(phys_data), intent(inout) :: nod_fld
!
!
!  Copy vectors
      call zrms_vector_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_filter_velo, iphys%i_filter_velo,           &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call zrms_vector_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_filter_vort, iphys%i_filter_vort,           &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call zrms_vector_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_filter_magne, iphys%i_filter_magne,         &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call zrms_vector_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_filter_current, iphys%i_filter_current,     &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
!
      call zrms_vector_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_wide_fil_velo, iphys%i_wide_fil_velo,       &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call zrms_vector_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_wide_fil_vort, iphys%i_wide_fil_vort,       &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call zrms_vector_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_wide_fil_magne, iphys%i_wide_fil_magne,     &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call zrms_vector_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_wide_fil_current, iphys%i_wide_fil_current, &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
!  Copy scalars
      call zrms_scalar_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_filter_temp, iphys%i_filter_temp,           &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call zrms_scalar_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_filter_comp, iphys%i_filter_comp,           &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
!
      call zrms_scalar_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_wide_fil_temp, iphys%i_wide_fil_temp,       &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call zrms_scalar_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_wide_fil_comp, iphys%i_wide_fil_comp,       &
     &    sph_params%m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      end subroutine zrms_filtered_field_from_trans
!
!-----------------------------------------------------------------------
!
      end module filtered_field_from_trans

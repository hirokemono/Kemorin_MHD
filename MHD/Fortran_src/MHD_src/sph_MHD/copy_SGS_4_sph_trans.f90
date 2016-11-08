!>@file   copy_SGS_4_sph_trans.f90
!!@brief  module copy_SGS_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Copy data from/to sphrical transform buffer for snapshots
!!
!!@verbatim
!!      subroutine copy_filtered_field_from_trans                       &
!!     &         (m_folding, sph_rtp, trns_MHD, node, iphys, nod_fld)
!!
!!      subroutine copy_SGS_field_from_trans                            &
!!     &         (m_folding, sph_rtp, trns_SGS, node, iphys, nod_fld)
!!      subroutine copy_SGS_force_from_trans                            &
!!     &         (m_folding, sph_rtp, trns_SGS, node, iphys, nod_fld)
!!
!!      subroutine copy_SGS_diff_field_from_trans                       &
!!     &         (m_folding, sph_rtp, trns_SGS2, node, iphys, nod_fld)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(address_4_sph_trans), intent(in) :: trns_MHD
!!        type(address_4_sph_trans), intent(in) :: trns_SGS
!!        type(address_4_sph_trans), intent(in) :: trns_SGS2
!!        type(node_data), intent(in) :: node
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module copy_SGS_4_sph_trans
!
      use m_precision
      use m_machine_parameter
!
      use t_geometry_data
      use t_phys_address
      use t_phys_data
      use t_spheric_rtp_data
      use t_addresses_sph_transform
!
      use copy_snap_4_sph_trans
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
     &         (m_folding, sph_rtp, trns_MHD, node, iphys, nod_fld)
!
      integer(kind = kint), intent(in) :: m_folding
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
     &    m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_filter_vort, iphys%i_filter_vort,           &
     &    m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_filter_magne, iphys%i_filter_magne,         &
     &    m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_filter_current, iphys%i_filter_current,     &
     &    m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_wide_fil_velo, iphys%i_wide_fil_velo,       &
     &    m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_wide_fil_vort, iphys%i_wide_fil_vort,       &
     &    m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_wide_fil_magne, iphys%i_wide_fil_magne,     &
     &    m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_wide_fil_current, iphys%i_wide_fil_current, &
     &    m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
!  Copy scalars
      call copy_scalar_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_filter_temp, iphys%i_filter_temp,           &
     &    m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call copy_scalar_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_filter_comp, iphys%i_filter_comp,           &
     &    m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
!
      call copy_scalar_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_wide_fil_temp, iphys%i_wide_fil_temp,       &
     &    m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      call copy_scalar_from_snap_trans                                  &
     &   (trns_MHD%b_trns%i_wide_fil_comp, iphys%i_wide_fil_comp,       &
     &    m_folding, sph_rtp, trns_MHD, node, nod_fld)
!
      end subroutine copy_filtered_field_from_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_SGS_field_from_trans                              &
     &         (m_folding, sph_rtp, trns_SGS, node, iphys, nod_fld)
!
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(in) :: trns_SGS
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
!  Copy vectors
      call copy_vector_from_snap_trans                                  &
     &   (trns_SGS%b_trns%i_velo, iphys%i_velo,                         &
     &    m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
!  Copy scalars
      call copy_scalar_from_snap_trans                                  &
     &   (trns_SGS%b_trns%i_div_Coriolis, iphys%i_div_Coriolis,         &
     &    m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
      end subroutine copy_SGS_field_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_SGS_force_from_trans                              &
     &         (m_folding, sph_rtp, trns_SGS, node, iphys, nod_fld)
!
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(in) :: trns_SGS
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
      call copy_vector_from_snap_force                                  &
     &   (trns_SGS%f_trns%i_SGS_inertia, iphys%i_SGS_inertia,           &
     &    m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
      call copy_vector_from_snap_force                                  &
     &   (trns_SGS%f_trns%i_SGS_Lorentz, iphys%i_SGS_Lorentz,           &
     &    m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
      call copy_vector_from_snap_force                                  &
     &   (trns_SGS%f_trns%i_SGS_vp_induct, iphys%i_SGS_vp_induct,       &
     &    m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
      call copy_vector_from_snap_force                                  &
     &   (trns_SGS%f_trns%i_SGS_h_flux, iphys%i_SGS_h_flux,             &
     &    m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
      call copy_vector_from_snap_force                                  &
     &   (trns_SGS%f_trns%i_SGS_c_flux, iphys%i_SGS_c_flux,             &
     &    m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
!
      call copy_vector_from_snap_force                                  &
     &   (trns_SGS%f_trns%i_wide_SGS_inertia, iphys%i_wide_SGS_inertia, &
     &    m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
      call copy_vector_from_snap_force                                  &
     &   (trns_SGS%f_trns%i_wide_SGS_Lorentz, iphys%i_wide_SGS_Lorentz, &
     &    m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
      call copy_vector_from_snap_force                                  &
     &   (trns_SGS%f_trns%i_wide_SGS_vp_induct,                         &
     &    iphys%i_wide_SGS_vp_induct,                                   &
     &    m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
      call copy_vector_from_snap_force                                  &
     &   (trns_SGS%f_trns%i_wide_SGS_h_flux, iphys%i_wide_SGS_h_flux,   &
     &    m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
      call copy_vector_from_snap_force                                  &
     &   (trns_SGS%f_trns%i_wide_SGS_c_flux, iphys%i_wide_SGS_c_flux,   &
     &    m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
!
!      call copy_scalar_from_snap_force                                 &
!     &   (trns_SGS%f_trns%i_comp_scale, iphys%i_comp_scale,            &
!     &    m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
      end  subroutine copy_SGS_force_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_SGS_diff_field_from_trans                         &
     &         (m_folding, sph_rtp, trns_SGS2, node, iphys, nod_fld)
!
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(in) :: trns_SGS2
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_SGS2%b_trns%i_SGS_rot_inertia, iphys%i_SGS_rot_inertia,  &
     &    m_folding, sph_rtp, trns_SGS2, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_SGS2%b_trns%i_SGS_rot_Lorentz, iphys%i_SGS_rot_Lorentz,  &
     &    m_folding, sph_rtp, trns_SGS2, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_SGS2%b_trns%i_SGS_induction, iphys%i_SGS_induction,      &
     &    m_folding, sph_rtp, trns_SGS2, node, nod_fld)
!
!
      call copy_scalar_from_snap_trans                                  &
     &   (trns_SGS2%b_trns%i_SGS_div_inertia, iphys%i_SGS_div_inertia,  &
     &    m_folding, sph_rtp, trns_SGS2, node, nod_fld)
!
      call copy_scalar_from_snap_trans                                  &
     &   (trns_SGS2%b_trns%i_SGS_div_Lorentz, iphys%i_SGS_div_Lorentz,  &
     &    m_folding, sph_rtp, trns_SGS2, node, nod_fld)
!
      call copy_scalar_from_snap_trans                                  &
     &   (trns_SGS2%b_trns%i_SGS_div_h_flux, iphys%i_SGS_div_h_flux,    &
     &    m_folding, sph_rtp, trns_SGS2, node, nod_fld)
!
      call copy_scalar_from_snap_trans                                  &
     &   (trns_SGS2%b_trns%i_SGS_div_c_flux, iphys%i_SGS_div_c_flux,    &
     &    m_folding, sph_rtp, trns_SGS2, node, nod_fld)
!
      end  subroutine copy_SGS_diff_field_from_trans
!
!-----------------------------------------------------------------------
!
      end module copy_SGS_4_sph_trans

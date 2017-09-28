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
!!     &         (sph_params, sph_rtp, trns_MHD, node, iphys, nod_fld)
!!
!!      subroutine copy_SGS_field_from_trans                            &
!!     &         (sph_params, sph_rtp, trns_SGS, node, iphys, nod_fld)
!!      subroutine copy_wide_SGS_field_from_trans                       &
!!     &         (sph_params, sph_rtp, trns_SGS, node, iphys, nod_fld)
!!      subroutine copy_SGS_force_from_trans                            &
!!     &         (sph_params, sph_rtp, trns_SGS, node, iphys, nod_fld)
!!
!!      subroutine copy_SGS_diff_field_from_trans                       &
!!     &         (sph_params, sph_rtp, trns_snap, node, iphys, nod_fld)
!!      subroutine copy_SGS_snap_fld_from_trans                         &
!!     &         (sph_params, sph_rtp, trns_snap, node, iphys, nod_fld)
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
      module copy_SGS_4_sph_trans
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
     &         (sph_params, sph_rtp, trns_MHD, node, iphys, nod_fld)
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
!-----------------------------------------------------------------------
!
      subroutine copy_SGS_field_from_trans                              &
     &         (sph_params, sph_rtp, trns_SGS, node, iphys, nod_fld)
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
     &   (trns_SGS%b_trns%i_SGS_inertia, iphys%i_SGS_inertia,           &
     &    sph_params%m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_SGS%b_trns%i_SGS_Lorentz, iphys%i_SGS_Lorentz,           &
     &    sph_params%m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_SGS%b_trns%i_SGS_vp_induct, iphys%i_SGS_vp_induct,       &
     &    sph_params%m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_SGS%b_trns%i_SGS_h_flux, iphys%i_SGS_h_flux,             &
     &    sph_params%m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_SGS%b_trns%i_SGS_c_flux, iphys%i_SGS_c_flux,             &
     &    sph_params%m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
!
      end subroutine copy_SGS_field_from_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_wide_SGS_field_from_trans                         &
     &         (sph_params, sph_rtp, trns_SGS, node, iphys, nod_fld)
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
      subroutine copy_SGS_force_from_trans                              &
     &         (sph_params, sph_rtp, trns_SGS, node, iphys, nod_fld)
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(in) :: trns_SGS
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
      call copy_vector_from_snap_force                                  &
     &   (trns_SGS%f_trns%i_SGS_inertia, iphys%i_SGS_inertia,           &
     &    sph_params%m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
      call copy_vector_from_snap_force                                  &
     &   (trns_SGS%f_trns%i_SGS_Lorentz, iphys%i_SGS_Lorentz,           &
     &    sph_params%m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
      call copy_vector_from_snap_force                                  &
     &   (trns_SGS%f_trns%i_SGS_vp_induct, iphys%i_SGS_vp_induct,       &
     &    sph_params%m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
      call copy_vector_from_snap_force                                  &
     &   (trns_SGS%f_trns%i_SGS_h_flux, iphys%i_SGS_h_flux,             &
     &    sph_params%m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
      call copy_vector_from_snap_force                                  &
     &   (trns_SGS%f_trns%i_SGS_c_flux, iphys%i_SGS_c_flux,             &
     &    sph_params%m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
!
!      call copy_scalar_from_snap_force                                 &
!     &   (trns_SGS%f_trns%i_comp_scale, iphys%i_comp_scale,            &
!     &    sph_params%m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
      call copy_scalar_from_snap_force                                  &
     &   (trns_SGS%f_trns%i_Csim_SGS_buoyancy,                          &
     &    iphys%i_Csim_SGS_buoyancy,                                    &
     &    sph_params%m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
      call copy_scalar_from_snap_force                                  &
     &   (trns_SGS%f_trns%i_Csim_SGS_comp_buo,                          &
     &    iphys%i_Csim_SGS_comp_buo,                                    &
     &    sph_params%m_folding, sph_rtp, trns_SGS, node, nod_fld)
!
      end  subroutine copy_SGS_force_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_SGS_diff_field_from_trans                         &
     &         (sph_params, sph_rtp, trns_snap, node, iphys, nod_fld)
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(in) :: trns_snap
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_SGS_rot_inertia, iphys%i_SGS_rot_inertia,  &
     &    sph_params%m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_SGS_rot_Lorentz, iphys%i_SGS_rot_Lorentz,  &
     &    sph_params%m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_SGS_induction, iphys%i_SGS_induction,      &
     &    sph_params%m_folding, sph_rtp, trns_snap, node, nod_fld)
!
!
      call copy_scalar_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_SGS_div_inertia, iphys%i_SGS_div_inertia,  &
     &    sph_params%m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call copy_scalar_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_SGS_div_Lorentz, iphys%i_SGS_div_Lorentz,  &
     &    sph_params%m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call copy_scalar_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_SGS_div_h_flux, iphys%i_SGS_div_h_flux,    &
     &    sph_params%m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call copy_scalar_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_SGS_div_c_flux, iphys%i_SGS_div_c_flux,    &
     &    sph_params%m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      end  subroutine copy_SGS_diff_field_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_SGS_snap_fld_from_trans                           &
     &         (sph_params, sph_rtp, trns_snap, node, iphys, nod_fld)
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(in) :: trns_snap
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_SGS_inertia, iphys%i_SGS_inertia,          &
     &    sph_params%m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_SGS_Lorentz, iphys%i_SGS_Lorentz,          &
     &    sph_params%m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_SGS_vp_induct, iphys%i_SGS_vp_induct,      &
     &    sph_params%m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_SGS_h_flux, iphys%i_SGS_h_flux,            &
     &    sph_params%m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_SGS_c_flux, iphys%i_SGS_c_flux,            &
     &    sph_params%m_folding, sph_rtp, trns_snap, node, nod_fld)
!
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_h_flux_w_sgs, iphys%i_h_flux_w_sgs,        &
     &    sph_params%m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_c_flux_w_sgs, iphys%i_c_flux_w_sgs,        &
     &    sph_params%m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_inertia_w_sgs, iphys%i_inertia_w_sgs,      &
     &    sph_params%m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_Lorentz_w_sgs, iphys%i_Lorentz_w_sgs,      &
     &    sph_params%m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_vp_induct_w_sgs, iphys%i_vp_induct_w_sgs,  &
     &    sph_params%m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &  (trns_snap%b_trns%i_mag_induct_w_sgs, iphys%i_mag_induct_w_sgs, &
     &   sph_params%m_folding, sph_rtp, trns_snap, node, nod_fld)
!
!
      end  subroutine copy_SGS_snap_fld_from_trans
!
!-----------------------------------------------------------------------
!
      end module copy_SGS_4_sph_trans

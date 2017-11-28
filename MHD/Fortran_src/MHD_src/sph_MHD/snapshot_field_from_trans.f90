!>@file   snapshot_field_from_trans.f90
!!@brief  module snapshot_field_from_trans
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Copy data from/to sphrical transform buffer for snapshots
!!
!!@verbatim
!!      subroutine copy_snap_vec_fld_from_trans                         &
!!     &         (m_folding, sph_rtp, trns_snap, node, iphys, nod_fld)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(address_4_sph_trans), intent(in) :: trns_snap
!!        type(node_data), intent(in) :: node
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module snapshot_field_from_trans
!
      use m_precision
      use m_machine_parameter
      use m_phys_constants
!
      use t_geometry_data
      use t_phys_address
      use t_phys_data
      use t_spheric_rtp_data
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
      subroutine copy_snap_vec_fld_from_trans                           &
     &         (m_folding, sph_rtp, trns_snap, node, iphys, nod_fld)
!
      use copy_fields_from_sph_trans
!
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(address_4_sph_trans), intent(in) :: trns_snap
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
!  Copy vectors
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_velo, iphys%i_velo,                        &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_vort, iphys%i_vort,                        &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_magne, iphys%i_magne,                      &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_current, iphys%i_current,                  &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_v_diffuse, iphys%i_v_diffuse,              &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_w_diffuse, iphys%i_w_diffuse,              &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_vp_diffuse, iphys%i_vp_diffuse,            &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_b_diffuse, iphys%i_b_diffuse,              &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_rot_inertia, iphys%i_rot_inertia,          &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_rot_Coriolis, iphys%i_rot_Coriolis,        &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_rot_Lorentz, iphys%i_rot_Lorentz,          &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_rot_buoyancy, iphys%i_rot_buoyancy,        &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_rot_comp_buo, iphys%i_rot_comp_buo,        &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_press_grad, iphys%i_press_grad,            &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_induction, iphys%i_induction,              &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_grad_t, iphys%i_grad_t,                    &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_grad_composit, iphys%i_grad_composit,      &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_grad_vx, iphys%i_grad_vx,                  &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_grad_vy, iphys%i_grad_vy,                  &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_grad_vz, iphys%i_grad_vz,                  &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_buoyancy, iphys%i_buoyancy,                &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_comp_buo, iphys%i_comp_buo,                &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_geostrophic, iphys%i_geostrophic,          &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
!
!  Copy scalars
      call copy_scalar_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_temp, iphys%i_temp,                        &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_scalar_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_light, iphys%i_light,                      &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call copy_scalar_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_press, iphys%i_press,                      &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_scalar_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_par_temp, iphys%i_par_temp,                &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_scalar_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_filter_temp, iphys%i_filter_temp,          &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_scalar_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_t_diffuse, iphys%i_t_diffuse,              &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_scalar_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_c_diffuse, iphys%i_c_diffuse,              &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call copy_scalar_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_h_advect, iphys%i_h_advect,                &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_scalar_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_c_advect, iphys%i_c_advect,                &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call copy_scalar_from_snap_trans                                  &
     &   (trns_snap%b_trns%i_div_Coriolis, iphys%i_div_Coriolis,        &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      end subroutine copy_snap_vec_fld_from_trans
!
!-----------------------------------------------------------------------
!
      end module snapshot_field_from_trans

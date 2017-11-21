!>@file   snapshot_forces_from_trans.f90
!!@brief  module snapshot_forces_from_trans
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Copy data from/to sphrical transform buffer for snapshots
!!
!!@verbatim
!!      subroutine copy_snap_vec_force_from_trans                       &
!!     &         (m_folding, sph_rtp, trns_snap, node, iphys, nod_fld)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(address_4_sph_trans), intent(in) :: trns_snap
!!        type(node_data), intent(in) :: node
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!      subroutine zmean_snap_vec_force_from_trans(m_folding, sph_rtp,  &
!!     &          trns_snap, node, iphys, nod_fld)
!!      subroutine zrms_snap_vec_force_from_trans(m_folding, sph_rtp,   &
!!     &          trns_snap, node, iphys, nod_fld)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(node_data), intent(in) :: node
!!        type(phys_address), intent(in) :: iphys
!!        type(address_4_sph_trans), intent(inout) :: trns_snap
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module snapshot_forces_from_trans
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
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_snap_vec_force_from_trans                         &
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
      call copy_vector_from_snap_force                                  &
     &    (trns_snap%f_trns%i_coriolis, iphys%i_Coriolis,               &
     &     m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call copy_vector_from_snap_force                                  &
     &    (trns_snap%f_trns%i_electric, iphys%i_electric,               &
     &     m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_vector_from_snap_force                                  &
     &    (trns_snap%f_trns%i_poynting, iphys%i_poynting,               &
     &     m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call copy_vector_from_snap_force                                  &
     &    (trns_snap%f_trns%i_mag_stretch, iphys%i_mag_stretch,         &
     &     m_folding, sph_rtp, trns_snap, node, nod_fld)
!
!
      call copy_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_me_gen, iphys%i_me_gen,                    &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_ujb, iphys%i_ujb,                          &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_nega_ujb, iphys%i_nega_ujb,                &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_buo_gen, iphys%i_buo_gen,                  &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_c_buo_gen, iphys%i_c_buo_gen,              &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_f_buo_gen, iphys%i_f_buo_gen,              &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call copy_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_Csim_SGS_m_flux,                           &
     &    iphys%i_Csim_SGS_m_flux,                                      &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_Csim_SGS_Lorentz,                          &
     &    iphys%i_Csim_SGS_Lorentz,                                     &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_Csim_SGS_induction,                        &
     &    iphys%i_Csim_SGS_induction,                                   &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_Csim_SGS_h_flux, iphys%i_Csim_SGS_h_flux,  &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_Csim_SGS_c_flux, iphys%i_Csim_SGS_c_flux,  &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_Csim_SGS_buoyancy,                         &
     &    iphys%i_Csim_SGS_buoyancy,                                    &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_Csim_SGS_comp_buo,                         &
     &    iphys%i_Csim_SGS_comp_buo,                                    &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call copy_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_reynolds_wk, iphys%i_reynolds_wk,          &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_SGS_Lor_wk, iphys%i_SGS_Lor_wk,            &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_SGS_buo_wk, iphys%i_SGS_buo_wk,            &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_SGS_comp_buo_wk, iphys%i_SGS_comp_buo_wk,  &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_SGS_me_gen, iphys%i_SGS_me_gen,            &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
!
!
      call copy_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_velo_scale, iphys%i_velo_scale,            &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_magne_scale, iphys%i_magne_scale,          &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_temp_scale, iphys%i_temp_scale,            &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call copy_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_comp_scale, iphys%i_comp_scale,            &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      end  subroutine copy_snap_vec_force_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine zmean_snap_vec_force_from_trans(m_folding, sph_rtp,    &
     &          trns_snap, node, iphys, nod_fld)
!
      use copy_fields_from_sph_trans
!
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(inout) :: trns_snap
      type(phys_data), intent(inout) :: nod_fld
!
!
      call zmean_vector_from_snap_force                                 &
     &    (trns_snap%f_trns%i_coriolis, iphys%i_Coriolis,               &
     &     m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call zmean_vector_from_snap_force                                 &
     &    (trns_snap%f_trns%i_electric, iphys%i_electric,               &
     &     m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zmean_vector_from_snap_force                                 &
     &    (trns_snap%f_trns%i_poynting, iphys%i_poynting,               &
     &     m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call zmean_vector_from_snap_force                                 &
     &    (trns_snap%f_trns%i_mag_stretch, iphys%i_mag_stretch,         &
     &     m_folding, sph_rtp, trns_snap, node, nod_fld)
!
!
      call zmean_scalar_from_snap_force                                 &
     &   (trns_snap%f_trns%i_me_gen, iphys%i_me_gen,                    &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zmean_scalar_from_snap_force                                 &
     &   (trns_snap%f_trns%i_ujb, iphys%i_ujb,                          &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zmean_scalar_from_snap_force                                 &
     &   (trns_snap%f_trns%i_nega_ujb, iphys%i_nega_ujb,                &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zmean_scalar_from_snap_force                                 &
     &   (trns_snap%f_trns%i_buo_gen, iphys%i_buo_gen,                  &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zmean_scalar_from_snap_force                                 &
     &   (trns_snap%f_trns%i_c_buo_gen, iphys%i_c_buo_gen,              &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zmean_scalar_from_snap_force                                 &
     &   (trns_snap%f_trns%i_f_buo_gen, iphys%i_f_buo_gen,              &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call zmean_scalar_from_snap_force                                 &
     &   (trns_snap%f_trns%i_Csim_SGS_m_flux,                           &
     &    iphys%i_Csim_SGS_m_flux,                                      &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zmean_scalar_from_snap_force                                 &
     &   (trns_snap%f_trns%i_Csim_SGS_Lorentz,                          &
     &    iphys%i_Csim_SGS_Lorentz,                                     &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zmean_scalar_from_snap_force                                 &
     &   (trns_snap%f_trns%i_Csim_SGS_induction,                        &
     &    iphys%i_Csim_SGS_induction,                                   &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zmean_scalar_from_snap_force                                 &
     &   (trns_snap%f_trns%i_Csim_SGS_h_flux, iphys%i_Csim_SGS_h_flux,  &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zmean_scalar_from_snap_force                                 &
     &   (trns_snap%f_trns%i_Csim_SGS_c_flux, iphys%i_Csim_SGS_c_flux,  &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zmean_scalar_from_snap_force                                 &
     &   (trns_snap%f_trns%i_Csim_SGS_buoyancy,                         &
     &    iphys%i_Csim_SGS_buoyancy,                                    &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zmean_scalar_from_snap_force                                 &
     &   (trns_snap%f_trns%i_Csim_SGS_comp_buo,                         &
     &    iphys%i_Csim_SGS_comp_buo,                                    &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call zmean_scalar_from_snap_force                                 &
     &   (trns_snap%f_trns%i_reynolds_wk, iphys%i_reynolds_wk,          &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zmean_scalar_from_snap_force                                 &
     &   (trns_snap%f_trns%i_SGS_Lor_wk, iphys%i_SGS_Lor_wk,            &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zmean_scalar_from_snap_force                                 &
     &   (trns_snap%f_trns%i_SGS_buo_wk, iphys%i_SGS_buo_wk,            &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zmean_scalar_from_snap_force                                 &
     &   (trns_snap%f_trns%i_SGS_comp_buo_wk, iphys%i_SGS_comp_buo_wk,  &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zmean_scalar_from_snap_force                                 &
     &   (trns_snap%f_trns%i_SGS_me_gen, iphys%i_SGS_me_gen,            &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
!
!
      call zmean_scalar_from_snap_force                                 &
     &   (trns_snap%f_trns%i_velo_scale, iphys%i_velo_scale,            &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zmean_scalar_from_snap_force                                 &
     &   (trns_snap%f_trns%i_magne_scale, iphys%i_magne_scale,          &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zmean_scalar_from_snap_force                                 &
     &   (trns_snap%f_trns%i_temp_scale, iphys%i_temp_scale,            &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zmean_scalar_from_snap_force                                 &
     &   (trns_snap%f_trns%i_comp_scale, iphys%i_comp_scale,            &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      end  subroutine zmean_snap_vec_force_from_trans
!
!-----------------------------------------------------------------------
!
      subroutine zrms_snap_vec_force_from_trans(m_folding, sph_rtp,     &
     &          trns_snap, node, iphys, nod_fld)
!
      use copy_fields_from_sph_trans
!
      integer(kind = kint), intent(in) :: m_folding
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(address_4_sph_trans), intent(inout) :: trns_snap
      type(phys_data), intent(inout) :: nod_fld
!
!
      call zrms_vector_from_snap_force                                  &
     &    (trns_snap%f_trns%i_coriolis, iphys%i_Coriolis,               &
     &     m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call zrms_vector_from_snap_force                                  &
     &    (trns_snap%f_trns%i_electric, iphys%i_electric,               &
     &     m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zrms_vector_from_snap_force                                  &
     &    (trns_snap%f_trns%i_poynting, iphys%i_poynting,               &
     &     m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call zrms_vector_from_snap_force                                  &
     &    (trns_snap%f_trns%i_mag_stretch, iphys%i_mag_stretch,         &
     &     m_folding, sph_rtp, trns_snap, node, nod_fld)
!
!
      call zrms_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_me_gen, iphys%i_me_gen,                    &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zrms_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_ujb, iphys%i_ujb,                          &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zrms_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_nega_ujb, iphys%i_nega_ujb,                &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zrms_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_buo_gen, iphys%i_buo_gen,                  &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zrms_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_c_buo_gen, iphys%i_c_buo_gen,              &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zrms_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_f_buo_gen, iphys%i_f_buo_gen,              &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call zrms_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_Csim_SGS_m_flux,                           &
     &    iphys%i_Csim_SGS_m_flux,                                      &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zrms_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_Csim_SGS_Lorentz,                          &
     &    iphys%i_Csim_SGS_Lorentz,                                     &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zrms_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_Csim_SGS_induction,                        &
     &    iphys%i_Csim_SGS_induction,                                   &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zrms_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_Csim_SGS_h_flux, iphys%i_Csim_SGS_h_flux,  &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zrms_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_Csim_SGS_c_flux, iphys%i_Csim_SGS_c_flux,  &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zrms_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_Csim_SGS_buoyancy,                         &
     &    iphys%i_Csim_SGS_buoyancy,                                    &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zrms_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_Csim_SGS_comp_buo,                         &
     &    iphys%i_Csim_SGS_comp_buo,                                    &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      call zrms_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_reynolds_wk, iphys%i_reynolds_wk,          &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zrms_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_SGS_Lor_wk, iphys%i_SGS_Lor_wk,            &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zrms_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_SGS_buo_wk, iphys%i_SGS_buo_wk,            &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zrms_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_SGS_comp_buo_wk, iphys%i_SGS_comp_buo_wk,  &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zrms_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_SGS_me_gen, iphys%i_SGS_me_gen,            &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
!
!
      call zrms_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_velo_scale, iphys%i_velo_scale,            &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zrms_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_magne_scale, iphys%i_magne_scale,          &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zrms_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_temp_scale, iphys%i_temp_scale,            &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
      call zrms_scalar_from_snap_force                                  &
     &   (trns_snap%f_trns%i_comp_scale, iphys%i_comp_scale,            &
     &    m_folding, sph_rtp, trns_snap, node, nod_fld)
!
      end  subroutine zrms_snap_vec_force_from_trans
!
!-----------------------------------------------------------------------
!
      end module snapshot_forces_from_trans

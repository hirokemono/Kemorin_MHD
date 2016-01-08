!>@file   copy_snap_4_sph_trans.f90
!!@brief  module copy_snap_4_sph_trans
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2009
!
!>@brief  Copy data from/to sphrical transform buffer for snapshots
!!
!!@verbatim
!!      subroutine copy_snap_vec_fld_from_trans(node, iphys, nod_fld)
!!      subroutine copy_snap_vec_fld_to_trans(node, iphys, nod_fld)
!!        type(node_data), intent(in) :: node
!!        type(phys_address), intent(in) :: iphys
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!
      module copy_snap_4_sph_trans
!
      use m_precision
      use m_machine_parameter
!
      use t_geometry_data
      use t_phys_address
      use t_phys_data
!
      implicit  none
!
      private :: copy_scalar_from_snap_trans
      private :: copy_vector_from_snap_trans
      private :: copy_scalar_from_snap_force
      private :: copy_vector_from_snap_force
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_snap_vec_fld_from_trans(node, iphys, nod_fld)
!
      use m_addresses_trans_sph_snap
!
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
!  Copy vectors
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_velo, iphys%i_velo, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_vort, iphys%i_vort, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_magne, iphys%i_magne, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_current, iphys%i_current, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_v_diffuse, iphys%i_v_diffuse, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_w_diffuse, iphys%i_w_diffuse, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_vp_diffuse, iphys%i_vp_diffuse, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_b_diffuse, iphys%i_b_diffuse, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_rot_inertia, iphys%i_rot_inertia, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_rot_Coriolis, iphys%i_rot_Coriolis, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_rot_Lorentz, iphys%i_rot_Lorentz, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_rot_buoyancy, iphys%i_rot_buoyancy, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_rot_comp_buo, iphys%i_rot_comp_buo, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_press_grad, iphys%i_press_grad, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_induction, iphys%i_induction, node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_grad_t, iphys%i_grad_t, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &   (bs_trns%i_grad_composit, iphys%i_grad_composit,               &
     &    node, nod_fld)
!
      call copy_vector_from_snap_trans                                  &
     &    (bs_trns%i_grad_vx, iphys%i_grad_vx, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &    (bs_trns%i_grad_vy, iphys%i_grad_vy, node, nod_fld)
      call copy_vector_from_snap_trans                                  &
     &    (bs_trns%i_grad_vz, iphys%i_grad_vz, node, nod_fld)
!
!  Copy scalars
      call copy_scalar_from_snap_trans                                  &
     &   (bs_trns%i_temp, iphys%i_temp, node, nod_fld)
      call copy_scalar_from_snap_trans                                  &
     &   (bs_trns%i_light, iphys%i_light, node, nod_fld)
!
      call copy_scalar_from_snap_trans                                  &
     &   (bs_trns%i_press, iphys%i_press, node, nod_fld)
      call copy_scalar_from_snap_trans                                  &
     &   (bs_trns%i_par_temp, iphys%i_par_temp, node, nod_fld)
      call copy_scalar_from_snap_trans                                  &
     &   (bs_trns%i_filter_temp, iphys%i_filter_temp, node, nod_fld)
      call copy_scalar_from_snap_trans                                  &
     &   (bs_trns%i_t_diffuse, iphys%i_t_diffuse, node, nod_fld)
      call copy_scalar_from_snap_trans                                  &
     &   (bs_trns%i_c_diffuse, iphys%i_c_diffuse, node, nod_fld)
!
      call copy_scalar_from_snap_trans                                  &
     &   (bs_trns%i_div_Coriolis, iphys%i_div_Coriolis, node, nod_fld)
!
      end subroutine copy_snap_vec_fld_from_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_snap_vec_fld_to_trans(node, iphys, nod_fld)
!
      use m_addresses_trans_sph_snap
!
      type(node_data), intent(in) :: node
      type(phys_address), intent(in) :: iphys
      type(phys_data), intent(inout) :: nod_fld
!
!
      call copy_vector_from_snap_force                                  &
     &    (fs_trns%i_coriolis, iphys%i_Coriolis, node, nod_fld)
!
      call copy_vector_from_snap_force                                  &
     &    (fs_trns%i_electric, iphys%i_electric, node, nod_fld)
      call copy_vector_from_snap_force                                  &
     &    (fs_trns%i_poynting, iphys%i_poynting, node, nod_fld)
!
      call copy_vector_from_snap_force                                  &
     &    (fs_trns%i_mag_stretch, iphys%i_mag_stretch, node, nod_fld)
!
!
      call copy_scalar_from_snap_force                                  &
     &   (fs_trns%i_me_gen, iphys%i_me_gen, node, nod_fld)
      call copy_scalar_from_snap_force                                  &
     &   (fs_trns%i_ujb, iphys%i_ujb, node, nod_fld)
      call copy_scalar_from_snap_force                                  &
     &   (fs_trns%i_nega_ujb, iphys%i_nega_ujb, node, nod_fld)
      call copy_scalar_from_snap_force                                  &
     &   (fs_trns%i_buo_gen, iphys%i_buo_gen, node, nod_fld)
      call copy_scalar_from_snap_force                                  &
     &   (fs_trns%i_c_buo_gen, iphys%i_c_buo_gen, node, nod_fld)
      call copy_scalar_from_snap_force                                  &
     &   (fs_trns%i_f_buo_gen, iphys%i_f_buo_gen, node, nod_fld)
!
      call copy_scalar_from_snap_force                                  &
     &   (fs_trns%i_velo_scale, iphys%i_velo_scale, node, nod_fld)
      call copy_scalar_from_snap_force                                  &
     &   (fs_trns%i_magne_scale, iphys%i_magne_scale, node, nod_fld)
      call copy_scalar_from_snap_force                                  &
     &   (fs_trns%i_temp_scale, iphys%i_temp_scale, node, nod_fld)
      call copy_scalar_from_snap_force                                  &
     &   (fs_trns%i_comp_scale, iphys%i_comp_scale, node, nod_fld)
!
      end  subroutine copy_snap_vec_fld_to_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_scalar_from_snap_trans                            &
     &         (i_trns, i_field, node, nod_fld)
!
      use m_addresses_trans_sph_snap
      use copy_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_nodal_scalar_from_trans                                 &
     &   (ncomp_snap_rj_2_rtp, i_trns, fls_rtp,                         &
     &    node%numnod, nod_fld%ntot_phys, i_field, nod_fld%d_fld)
!
      end subroutine copy_scalar_from_snap_trans
!
!-----------------------------------------------------------------------
!
      subroutine copy_vector_from_snap_trans                            &
     &         (i_trns, i_field, node, nod_fld)
!
      use m_addresses_trans_sph_snap
      use copy_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_nodal_vector_from_trans                                 &
     &    (ncomp_snap_rj_2_rtp, i_trns, fls_rtp,                        &
     &     node%numnod, nod_fld%ntot_phys, i_field, nod_fld%d_fld)
!
      end subroutine copy_vector_from_snap_trans
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_scalar_from_snap_force                            &
     &         (i_trns, i_field, node, nod_fld)
!
      use m_addresses_trans_sph_snap
      use copy_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_nodal_scalar_from_trans                                 &
     &   (ncomp_snap_rtp_2_rj, i_trns, frs_rtp,                         &
     &    node%numnod, nod_fld%ntot_phys, i_field, nod_fld%d_fld)
!
      end subroutine copy_scalar_from_snap_force
!
!-----------------------------------------------------------------------
!
      subroutine copy_vector_from_snap_force                            &
     &         (i_trns, i_field, node, nod_fld)
!
      use m_addresses_trans_sph_snap
      use copy_field_4_sph_trans
!
      integer(kind = kint), intent(in) :: i_field, i_trns
      type(node_data), intent(in) :: node
      type(phys_data), intent(inout) :: nod_fld
!
!
      if( (i_field*i_trns) .le. 0) return
      call copy_nodal_vector_from_trans                                 &
     &   (ncomp_snap_rtp_2_rj, i_trns, frs_rtp,                         &
     &    node%numnod, nod_fld%ntot_phys, i_field, nod_fld%d_fld)
!
      end subroutine copy_vector_from_snap_force
!
!-----------------------------------------------------------------------
!
      end module copy_snap_4_sph_trans

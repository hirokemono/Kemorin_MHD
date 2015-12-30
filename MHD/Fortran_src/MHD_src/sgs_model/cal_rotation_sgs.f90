!
!     module cal_rotation_sgs
!
!     Written by H. Matsui
!
!!      subroutine choose_cal_rotation_sgs(iflag_commute, iflag_4_supg, &
!!     &          iele_fsmp_stack, m_lump, node, ele, surf, sf_grp,     &
!!     &          nod_bc, sgs_sf, iak_diff, i_vector, i_rot)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(vect_fixed_nod_bc_type), intent(in) :: nod_bc
!!        type(scaler_surf_bc_data_type),  intent(in) :: sgs_sf(3)
!
      module cal_rotation_sgs
!
      use m_precision
!
      use m_phys_constants
      use m_nod_comm_table
      use m_node_phys_data
      use m_jacobians
      use m_element_id_4_node
      use m_finite_element_matrix
      use m_int_vol_data
      use m_filter_elength
      use m_SGS_model_coefs
!
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_nodal_bc_data
      use t_surface_bc_data
!
      use cal_ff_smp_to_ffs
      use cal_for_ffs
      use nod_phys_send_recv
!
      private :: choose_int_vol_rot_sgs
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine choose_cal_rotation_sgs(iflag_commute, iflag_4_supg,   &
     &          iele_fsmp_stack, m_lump, node, ele, surf, sf_grp,       &
     &          nod_bc, sgs_sf, iak_diff, i_vector, i_rot)
!
      use m_control_parameter
!
      use cal_rotation
      use set_boundary_scalars
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(vect_fixed_nod_bc_type), intent(in) :: nod_bc
      type(scaler_surf_bc_data_type),  intent(in) :: sgs_sf(3)
!
      integer(kind = kint), intent(in) :: iflag_4_supg, iflag_commute
      integer(kind = kint), intent(in) :: iak_diff, i_vector, i_rot
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
!
      if(iflag_SGS_model .ne. id_SGS_none                               &
     &     .and. iflag_commute .eq. id_SGS_commute_ON) then
        call choose_int_vol_rot_sgs(iflag_4_supg, iele_fsmp_stack,      &
     &      node, ele, surf, sf_grp, sgs_sf, iak_diff, i_vector)
      else
        call choose_int_vol_rotations(iflag_4_supg, iele_fsmp_stack,    &
     &      node, ele, i_vector)
      end if
!
      call cal_ff_smp_2_vector(node, rhs_tbl1, f1_nl%ff_smp,            &
     &    m_lump%ml, nod_fld1%ntot_phys, i_rot, nod_fld1%d_fld)
!
      call set_boundary_vect(nod_bc, i_rot, nod_fld1)
!
! ----------   communications
      call vector_send_recv(i_rot, node, nod_comm, nod_fld1)
      nod_fld1%iflag_update(i_rot:i_rot+2) = 1
!
      end subroutine choose_cal_rotation_sgs
!
!-----------------------------------------------------------------------
!
      subroutine choose_int_vol_rot_sgs(iflag_4_supg, iele_fsmp_stack,  &
     &          node, ele, surf, sf_grp, sgs_sf, iak_diff, i_vector)
!
      use m_control_parameter
      use m_node_phys_data
      use m_element_phys_data
      use m_element_id_4_node
      use m_jacobian_sf_grp
      use m_finite_element_matrix
      use m_filter_elength
!
      use int_sgs_vect_differences
      use int_sgs_vect_diff_upw
      use int_surf_rot_sgs
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(scaler_surf_bc_data_type),  intent(in) :: sgs_sf(3)
!
      integer(kind = kint), intent(in) :: iflag_4_supg
      integer(kind = kint), intent(in) :: iak_diff, i_vector
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
!
      call reset_ff_smp(node%max_nod_smp, f1_nl)
!
      if ( iflag_4_supg .eq. id_magnetic_SUPG) then
        call int_sgs_rotation_upw                                       &
     &     (node, ele, jac1_3d_q, rhs_tbl1, nod_fld1, FEM1_elen,        &
     &      iele_fsmp_stack, intg_point_t_evo, ifilter_final,           &
     &      ak_diff(1,iak_diff), i_vector, fld_ele1%ntot_phys,          &
     &      iphys_ele%i_magne, fld_ele1%d_fld, fem1_wk, f1_nl)
      else if ( iflag_4_supg .eq. id_turn_ON) then
        call int_sgs_rotation_upw                                       &
     &     (node, ele, jac1_3d_q, rhs_tbl1, nod_fld1, FEM1_elen,        &
     &      iele_fsmp_stack, intg_point_t_evo, ifilter_final,           &
     &      ak_diff(1,iak_diff), i_vector, fld_ele1%ntot_phys,          &
     &      iphys_ele%i_velo, fld_ele1%d_fld, fem1_wk, f1_nl)
      else
        call int_sgs_rotation                                           &
     &     (node, ele, jac1_3d_q, rhs_tbl1, nod_fld1, FEM1_elen,        &
     &      iele_fsmp_stack, intg_point_t_evo, ifilter_final,           &
     &      ak_diff(1,iak_diff), i_vector, fem1_wk, f1_nl)
      end if
!
      call int_surf_rotation_sgs(node, ele, surf, sf_grp,               &
     &    nod_fld1, jac1_sf_grp_2d_q, rhs_tbl1, FEM1_elen, sgs_sf,      &
     &    intg_point_t_evo, ifilter_final, iak_diff, i_vector,          &
     &    fem1_wk, f1_nl)
!
      end subroutine choose_int_vol_rot_sgs
!
!-----------------------------------------------------------------------
!
      end module cal_rotation_sgs

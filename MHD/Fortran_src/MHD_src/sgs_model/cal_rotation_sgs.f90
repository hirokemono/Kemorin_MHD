!
!     module cal_rotation_sgs
!
!     Written by H. Matsui
!
!!      subroutine choose_cal_rotation_sgs(iflag_commute, iflag_4_supg, &
!!     &          iak_diff, i_vector, i_rot, iele_fsmp_stack, m_lump,   &
!!     &          nod_comm, node, ele, surf, sf_grp, iphys_ele, ele_fld,&
!!     &          jac_3d, jac_sf_grp, FEM_elens, diff_coefs, nod_bc,    &
!!     &          sgs_sf, rhs_tbl, fem_wk, surf_wk, f_nl, nod_fld)
!!        type(communication_table), intent(in) :: nod_comm
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(surface_data), intent(in) :: surf
!!        type(surface_group_data), intent(in) :: sf_grp
!!        type(lumped_mass_matrices), intent(in) :: m_lump
!!        type(vect_fixed_nod_bc_type), intent(in) :: nod_bc
!!        type(phys_address), intent(in) :: iphys_ele
!!        type(phys_data), intent(in) :: ele_fld
!!        type(jacobians_3d), intent(in) :: jac_3d
!!        type(jacobians_2d), intent(in) :: jac_sf_grp
!!        type(gradient_model_data_type), intent(in) :: FEM_elens
!!        type(MHD_coefficients_type), intent(in) :: diff_coefs
!!        type(scaler_surf_bc_data_type), intent(in) :: sgs_sf(3)
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(work_finite_element_mat), intent(inout) :: fem_wk
!!        type(work_surface_element_mat), intent(inout) :: surf_wk
!!        type(finite_ele_mat_node), intent(inout) :: f_nl
!!        type(phys_data), intent(inout) :: nod_fld
!
      module cal_rotation_sgs
!
      use m_precision
!
      use m_machine_parameter
      use m_control_parameter
      use m_phys_constants
!
      use t_comm_table
      use t_geometry_data
      use t_surface_data
      use t_group_data
      use t_nodal_bc_data
      use t_surface_bc_data
      use t_phys_address
      use t_phys_data
      use t_jacobian_3d
      use t_jacobian_2d
      use t_table_FEM_const
      use t_finite_element_mat
      use t_int_surface_data
      use t_MHD_finite_element_mat
      use t_filter_elength
      use t_material_property
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
     &          iak_diff, i_vector, i_rot, iele_fsmp_stack, m_lump,     &
     &          nod_comm, node, ele, surf, sf_grp, iphys_ele, ele_fld,  &
     &          jac_3d, jac_sf_grp, FEM_elens, diff_coefs, nod_bc,      &
     &          sgs_sf, rhs_tbl, fem_wk, surf_wk, f_nl, nod_fld)
!
      use cal_rotation
      use set_boundary_scalars
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(lumped_mass_matrices), intent(in) :: m_lump
      type(vect_fixed_nod_bc_type), intent(in) :: nod_bc
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(MHD_coefficients_type), intent(in) :: diff_coefs
      type(scaler_surf_bc_data_type), intent(in) :: sgs_sf(3)
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: iflag_4_supg, iflag_commute
      integer(kind = kint), intent(in) :: iak_diff, i_vector, i_rot
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(phys_data), intent(inout) :: nod_fld
!
!
      if(iflag_SGS_model .ne. id_SGS_none                               &
     &     .and. iflag_commute .eq. id_SGS_commute_ON) then
        call choose_int_vol_rot_sgs                                     &
     &     (iflag_4_supg, iele_fsmp_stack, iak_diff, i_vector,          &
     &      node, ele, surf, sf_grp, nod_fld, iphys_ele, ele_fld,       &
     &      jac_3d, jac_sf_grp, FEM_elens, diff_coefs, sgs_sf, rhs_tbl, &
     &      fem_wk, surf_wk, f_nl)
      else
        call choose_int_vol_rotations(iflag_4_supg, iele_fsmp_stack,    &
     &      i_vector, node, ele, nod_fld, iphys_ele, ele_fld,           &
     &      jac_3d, rhs_tbl, fem_wk, f_nl)
      end if
!
      call cal_ff_smp_2_vector(node, rhs_tbl, f_nl%ff_smp,              &
     &    m_lump%ml, nod_fld%ntot_phys, i_rot, nod_fld%d_fld)
!
      call set_boundary_vect(nod_bc, i_rot, nod_fld)
!
! ----------   communications
      call vector_send_recv(i_rot, nod_comm, nod_fld)
      nod_fld%iflag_update(i_rot:i_rot+2) = 1
!
      end subroutine choose_cal_rotation_sgs
!
!-----------------------------------------------------------------------
!
      subroutine choose_int_vol_rot_sgs                                 &
     &         (iflag_4_supg, iele_fsmp_stack, iak_diff, i_vector,      &
     &          node, ele, surf, sf_grp, nod_fld, iphys_ele, ele_fld,   &
     &          jac_3d, jac_sf_grp, FEM_elens, diff_coefs,              &
     &          sgs_sf, rhs_tbl, fem_wk, surf_wk, f_nl)
!
      use int_sgs_vect_differences
      use int_sgs_vect_diff_upw
      use int_surf_rot_sgs
!
      type(node_data), intent(in) :: node
      type(element_data), intent(in) :: ele
      type(surface_data), intent(in) :: surf
      type(surface_group_data), intent(in) :: sf_grp
      type(phys_data), intent(in) :: nod_fld
      type(phys_address), intent(in) :: iphys_ele
      type(phys_data), intent(in) :: ele_fld
      type(jacobians_3d), intent(in) :: jac_3d
      type(jacobians_2d), intent(in) :: jac_sf_grp
      type(gradient_model_data_type), intent(in) :: FEM_elens
      type(MHD_coefficients_type), intent(in) :: diff_coefs
      type(scaler_surf_bc_data_type), intent(in) :: sgs_sf(3)
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!
      integer(kind = kint), intent(in) :: iflag_4_supg
      integer(kind = kint), intent(in) :: iak_diff, i_vector
      integer(kind = kint), intent(in) :: iele_fsmp_stack(0:np_smp)
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(work_surface_element_mat), intent(inout) :: surf_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
!
      call reset_ff_smp(node%max_nod_smp, f_nl)
!
      if ( iflag_4_supg .eq. id_magnetic_SUPG) then
        call int_sgs_rotation_upw                                       &
     &     (node, ele, jac_3d, rhs_tbl, nod_fld, FEM_elens,             &
     &      iele_fsmp_stack, intg_point_t_evo, ifilter_final,           &
     &      diff_coefs%num_field, iak_diff, diff_coefs%ak, i_vector,    &
     &      ele_fld%ntot_phys, iphys_ele%i_magne, ele_fld%d_fld,        &
     &      fem_wk, f_nl)
      else if ( iflag_4_supg .eq. id_turn_ON) then
        call int_sgs_rotation_upw                                       &
     &     (node, ele, jac_3d, rhs_tbl, nod_fld, FEM_elens,             &
     &      iele_fsmp_stack, intg_point_t_evo, ifilter_final,           &
     &      diff_coefs%num_field, iak_diff, diff_coefs%ak, i_vector,    &
     &      ele_fld%ntot_phys, iphys_ele%i_velo, ele_fld%d_fld,         &
     &      fem_wk, f_nl)
      else
        call int_sgs_rotation                                           &
     &     (node, ele, jac_3d, rhs_tbl, nod_fld, FEM_elens,             &
     &      iele_fsmp_stack, intg_point_t_evo, ifilter_final,           &
     &      diff_coefs%num_field, iak_diff, diff_coefs%ak, i_vector,    &
     &      fem_wk, f_nl)
      end if
!
      call int_surf_rotation_sgs(node, ele, surf, sf_grp,               &
     &    nod_fld, jac_sf_grp, rhs_tbl, FEM_elens, sgs_sf,              &
     &    intg_point_t_evo, ifilter_final,                              &
     &    diff_coefs%num_field, iak_diff, diff_coefs%ak,                &
     &    i_vector, fem_wk, surf_wk, f_nl)
!
      end subroutine choose_int_vol_rot_sgs
!
!-----------------------------------------------------------------------
!
      end module cal_rotation_sgs

!cal_deltax_and_prods_4_nod.f90
!     module cal_deltax_and_prods_4_nod
!
!     Written by H. Matsui on Nov., 2006
!
!      subroutine cal_dx2_on_node(nod_comm, node, ele,                  &
!     &          jac_3d, rhs_tbl, tbl_crs, mass, m_lump, itype_mass,    &
!     &          FEM_elen, fem_wk, f_l)
!      subroutine cal_dxi_dxes_node(nod_comm, node, ele,                &
!     &          jac_3d, rhs_tbl, tbl_crs, mass, m_lump, itype_mass,    &
!     &          dxidxs, fem_wk, f_l)
!        type(communication_table), intent(in) :: nod_comm
!        type(node_data),    intent(in) :: node
!        type(element_data), intent(in) :: ele
!        type(jacobians_3d), intent(in) :: jac_3d
!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!        type(CRS_matrix_connect), intent(in) :: tbl_crs
!        type(CRS_matrix), intent(in) :: mass
!        type(lumped_mass_matrices), intent(in) :: m_lump
!        type(gradient_model_data_type), intent(inout) :: FEM_elen
!        type(work_finite_element_mat), intent(inout) :: fem_wk
!        type(finite_ele_mat_node), intent(inout) :: f_l
!
      module cal_deltax_and_prods_4_nod
!
      use m_precision
!
      use m_constants
      use m_phys_constants
!
      use t_geometry_data
      use t_jacobians
      use t_comm_table
      use t_table_FEM_const
      use t_finite_element_mat
      use t_crs_matrix
!
      use int_vol_elesize_on_node
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine cal_dx2_on_node(nod_comm, node, ele,                   &
     &          jac_3d, rhs_tbl, tbl_crs, mass, m_lump, itype_mass,     &
     &          FEM_elen, fem_wk, f_l)
!
      use t_filter_elength
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(CRS_matrix_connect), intent(in) :: tbl_crs
      type(CRS_matrix), intent(in) :: mass
      type(lumped_mass_matrices), intent(in) :: m_lump
!
      integer(kind = kint), intent(in) :: itype_mass
!
      type(gradient_model_data_type), intent(inout) :: FEM_elen
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
!
!
      call int_dx_ele2_node(nod_comm, node, ele,                        &
     &   jac_3d, rhs_tbl, tbl_crs, mass, m_lump, itype_mass,            &
     &   FEM_elen%elen_ele%moms%f_x2, FEM_elen%elen_nod%moms%f_x2,      &
     &   fem_wk, f_l)
      call int_dx_ele2_node(nod_comm, node, ele,                        &
     &   jac_3d, rhs_tbl, tbl_crs, mass, m_lump, itype_mass,            &
     &   FEM_elen%elen_ele%moms%f_y2, FEM_elen%elen_nod%moms%f_y2,      &
     &   fem_wk, f_l)
      call int_dx_ele2_node(nod_comm, node, ele,                        &
     &   jac_3d, rhs_tbl, tbl_crs, mass, m_lump, itype_mass,            &
     &   FEM_elen%elen_ele%moms%f_z2, FEM_elen%elen_nod%moms%f_z2,      &
     &   fem_wk, f_l)
!
      call int_dx_ele2_node(nod_comm, node, ele,                        &
     &   jac_3d, rhs_tbl, tbl_crs, mass, m_lump, itype_mass,            &
     &   FEM_elen%elen_ele%moms%f_xy, FEM_elen%elen_nod%moms%f_xy,      &
     &   fem_wk, f_l)
      call int_dx_ele2_node(nod_comm, node, ele,                        &
     &   jac_3d, rhs_tbl, tbl_crs, mass, m_lump, itype_mass,            &
     &   FEM_elen%elen_ele%moms%f_yz, FEM_elen%elen_nod%moms%f_yz,      &
     &   fem_wk, f_l)
      call int_dx_ele2_node(nod_comm, node, ele,                        &
     &   jac_3d, rhs_tbl, tbl_crs, mass, m_lump, itype_mass,            &
     &   FEM_elen%elen_ele%moms%f_zx, FEM_elen%elen_nod%moms%f_zx,      &
     &   fem_wk, f_l)
!
      end subroutine cal_dx2_on_node
!
! -----------------------------------------------------------------------
!
      subroutine cal_dxi_dxes_node(nod_comm, node, ele,                 &
     &          jac_3d, rhs_tbl, tbl_crs, mass, m_lump, itype_mass,     &
     &          dxidxs, fem_wk, f_l)
!
      use t_filter_dxdxi
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(CRS_matrix_connect), intent(in) :: tbl_crs
      type(CRS_matrix), intent(in) :: mass
      type(lumped_mass_matrices), intent(in) :: m_lump
!
      integer(kind = kint), intent(in) :: itype_mass
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_l
      type(dxidx_data_type), intent(inout) :: dxidxs
!
!
      call int_dx_ele2_node(nod_comm, node, ele,                        &
     &   jac_3d, rhs_tbl, tbl_crs, mass, m_lump, itype_mass,            &
     &    dxidxs%dx_ele%dxi%df_dx, dxidxs%dx_nod%dxi%df_dx,             &
     &    fem_wk, f_l)
      call int_dx_ele2_node(nod_comm, node, ele,                        &
     &   jac_3d, rhs_tbl, tbl_crs, mass, m_lump, itype_mass,            &
     &    dxidxs%dx_nod%dxi%df_dy, dxidxs%dx_nod%dxi%df_dy,             &
     &    fem_wk, f_l)
      call int_dx_ele2_node(nod_comm, node, ele,                        &
     &   jac_3d, rhs_tbl, tbl_crs, mass, m_lump, itype_mass,            &
     &    dxidxs%dx_nod%dxi%df_dz, dxidxs%dx_nod%dxi%df_dz,             &
     &    fem_wk, f_l)
!
      call int_dx_ele2_node(nod_comm, node, ele,                        &
     &   jac_3d, rhs_tbl, tbl_crs, mass, m_lump, itype_mass,            &
     &    dxidxs%dx_ele%dei%df_dx, dxidxs%dx_nod%dei%df_dx,             &
     &    fem_wk, f_l)
      call int_dx_ele2_node(nod_comm, node, ele,                        &
     &   jac_3d, rhs_tbl, tbl_crs, mass, m_lump, itype_mass,            &
     &    dxidxs%dx_nod%dei%df_dy, dxidxs%dx_nod%dei%df_dy,             &
     &    fem_wk, f_l)
      call int_dx_ele2_node(nod_comm, node, ele,                        &
     &   jac_3d, rhs_tbl, tbl_crs, mass, m_lump, itype_mass,            &
     &    dxidxs%dx_nod%dei%df_dz, dxidxs%dx_nod%dei%df_dz,             &
     &    fem_wk, f_l)
!
      call int_dx_ele2_node(nod_comm, node, ele,                        &
     &   jac_3d, rhs_tbl, tbl_crs, mass, m_lump, itype_mass,            &
     &    dxidxs%dx_ele%dzi%df_dx, dxidxs%dx_nod%dzi%df_dx,             &
     &    fem_wk, f_l)
      call int_dx_ele2_node(nod_comm, node, ele,                        &
     &   jac_3d, rhs_tbl, tbl_crs, mass, m_lump, itype_mass,            &
     &    dxidxs%dx_nod%dzi%df_dy, dxidxs%dx_nod%dzi%df_dy,             &
     &    fem_wk, f_l)
      call int_dx_ele2_node(nod_comm, node, ele,                        &
     &   jac_3d, rhs_tbl, tbl_crs, mass, m_lump, itype_mass,            &
     &    dxidxs%dx_nod%dzi%df_dz, dxidxs%dx_nod%dzi%df_dz,             &
     &    fem_wk, f_l)
!
      end subroutine cal_dxi_dxes_node
!
! -----------------------------------------------------------------------
!
      end module cal_deltax_and_prods_4_nod

!cal_1st_diff_deltax_4_nod.f90
!     module cal_1st_diff_deltax_4_nod
!
!     Written by H. Matsui on Mar., 2008
!
!      subroutine cal_1st_diffs_dx_by_consist(nod_comm, node, ele,      &
!     &         jac_3d, rhs_tbl, tbl_crs, mass, FEM_elen, fem_wk, f_nl)
!      subroutine cal_diffs_filter_nod_consist(nod_comm, node, ele,     &
!     &          jac_3d, rhs_tbl, tbl_crs, mass, fem_wk, f_nl, mom_nod)
!      subroutine cal_diffs_filter_nod_lump(node, ele, jac_3d,          &
!     &         rhs_tbl, m_lump, fem_wk, f_nl, mom_nod)
!      subroutine cal_1st_diffs_dx_by_lump                              &
!     &         (rhs_tbl, m_lump, FEM_elen, fem_wk, f_nl)
!
      module cal_1st_diff_deltax_4_nod
!
      use m_precision
      use m_phys_constants
!
      use t_geometry_data
      use t_jacobians
      use t_table_FEM_const
      use t_finite_element_mat
!
      implicit none
!
      private :: take_1st_diffs_nod_by_consist
      private :: take_1st_diffs_nod_by_lump
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine cal_1st_diffs_dx_by_consist(nod_comm, node, ele,       &
     &         jac_3d, rhs_tbl, tbl_crs, mass, FEM_elen, fem_wk, f_nl)
!
      use t_comm_table
      use t_crs_matrix
      use t_filter_elength
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(CRS_matrix_connect), intent(in) :: tbl_crs
      type(CRS_matrix), intent(in) :: mass
!
      type(gradient_model_data_type), intent(inout) :: FEM_elen
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
!      1st derivatives
!
      call take_1st_diffs_nod_by_consist(nod_comm, node, ele,           &
     &    jac_3d, rhs_tbl, tbl_crs, mass, fem_wk, f_nl,                 &
     &    FEM_elen%elen_nod%moms%f_x2, FEM_elen%elen_nod%diff%df_x2)
      call take_1st_diffs_nod_by_consist(nod_comm, node, ele,           &
     &    jac_3d, rhs_tbl, tbl_crs, mass, fem_wk, f_nl,                 &
     &    FEM_elen%elen_nod%moms%f_y2, FEM_elen%elen_nod%diff%df_y2)
      call take_1st_diffs_nod_by_consist(nod_comm, node, ele,           &
     &    jac_3d, rhs_tbl, tbl_crs, mass, fem_wk, f_nl,                 &
     &    FEM_elen%elen_nod%moms%f_z2, FEM_elen%elen_nod%diff%df_z2)
!
      call take_1st_diffs_nod_by_consist(nod_comm, node, ele,           &
     &    jac_3d, rhs_tbl, tbl_crs, mass, fem_wk, f_nl,                 &
     &    FEM_elen%elen_nod%moms%f_xy, FEM_elen%elen_nod%diff%df_xy)
      call take_1st_diffs_nod_by_consist(nod_comm, node, ele,           &
     &    jac_3d, rhs_tbl, tbl_crs, mass, fem_wk, f_nl,                 &
     &    FEM_elen%elen_nod%moms%f_yz, FEM_elen%elen_nod%diff%df_yz)
      call take_1st_diffs_nod_by_consist(nod_comm, node, ele,           &
     &    jac_3d, rhs_tbl, tbl_crs, mass, fem_wk, f_nl,                 &
     &    FEM_elen%elen_nod%moms%f_zx, FEM_elen%elen_nod%diff%df_zx)
!
      end subroutine cal_1st_diffs_dx_by_consist
!
!-----------------------------------------------------------------------
!
      subroutine cal_diffs_filter_nod_consist(nod_comm, node, ele,      &
     &          jac_3d, rhs_tbl, tbl_crs, mass, fem_wk, f_nl, mom_nod)
!
      use t_comm_table
      use t_crs_matrix
      use t_filter_moments
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(CRS_matrix_connect), intent(in) :: tbl_crs
      type(CRS_matrix), intent(in) :: mass
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
!
!      1st derivatives
!
      call take_1st_diffs_nod_by_consist(nod_comm, node, ele,           &
     &    jac_3d, rhs_tbl, tbl_crs, mass, fem_wk, f_nl,                 &
     &    mom_nod%moms%f_x,  mom_nod%diff%df_x)
      call take_1st_diffs_nod_by_consist(nod_comm, node, ele,           &
     &    jac_3d, rhs_tbl, tbl_crs, mass, fem_wk, f_nl,                 &
     &    mom_nod%moms%f_y,  mom_nod%diff%df_y)
      call take_1st_diffs_nod_by_consist(nod_comm, node, ele,           &
     &    jac_3d, rhs_tbl, tbl_crs, mass, fem_wk, f_nl,                 &
     &    mom_nod%moms%f_z,  mom_nod%diff%df_z)
!
      call take_1st_diffs_nod_by_consist(nod_comm, node, ele,           &
     &    jac_3d, rhs_tbl, tbl_crs, mass, fem_wk, f_nl,                 &
     &    mom_nod%moms%f_x2, mom_nod%diff%df_x2)
      call take_1st_diffs_nod_by_consist(nod_comm, node, ele,           &
     &    jac_3d, rhs_tbl, tbl_crs, mass, fem_wk, f_nl,                 &
     &    mom_nod%moms%f_y2, mom_nod%diff%df_y2)
      call take_1st_diffs_nod_by_consist(nod_comm, node, ele,           &
     &    jac_3d, rhs_tbl, tbl_crs, mass, fem_wk, f_nl,                 &
     &    mom_nod%moms%f_z2, mom_nod%diff%df_z2)
!
      call take_1st_diffs_nod_by_consist(nod_comm, node, ele,           &
     &    jac_3d, rhs_tbl, tbl_crs, mass, fem_wk, f_nl,                 &
     &    mom_nod%moms%f_xy, mom_nod%diff%df_xy)
      call take_1st_diffs_nod_by_consist(nod_comm, node, ele,           &
     &    jac_3d, rhs_tbl, tbl_crs, mass, fem_wk, f_nl,                 &
     &    mom_nod%moms%f_yz, mom_nod%diff%df_yz)
      call take_1st_diffs_nod_by_consist(nod_comm, node, ele,           &
     &    jac_3d, rhs_tbl, tbl_crs, mass, fem_wk, f_nl,                 &
     &    mom_nod%moms%f_zx, mom_nod%diff%df_zx)
!
      end subroutine cal_diffs_filter_nod_consist
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine cal_1st_diffs_dx_by_lump(node, ele, jac_3d,            &
     &          rhs_tbl, m_lump, FEM_elen, fem_wk, f_nl)
!
      use t_filter_elength
!
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
!
      type(gradient_model_data_type), intent(inout) :: FEM_elen
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
!      1st derivatives
!
      call take_1st_diffs_nod_by_lump                                   &
     &   (node, ele, jac_3d, rhs_tbl, m_lump, fem_wk, f_nl,             &
     &    FEM_elen%elen_nod%moms%f_x2, FEM_elen%elen_nod%diff%df_x2)
      call take_1st_diffs_nod_by_lump                                   &
     &   (node, ele, jac_3d, rhs_tbl, m_lump, fem_wk, f_nl,             &
     &    FEM_elen%elen_nod%moms%f_y2, FEM_elen%elen_nod%diff%df_y2)
      call take_1st_diffs_nod_by_lump                                   &
     &   (node, ele, jac_3d, rhs_tbl, m_lump, fem_wk, f_nl,             &
     &    FEM_elen%elen_nod%moms%f_z2, FEM_elen%elen_nod%diff%df_z2)
!
      call take_1st_diffs_nod_by_lump                                   &
     &   (node, ele, jac_3d, rhs_tbl, m_lump, fem_wk, f_nl,             &
     &    FEM_elen%elen_nod%moms%f_xy, FEM_elen%elen_nod%diff%df_xy)
      call take_1st_diffs_nod_by_lump                                   &
     &   (node, ele, jac_3d, rhs_tbl, m_lump, fem_wk, f_nl,             &
     &    FEM_elen%elen_nod%moms%f_yz, FEM_elen%elen_nod%diff%df_yz)
      call take_1st_diffs_nod_by_lump                                   &
     &   (node, ele, jac_3d, rhs_tbl, m_lump, fem_wk, f_nl,             &
     &    FEM_elen%elen_nod%moms%f_zx, FEM_elen%elen_nod%diff%df_zx)
!
      end subroutine cal_1st_diffs_dx_by_lump
!
!-----------------------------------------------------------------------
!
      subroutine cal_diffs_filter_nod_lump(node, ele, jac_3d,           &
     &         rhs_tbl, m_lump, fem_wk, f_nl, mom_nod)
!
      use t_filter_moments
!
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
      type(nod_mom_diffs_type), intent(inout) :: mom_nod
!
!      1st derivatives
!
      call take_1st_diffs_nod_by_lump                                   &
     &   (node, ele, jac_3d, rhs_tbl, m_lump, fem_wk, f_nl,             &
     &    mom_nod%moms%f_x,  mom_nod%diff%df_x)
      call take_1st_diffs_nod_by_lump                                   &
     &   (node, ele, jac_3d, rhs_tbl, m_lump, fem_wk, f_nl,             &
     &    mom_nod%moms%f_y,  mom_nod%diff%df_y)
      call take_1st_diffs_nod_by_lump                                   &
     &   (node, ele, jac_3d, rhs_tbl, m_lump, fem_wk, f_nl,             &
     &    mom_nod%moms%f_z,  mom_nod%diff%df_z)
!
      call take_1st_diffs_nod_by_lump                                   &
     &   (node, ele, jac_3d, rhs_tbl, m_lump, fem_wk, f_nl,             &
     &    mom_nod%moms%f_x2, mom_nod%diff%df_x2)
      call take_1st_diffs_nod_by_lump                                   &
     &   (node, ele, jac_3d, rhs_tbl, m_lump, fem_wk, f_nl,             &
     &    mom_nod%moms%f_y2, mom_nod%diff%df_y2)
      call take_1st_diffs_nod_by_lump                                   &
     &   (node, ele, jac_3d, rhs_tbl, m_lump, fem_wk, f_nl,             &
     &    mom_nod%moms%f_z2, mom_nod%diff%df_z2)
!
      call take_1st_diffs_nod_by_lump                                   &
     &   (node, ele, jac_3d, rhs_tbl, m_lump, fem_wk, f_nl,             &
     &    mom_nod%moms%f_xy, mom_nod%diff%df_xy)
      call take_1st_diffs_nod_by_lump                                   &
     &   (node, ele, jac_3d, rhs_tbl, m_lump, fem_wk, f_nl,             &
     &    mom_nod%moms%f_yz, mom_nod%diff%df_yz)
      call take_1st_diffs_nod_by_lump                                   &
     &   (node, ele, jac_3d, rhs_tbl, m_lump, fem_wk, f_nl,             &
     &    mom_nod%moms%f_zx, mom_nod%diff%df_zx)
!
      end subroutine cal_diffs_filter_nod_lump
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine take_1st_diffs_nod_by_consist                          &
     &         (nod_comm, node, ele, jac_3d, rhs_tbl,                   &
     &          tbl_crs, mass, fem_wk, f_nl, org_field, diff_field)
!
      use t_comm_table
      use t_crs_matrix
!
      use int_vol_elesize_on_node
      use cal_ff_smp_to_ffs
      use cal_sol_deltax_by_consist
!
      type(communication_table), intent(in) :: nod_comm
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(CRS_matrix_connect), intent(in) :: tbl_crs
      type(CRS_matrix), intent(in) :: mass
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      real(kind = kreal), intent(inout) :: org_field(node%numnod)
      real(kind = kreal), intent(inout) :: diff_field(node%numnod,3)
      integer(kind = kint) :: nd
!
!
      call reset_ff(node%numnod, f_nl)
      call reset_ff_smp(node%max_nod_smp, f_nl)
!
      call int_vol_diff_dxs(node, ele, jac_3d,                          &
     &    rhs_tbl, fem_wk, f_nl, org_field)
      call cal_ff_smp_2_ff(node, rhs_tbl, n_vector,                     &
     &    f_nl%ff_smp, f_nl%ff)
      do nd = 1, n_vector
        call cal_sol_dx_by_consist                                      &
     &     (node, nod_comm, tbl_crs, mass, f_nl, diff_field(1,nd), nd)
      end do
!
      end subroutine take_1st_diffs_nod_by_consist
!
!-----------------------------------------------------------------------
!
      subroutine take_1st_diffs_nod_by_lump                             &
     &         (node, ele, jac_3d, rhs_tbl, m_lump, fem_wk, f_nl,       &
     &          org_field, diff_field)
!
      use int_vol_elesize_on_node
      use cal_ff_smp_to_ffs
!
      type(node_data),    intent(in) :: node
      type(element_data), intent(in) :: ele
      type(jacobians_3d), intent(in) :: jac_3d
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(lumped_mass_matrices), intent(in) :: m_lump
!
      type(work_finite_element_mat), intent(inout) :: fem_wk
      type(finite_ele_mat_node), intent(inout) :: f_nl
!
      real(kind = kreal), intent(inout) :: org_field(node%numnod)
      real(kind = kreal), intent(inout)                                 &
     &                   :: diff_field(node%numnod,n_vector)
!
      call reset_ff_smp(node%max_nod_smp, f_nl)
!
      call int_vol_diff_dxs(node, ele, jac_3d,                          &
     &    rhs_tbl, fem_wk, f_nl, org_field)
      call cal_ff_smp_2_vector(node, rhs_tbl,                           &
     &    f_nl%ff_smp, m_lump%ml, n_vector, ione, diff_field)
!
      end subroutine take_1st_diffs_nod_by_lump
!
!-----------------------------------------------------------------------
!
      end module cal_1st_diff_deltax_4_nod

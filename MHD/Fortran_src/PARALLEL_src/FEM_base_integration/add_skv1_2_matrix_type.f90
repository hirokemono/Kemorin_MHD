!add_skv1_2_matrix_type.f90
!     module add_skv1_2_matrix_type
!
!      subroutine add_skv1_2_matrix11_type(ele, rhs_tbl, mat_tbl,       &
!     &          fem_wk, k2, mat11)
!      subroutine add_skv1_2_matrix33_type(ele, rhs_tbl, mat_tbl,       &
!     &          fem_wk, k2, mat33)
!
      module add_skv1_2_matrix_type
!
      use m_precision
!
      use m_machine_parameter
      use t_geometry_data
      use t_table_FEM_const
      use t_finite_element_mat
      use t_solver_djds
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine add_skv1_2_matrix11_type(ele, rhs_tbl, mat_tbl,        &
     &          fem_wk, k2, mat11)
!
      use add_skv1_2_matrix
!
      type(element_data), intent(in) :: ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: mat_tbl
!
      integer (kind = kint), intent(in) :: k2
      type(work_finite_element_mat), intent(in) :: fem_wk
!
      type(DJDS_MATRIX),  intent(inout) :: mat11
!
!
      call add_skv1_2_matrix11(np_smp, ele%numele, ele%nnod_4_ele,      &
     &    rhs_tbl%inod_ele_max, rhs_tbl%num_sort_smp,                   &
     &    rhs_tbl%nod_stack_smp, rhs_tbl%iele_sort_smp,                 &
     &    rhs_tbl%iconn_sort_smp, mat_tbl%idx_4_mat,                    &
     &    k2, fem_wk%sk6, mat11%num_comp, mat11%aiccg)
!
      end subroutine add_skv1_2_matrix11_type
!
!-----------------------------------------------------------------------
!
      subroutine add_skv1_2_matrix33_type(ele, rhs_tbl, mat_tbl,        &
     &          fem_wk, k2, mat33)
!
      use add_skv1_2_matrix
!
      type(element_data), intent(in) :: ele
      type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
      type(table_mat_const), intent(in) :: mat_tbl
!
      integer (kind = kint), intent(in) :: k2
      type(work_finite_element_mat), intent(in) :: fem_wk
!
      type(DJDS_MATRIX),  intent(inout) :: mat33
!
!
      call add_skv1_2_matrix33(np_smp, ele%numele, ele%nnod_4_ele,      &
     &    rhs_tbl%inod_ele_max, rhs_tbl%num_sort_smp,                   &
     &    rhs_tbl%nod_stack_smp, rhs_tbl%iele_sort_smp,                 &
     &    rhs_tbl%iconn_sort_smp, mat_tbl%idx_4_mat,                    &
     &    k2, fem_wk%sk6, mat33%num_comp, mat33%aiccg)
!
      end subroutine add_skv1_2_matrix33_type
!
!-----------------------------------------------------------------------
!
      end module add_skv1_2_matrix_type

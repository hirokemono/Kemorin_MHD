!
!     module   t_sorted_node_MHD
!.......................................................................
!
!     Written by H. Matsui
!
!!      subroutine set_index_list_4_mat_etr_l                           &
!!     &         (node, ele, rhs_tbl, djds_tbl_lin, mat_tbl_q, linear)
!!        type(node_data), intent(in) :: node
!!        type(element_data), intent(in) :: ele
!!        type(field_geometry_data), intent(in) :: fluid
!!        type(field_geometry_data), intent(in) :: conduct
!!        type(tables_4_FEM_assembles), intent(in) :: rhs_tbl
!!        type(table_mat_const), intent(in) :: mat_tbl_q
!!
!!      subroutine set_index_list_4_mat_fl                              &
!!     &         (node, ele, fluid, rhs_tbl, djds_tbl_fl, fluid_q)
!!      subroutine set_index_list_4_mat_fl_l(node, ele, fluid,          &
!!     &          rhs_tbl, djds_tbl_fll, fluid_q, fluid_l)
!!
!!      subroutine set_index_list_4_mat_fullcd(node, ele, conduct,      &
!!     &          rhs_tbl, djds_tbl, full_conduct_q)
!!
!!      subroutine deallocate_MHD_matrix_lists(MHD_mat_tbls)
!
      module t_sorted_node_MHD
!
      use m_precision
      use m_geometry_constants
      use t_geometry_data_MHD
      use t_geometry_data
      use t_table_FEM_const
!
      implicit  none
!
!>  Structures for FEM marix table
      type tables_MHD_mat_const
!>  Structure for linear FEM marix table
        type(table_mat_const) :: base
!>  Structure for linear FEM marix table
        type(table_mat_const) :: linear
!
!>  Structure for quad FEM marix table for fluid
        type(table_mat_const) :: fluid_q
!>  Structure for linear FEM marix table for fluid
        type(table_mat_const) :: fluid_l
!
!>  Structure for quad FEM marix table for conductor but whole domain
        type(table_mat_const) :: full_conduct_q
      end type tables_MHD_mat_const
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine deallocate_MHD_matrix_lists(MHD_mat_tbls)
!
      type(tables_MHD_mat_const), intent(inout) :: MHD_mat_tbls
!
!
      call dealloc_type_marix_list(MHD_mat_tbls%linear)
!
      call dealloc_type_marix_list(MHD_mat_tbls%fluid_q)
      call dealloc_type_marix_list(MHD_mat_tbls%full_conduct_q)
!
      call dealloc_type_marix_list(MHD_mat_tbls%fluid_l)
!
      end subroutine deallocate_MHD_matrix_lists
!
!-----------------------------------------------------------------------
!
      end module t_sorted_node_MHD

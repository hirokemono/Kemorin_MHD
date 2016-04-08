!
!     module   t_sorted_node_MHD
!.......................................................................
!
!     Written by H. Matsui
!
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

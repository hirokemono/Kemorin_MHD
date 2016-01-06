!>@file   m_element_id_4_node.f90
!!@brief  module m_element_id_4_node
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2006
!
!> @brief included element list for each node
!!
!!@verbatim
!!       subroutine set_idx_list_4_whole_crs
!!
!!       subroutine deallocate_sorted_node
!
!!       subroutine deallocate_marix_list
!!       subroutine deallocate_marix_list_l
!!@endverbatim
!!
      module m_element_id_4_node
!
      use m_precision
      use m_constants
!
      use t_next_node_ele_4_node
      use t_table_FEM_const
!
      use t_crs_connect
      use t_crs_matrix
!
      implicit none
!
!>   Structure of neighbouring node and element list for each node
      type(next_nod_ele_table), save :: next_tbl1
!
!>  Structure for FEM construction table
      type(tables_4_FEM_assembles), save :: rhs_tbl1
!
!>  Structure for quad FEM marix table
      type(table_mat_const), save :: mat_tbl_q1
!
!
!>  Structures for index table for compressed raw strage matrix
      type(CRS_matrix_connect), save :: tbl1_crs
!
!>  Structures for compressed raw strage matrix
      type(CRS_matrix), save :: mat1_crs
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
!
      subroutine set_connect_RHS_assemble
!
      use m_geometry_data
      use set_table_type_RHS_assemble
!
!      Search surrounding node and element
!
      call s_set_table_type_RHS_assemble                                &
     &   (node1, ele1, next_tbl1, rhs_tbl1)
!
      end subroutine set_connect_RHS_assemble
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_idx_list_4_whole_crs
!
      use m_geometry_data
      use set_index_list_4_crs
!
!
      call set_idx_list_whole_crs_mat                                   &
     &   (node1, ele1, tbl1_crs, rhs_tbl1, mat_tbl_q1)
!
      end subroutine set_idx_list_4_whole_crs
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine deallocate_sorted_node
!
      call dealloc_type_sort_smp(rhs_tbl1)
      call dealloc_type_sorted_node(rhs_tbl1)
!
      end subroutine deallocate_sorted_node
!
!-----------------------------------------------------------------------
!
       subroutine deallocate_marix_list
!
      call dealloc_type_marix_list(mat_tbl_q1)
!
       end subroutine deallocate_marix_list
!
!-----------------------------------------------------------------------
!
      end module m_element_id_4_node

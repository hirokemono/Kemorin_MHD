!
!     module   m_sorted_node
!.......................................................................
!
!     Written by H. Matsui
!
!      subroutine sort_node_index
!      subroutine set_idx_list_4_whole_crs
!
!       subroutine deallocate_sorted_node
!
!       subroutine deallocate_marix_list
!       subroutine deallocate_marix_list_l
!
      module m_sorted_node
!
      use m_precision
      use t_table_FEM_const
!
      implicit  none
!
!>  Structure for FEM construction table
      type(tables_4_FEM_assembles), save :: rhs_tbl1
!
!>  Structure for quad FEM marix table
      type(table_mat_const), save :: mat_tbl_q1
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine set_connect_RHS_assemble
!
      use m_next_node_id_4_node
!
!      Search surrounding node and element
!
      call set_belonged_ele_and_next_nod
!
!      set RHS assemble table
!
      call sort_node_index
!
      end subroutine set_connect_RHS_assemble
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sort_node_index
!
      use m_geometry_data
      use m_element_id_4_node
!
      use ordering_rhs_assemble_type
!
!
      call s_sort_node_index_type(node1, ele_4_nod1, rhs_tbl1)
!
      end  subroutine sort_node_index
!
!-----------------------------------------------------------------------
!
      subroutine set_idx_list_4_whole_crs
!
      use m_geometry_data
      use set_index_list_4_crs
!
!
      call alloc_type_marix_list(ele1%nnod_4_ele, rhs_tbl1, mat_tbl_q1)
!
      call s_set_index_list_4_crs(node1%numnod, node1%internal_node,    &
     &    ele1%numele, ele1%nnod_4_ele, ele1%ie,                        &
     &    rhs_tbl1%inod_ele_max, rhs_tbl1%num_sort_smp,                 &
     &    rhs_tbl1%nod_stack_smp, rhs_tbl1%iele_sort_smp,               &
     &    rhs_tbl1%iconn_sort_smp, mat_tbl_q1%idx_4_mat)
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
      end module m_sorted_node

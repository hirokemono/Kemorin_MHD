!
!      module copy_merge_mesh_2_IO
!
!      Written by H. Matsui on Jan., 2007
!
!      subroutine s_copy_merge_mesh_2_IO
!
      module copy_merge_mesh_2_IO
!
      use m_precision
!
      use m_geometry_data_4_merge
      use m_read_mesh_data
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_copy_merge_mesh_2_IO
!
      use m_constants
!
      use m_read_mesh_data
      use m_comm_data_IO
      use set_nnod_4_ele_by_type
      use set_node_types_4_IO
      use set_element_types_4_IO
      use set_group_types_4_IO
!
      integer (kind = kint) :: i
!
!
      num_neib_domain_IO = izero
      call allocate_neib_comm_stack_IO
      call allocate_comm_item_IO
      call copy_node_type_to_IO(merged%node)
      call copy_ele_connect_type_to_IO(merged%ele)
!
      numnod_dummy =        merge_tbl%nnod_merged
      internal_node_dummy = merge_tbl%nnod_merged
!
      numele_dummy = merge_tbl%nele_merged
      do i = 1, merge_tbl%nele_merged
        call s_set_nnod_4_ele_by_type(nodelm_dummy(i),i_ele_dummy(i))
      end do
!
      call set_grp_data_type_to_IO(merged_grp)
!
      call deallocate_node_geometry_type(merged%node)
      call deallocate_array_4_merge
!
      end subroutine s_copy_merge_mesh_2_IO
!
! ----------------------------------------------------------------------
!
      end module copy_merge_mesh_2_IO

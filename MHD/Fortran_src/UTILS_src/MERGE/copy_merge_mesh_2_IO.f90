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
      use m_read_boundary_data
      use set_nnod_4_ele_by_type
      use set_node_data_4_IO
      use set_element_data_4_IO
!
      integer (kind = kint) :: i
!
!
      comm_IO%num_neib = izero
      call allocate_type_comm_tbl_num(comm_IO)
      call allocate_type_comm_tbl_item(comm_IO)
      call copy_node_geometry_to_IO(merged%node)
      call copy_ele_connect_to_IO(merged%ele)
!
      nod_IO%numnod =        merge_tbl%nnod_merged
      nod_IO%internal_node = merge_tbl%nnod_merged
!
      ele_IO%numele = merge_tbl%nele_merged
      do i = 1, merge_tbl%nele_merged
        call s_set_nnod_4_ele_by_type                                   &
     &     (ele_IO%elmtyp(i), ele_IO%nodelm(i))
      end do
!
      call set_grp_data_to_IO                                           &
     &   (merged_grp%nod_grp, merged_grp%ele_grp, merged_grp%surf_grp)
!
      call dealloc_groups_data(merged_grp)
      call deallocate_ele_connect_type(merged%ele)
      call deallocate_node_geometry_type(merged%node)
      call deallocate_array_4_merge
!
      end subroutine s_copy_merge_mesh_2_IO
!
! ----------------------------------------------------------------------
!
      end module copy_merge_mesh_2_IO

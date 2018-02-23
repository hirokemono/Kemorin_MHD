!
!      module m_geometry_data_4_merge
!
!      Written by H. Matsui
!      Modified by H. Matsui on Apr., 2012
!
!      subroutine allocate_geometry_data_4_merge
!
!      subroutine allocate_number_of_mesh
!      subroutine allocate_array_4_node
!      subroutine allocate_array_4_element
!
!      subroutine allocate_merged_group_num
!      subroutine allocate_merged_group_item
!
!      subroutine deallocate_array_4_merge
!      subroutine deallocate_number_of_mesh
!
      module m_geometry_data_4_merge
!
      use m_precision
!
      use m_constants
      use t_mesh_data
      use t_group_data
      use t_merged_geometry_data
      use t_phys_data
      use t_mesh_data_4_merge
!
      implicit    none
!
!  ==============================
! . for mesh data & result data
!  ==============================
!
      type(merged_mesh), save :: mgd_mesh1
!
      type(merged_stacks) :: merge_tbl
!>      merged index table
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_geometry_data_4_merge
!
!
      call allocate_array_4_node
      call allocate_array_4_element
!
      end subroutine allocate_geometry_data_4_merge
!
!------------------------------------------------------------------
!
      subroutine allocate_number_of_mesh
!
!
      merge_tbl%num_subdomain = mgd_mesh1%num_pe
      allocate( mgd_mesh1%subdomain(mgd_mesh1%num_pe) )
!
      call alloc_subdomain_stack(mgd_mesh1%num_pe, merge_tbl)
!
      end subroutine allocate_number_of_mesh
!
!------------------------------------------------------------------
!
      subroutine deallocate_number_of_mesh
!
      call dealloc_subdomain_stack(merge_tbl)
!
      end subroutine deallocate_number_of_mesh
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_array_4_node
!
      use t_geometry_data
!
      integer(kind = kint) :: i
!
!
      call allocate_node_geometry_type(mgd_mesh1%merged%node)
      call alloc_local_nod_id_tbl(merge_tbl)
!
      do i = 1, mgd_mesh1%merged%node%numnod
        mgd_mesh1%merged%node%inod_global(i) = i
      end do
!
      end subroutine allocate_array_4_node
!
!------------------------------------------------------------------
!
      subroutine allocate_array_4_element
!
      use t_geometry_data
!
      integer(kind = kint) :: i
!
!
      call allocate_ele_connect_type(mgd_mesh1%merged%ele)
      call alloc_local_ele_id_tbl(merge_tbl)
!
      do i = 1, mgd_mesh1%merged%ele%numele
        mgd_mesh1%merged%ele%iele_global(i) = i
      end do
!
      end subroutine allocate_array_4_element
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_array_4_merge
!
!
      call dealloc_local_nod_id_tbl(merge_tbl)
      call dealloc_local_ele_id_tbl(merge_tbl)
!
      end subroutine deallocate_array_4_merge
!
!------------------------------------------------------------------
!
      end module m_geometry_data_4_merge

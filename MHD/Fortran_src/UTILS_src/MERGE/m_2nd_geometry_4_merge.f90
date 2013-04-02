!
!      module m_2nd_geometry_4_merge
!
!      Written by H. Matsui on Feb., 2007
!      Modified by H. Matsui on Apr., 2012
!
!      subroutine allocate_number_of_2nd_mesh
!      subroutine allocate_2nd_merged_geometry
!      subroutine allocate_2nd_merge_table
!
!      subroutine deallocate_number_of_2nd_mesh
!
!      subroutine deallocate_2nd_merge_table
!
      module m_2nd_geometry_4_merge
!
      use m_precision
!
      use t_mesh_data
      use t_merged_geometry_data
!
      implicit none
!
      integer(kind = kint) :: num_pe2
      type(mesh_geometry), allocatable :: subdomains_2(:)
      type(merged_stacks) :: merge_tbl_2
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_number_of_2nd_mesh
!
      allocate( subdomains_2(num_pe2) )
!
      call alloc_subdomain_stack(num_pe2, merge_tbl_2)
!
      end subroutine allocate_number_of_2nd_mesh
!
!------------------------------------------------------------------
!
      subroutine allocate_2nd_merged_geometry
!
      use t_geometry_data
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, num_pe2
        call allocate_node_geometry_type(subdomains_2(ip)%node)
        call allocate_ele_connect_type(subdomains_2(ip)%ele)
      end do
!
      end subroutine allocate_2nd_merged_geometry
!
!------------------------------------------------------------------
!
      subroutine allocate_2nd_merge_table
!
!
      call alloc_local_nod_id_tbl(merge_tbl_2)
      call alloc_local_ele_id_tbl(merge_tbl_2)
!
      end subroutine allocate_2nd_merge_table
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_number_of_2nd_mesh
!
      call dealloc_subdomain_stack(merge_tbl_2)
!
      end subroutine deallocate_number_of_2nd_mesh
!
!------------------------------------------------------------------
!
      subroutine deallocate_2nd_merge_table
!
!
      call dealloc_local_nod_id_tbl(merge_tbl_2)
      call dealloc_local_ele_id_tbl(merge_tbl_2)
!
      end subroutine deallocate_2nd_merge_table
!
!------------------------------------------------------------------
!
      end module m_2nd_geometry_4_merge
!

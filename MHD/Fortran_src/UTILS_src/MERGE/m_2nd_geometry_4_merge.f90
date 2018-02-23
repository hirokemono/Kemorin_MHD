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
      use t_mesh_data_4_merge
!
      implicit none
!
      type(second_mesh), save :: sec_mesh1
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_number_of_2nd_mesh
!
      allocate( sec_mesh1%subdomains_2(sec_mesh1%num_pe2) )
!
      call alloc_subdomain_stack                                        &
     &   (sec_mesh1%num_pe2, sec_mesh1%merge_tbl_2)
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
      do ip = 1, sec_mesh1%num_pe2
        call allocate_node_geometry_type(sec_mesh1%subdomains_2(ip)%node)
        call allocate_ele_connect_type(sec_mesh1%subdomains_2(ip)%ele)
      end do
!
      end subroutine allocate_2nd_merged_geometry
!
!------------------------------------------------------------------
!
      subroutine allocate_2nd_merge_table
!
!
      call alloc_local_nod_id_tbl(sec_mesh1%merge_tbl_2)
      call alloc_local_ele_id_tbl(sec_mesh1%merge_tbl_2)
!
      end subroutine allocate_2nd_merge_table
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_number_of_2nd_mesh
!
      call dealloc_subdomain_stack(sec_mesh1%merge_tbl_2)
!
      end subroutine deallocate_number_of_2nd_mesh
!
!------------------------------------------------------------------
!
      subroutine deallocate_2nd_merge_table
!
!
      call dealloc_local_nod_id_tbl(sec_mesh1%merge_tbl_2)
      call dealloc_local_ele_id_tbl(sec_mesh1%merge_tbl_2)
!
      end subroutine deallocate_2nd_merge_table
!
!------------------------------------------------------------------
!
      end module m_2nd_geometry_4_merge
!

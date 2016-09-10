!
!      module const_newdomain_filter
!
!      modified by H. Matsui on Apr., 2008
!
!!      subroutine marking_used_node_4_filtering                        &
!!     &         (ip2, ifile_type, node, numele)
!!      subroutine trans_filter_4_new_domains                           &
!!     &         (ip2, ifile_type, node, numele)
!
      module const_newdomain_filter
!
      use m_precision
!
      use calypso_mpi
      use t_geometry_data
      use m_read_mesh_data
      use m_filter_func_4_sorting
      use m_filter_coefs
      use m_comm_data_IO
      use set_parallel_file_name
      use mesh_IO_select
      use read_org_filter_coefs
      use set_node_data_4_IO
      use set_filters_4_new_domains
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine marking_used_node_4_filtering                          &
     &         (ip2, ifile_type, node, numele)
!
      integer(kind = kint), intent(in) :: ip2, ifile_type
      integer(kind = kint), intent(inout) :: numele
      type(node_data), intent(inout) :: node
      integer(kind = kint) :: ip, my_rank
!
!
      call clear_imark_whole_nod
!
      do ip = 1, nprocs
        my_rank = ip - 1
!
        mesh_file_head = mesh_file_head
        call sel_read_geometry_size(my_rank)
        call copy_node_geometry_from_IO(node)
        call deallocate_type_neib_id(comm_IO)
!
        numele = ele_IO%numele
!
!     read filtering information
!
        call read_original_filter_coefs(ifile_type, my_rank,            &
     &      node%numnod, numele)
!
        call nod_marking_by_filtering_data                              &
     &     (node%numnod, node%internal_node, node%inod_global, node%xx, &
     &      ip2)
!
        call deallocate_whole_filter_coefs
        call deallocate_fluid_filter_coefs
!
        call dealloc_node_geometry_base(node)
      end do
!
      end subroutine marking_used_node_4_filtering
!
!------------------------------------------------------------------
!
      subroutine trans_filter_4_new_domains                             &
     &         (ip2, ifile_type, node, numele)
!
      integer(kind = kint), intent(in) :: ip2, ifile_type
      integer(kind = kint), intent(inout) :: numele
      type(node_data), intent(inout) :: node
      integer(kind = kint) :: ip, my_rank, icou_st
!
!
      icou_st = 0
      do ip = 1, nprocs
        my_rank = ip - 1
!
        call sel_read_geometry_size(my_rank)
        call copy_node_geometry_from_IO(node)
        call deallocate_type_neib_id(comm_IO)
!
        numele = ele_IO%numele
!
!     read filtering information
!
        call read_original_filter_coefs(ifile_type, my_rank,            &
     &      node%numnod, numele)
!
        call set_filter_for_new_each_domain                             &
     &     (node%numnod, node%internal_node, node%inod_global,          &
     &      ip2, icou_st)
!
        call deallocate_whole_filter_coefs
        call deallocate_fluid_filter_coefs
!
        call dealloc_node_geometry_base(node)
      end do
!
      end subroutine trans_filter_4_new_domains
!
!------------------------------------------------------------------
!
      end module const_newdomain_filter

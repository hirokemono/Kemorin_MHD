!set_inod_newdomain_filter.f90
!      module set_inod_newdomain_filter
!
!     Written by H. Matsui on May., 2008
!
!      subroutine set_inod_4_newdomain_filter                           &
!     &         (org_node, org_ele, new_node)
!
      module set_inod_newdomain_filter
!
      use m_precision
!
      use m_2nd_pallalel_vector
      use m_comm_data_IO
      use m_internal_4_partitioner
      use set_parallel_file_name
      use mesh_IO_select
      use const_newdomain_filter
      use set_filters_4_new_domains
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_inod_4_newdomain_filter                            &
     &         (org_node, org_ele, new_node)
!
      use t_geometry_data
!
      use m_internal_4_partitioner
      use m_filter_file_names
      use m_field_file_format
      use set_node_data_4_IO
!
      type(node_data),    intent(inout) :: org_node
      type(element_data), intent(inout) :: org_ele
      type(node_data), intent(inout) :: new_node
!
      integer(kind = kint) :: ip2, my_rank2
!
      do ip2 = 1, nprocs_2nd
        my_rank2 = ip2 - 1
!
        mesh_file_head = target_mesh_head
        call sel_read_geometry_size(my_rank2)
        call copy_node_geometry_from_IO(new_node)
!
        call deallocate_neib_domain_IO
!
        call marking_used_node_4_filtering                              &
     &     (ip2, ifmt_3d_filter, org_node, org_ele%numele)
!
        call set_num_globalnod_4_newdomain(ip2, new_node)
!
        call dealloc_node_geometry_base(new_node)
      end do
!
      end subroutine set_inod_4_newdomain_filter
!
!   --------------------------------------------------------------------
!
      end module set_inod_newdomain_filter

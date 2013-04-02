!set_inod_newdomain_filter.f90
!      module set_inod_newdomain_filter
!
      module set_inod_newdomain_filter
!
!     Written by H. Matsui on May., 2008
!
      use m_precision
!
      use m_2nd_pallalel_vector
      use m_2nd_geometry_data
      use m_comm_data_IO
      use m_internal_4_partitioner
      use set_parallel_file_name
      use mesh_IO_select
      use set_2nd_node_geometry_4_IO
      use const_newdomain_filter
      use set_filters_4_new_domains
!
      implicit none
!
!      subroutine set_inod_4_newdomain_filter
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_inod_4_newdomain_filter
!
      use m_internal_4_partitioner
!
      integer(kind = kint) :: ip2, my_rank2
!
      do ip2 = 1, nprocs_2nd
        my_rank2 = ip2 - 1
!
        mesh_file_head = target_mesh_head
        call sel_read_geometry_size(my_rank2)
        call copy_2nd_node_geometry_from_IO
!
        call deallocate_neib_domain_IO
!
        call marking_used_node_4_filtering(ip2, ifile_type)
!
        call set_num_globalnod_4_newdomain(ip2)
!
        call deallocate_2nd_node_position
!
      end do
!
      end subroutine set_inod_4_newdomain_filter
!
!   --------------------------------------------------------------------
!
      end module set_inod_newdomain_filter

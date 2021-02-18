!const_new_mesh_filter.f90
!      module const_new_mesh_filter
!
!     Written by H. Matsui on May., 2008
!
!!      subroutine const_mesh_newdomain_filter                          &
!!     &         (new_filter_coef_head, nprocs_2nd, itl_nod_part,       &
!!     &          new_comm, filter_node, comm_part)
!!      subroutine const_mesh_each_filter_domain                        &
!!     &         (my_rank2, new_filter_coef_head, itl_nod_part,         &
!!     &          new_comm, filter_node, comm_part)
!!        type(internal_4_partitioner), intent(in) :: itl_nod_part
!!        type(communication_table), intent(inout) :: new_comm
!!        type(node_data), intent(inout) :: filter_node
!!        type(partitioner_comm_tables), intent(inout) :: comm_part
!
      module const_new_mesh_filter
!
      use m_precision
      use t_geometry_data
      use t_internal_4_partitioner
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine const_mesh_newdomain_filter                            &
     &         (new_filter_coef_head, nprocs_2nd, itl_nod_part,         &
     &          new_comm, filter_node, comm_part)
!
      use t_comm_table
      use t_partitioner_comm_table
!
      character(len=kchara), intent(in) :: new_filter_coef_head
      integer, intent(in) :: nprocs_2nd
      type(internal_4_partitioner), intent(in) :: itl_nod_part
!
      type(communication_table), intent(inout) :: new_comm
      type(node_data), intent(inout) :: filter_node
      type(partitioner_comm_tables), intent(inout) :: comm_part
      integer :: my_rank2
!
!     output node data with communication table
!
      do my_rank2 = 0, nprocs_2nd-1
        call const_mesh_each_filter_domain                              &
     &     (my_rank2, new_filter_coef_head, itl_nod_part,               &
     &      new_comm, filter_node, comm_part)
      end do
!
      end subroutine const_mesh_newdomain_filter
!
!   --------------------------------------------------------------------
!
      subroutine const_mesh_each_filter_domain                          &
     &         (my_rank2, new_filter_coef_head, itl_nod_part,           &
     &          new_comm, filter_node, comm_part)
!
      use calypso_mpi
      use m_filter_file_names
      use m_file_format_switch
      use t_filter_file_data
      use t_partitioner_comm_table
      use set_parallel_file_name
      use filter_coefs_file_IO
      use filter_coefs_file_IO_b
      use filter_moment_IO_select
      use copy_mesh_structures
      use set_filters_4_new_domains
      use sel_part_nod_comm_input
!
      use t_comm_table
      use t_filter_file_data
!
      integer, intent(in) :: my_rank2
      character(len=kchara), intent(in) :: new_filter_coef_head
      type(internal_4_partitioner), intent(in) :: itl_nod_part
!
      type(node_data), intent(inout) :: filter_node
      type(communication_table), intent(inout) :: new_comm
      type(partitioner_comm_tables), intent(inout) :: comm_part
!
      type (filter_file_data), save :: filter_IO
      integer(kind = kint) :: ip2
!
!
      ip2 = my_rank2 + 1
!C
!C +--------------------------+
!C | read INITIAL LOCAL files |
!C +--------------------------+
!C===
      call load_node_comm_tbl_4_part(ip2, comm_part, new_comm)
!C
!C +-----------------+
!C | LOCAL NUMBERING |
!C +-----------------+
!C===
      new_comm%id_neib(1:new_comm%num_neib)                             &
     &        = new_comm%id_neib(1:new_comm%num_neib) - 1
!
      filter_node%numnod =        itl_nod_part%num_4_subdomain(ip2)
      filter_node%internal_node = itl_nod_part%num_inter_sub(ip2)
      call alloc_node_geometry_base(filter_node)
!          write(*,*) 'set_newdomain_filtering_nod'
      call set_newdomain_filtering_nod                                  &
     &   (ip2, itl_nod_part, filter_node)
!
!          write(*,*) 'copy_comm_tbl_type(my_rank, new_comm)'
      call copy_comm_tbl_type(new_comm, filter_IO%nod_comm)
      call dealloc_comm_table(new_comm)
!
!          write(*,*) 'copy_node_geometry to filter_IO'
      call copy_node_geometry(filter_node, filter_IO%node)
      call dealloc_node_geometry_base(filter_node)
!
      filter_file_head = new_filter_coef_head
      call sel_write_filter_geometry_file(my_rank2, filter_IO)
!
      write(*,*) 'write filter file end'
!
      end subroutine const_mesh_each_filter_domain
!
!   --------------------------------------------------------------------
!
      end module const_new_mesh_filter

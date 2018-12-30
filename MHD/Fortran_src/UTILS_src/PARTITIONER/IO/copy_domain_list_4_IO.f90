!
!     module copy_domain_list_4_IO
!
!      Written by H. Matsui on Sep., 2007
!
!      subroutine copy_domain_list_from_IO(numnod, internal_node)
!      subroutine copy_domain_list_to_IO(numnod, internal_node)
!
!      subroutine copy_finer_domain_list_from_IO
!
      module copy_domain_list_4_IO
!
      use m_precision
!
      use m_ctl_param_partitioner
      use m_domain_group_4_partition
      use m_subdomain_table_IO
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine copy_domain_list_from_IO(numnod, internal_node)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
!
!
      if (nnod_group_IO .ne. numnod) stop 'check number of node'
!
      if (internod_group_IO .ne. internal_node) then
        stop 'check number of internal node'
      end if
!
      num_domain = nproc_group_IO
      nod_d_grp1%IGROUP(1:internal_node) = IGROUP_IO(1:internal_node)
!
      call deallocate_domain_group_IO
!
      end subroutine copy_domain_list_from_IO
!
!   --------------------------------------------------------------------
!
      subroutine copy_domain_list_to_IO(numnod, internal_node)
!
      integer(kind = kint), intent(in) :: numnod, internal_node
!
!
      nnod_group_IO =     numnod
      internod_group_IO = internal_node
      nproc_group_IO =    num_domain
!
      call allocate_domain_group_IO
!
      IGROUP_IO(1:internal_node) = nod_d_grp1%IGROUP(1:internal_node)
!
      end subroutine copy_domain_list_to_IO
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine copy_finer_domain_list_from_IO(new_node)
!
      use t_geometry_data
!
      type(node_data), intent(inout) :: new_node
!
!
      if(nnod_group_IO .ne. new_node%numnod) stop 'check number of node'
!
      if(internod_group_IO .ne. new_node%internal_node) then
        stop 'check number of internal node'
      end if
!
      num_domain = nproc_group_IO
      nnod_group_finer = new_node%numnod
      call allocate_finer_domain_group
!
      IGROUP_FINER(1:new_node%internal_node)                            &
     &      = IGROUP_IO(1:new_node%internal_node)
!
      call deallocate_domain_group_IO
!
      end subroutine copy_finer_domain_list_from_IO
!
!   --------------------------------------------------------------------
!
      end module copy_domain_list_4_IO

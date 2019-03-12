!generate_comm_tables.f90
!      module generate_comm_tables
!
!     Written by H. Matsui on Sep., 2007
!
!!      subroutine gen_node_import_tables                               &
!!     &         (nprocs, itl_nod_part, nod_d_grp, comm_part)
!!        type(domain_group_4_partition), intent(in) :: nod_d_grp
!!        type(partitioner_comm_tables), intent(inout) :: comm_part
!!      subroutine gen_node_export_tables                               &
!!     &         (nprocs, itl_nod_part, nod_d_grp, comm_part)
!!        type(internal_4_partitioner), intent(in) :: itl_nod_part
!!        type(domain_group_4_partition), intent(inout) :: nod_d_grp
!!        type(partitioner_comm_tables), intent(inout) :: comm_part
!
      module generate_comm_tables
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use t_domain_group_4_partition
      use t_comm_table
      use t_internal_4_partitioner
      use t_partitioner_comm_table
      use cal_minmax_and_stacks
!
      implicit  none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine gen_node_import_tables                                 &
     &         (nprocs, itl_nod_part, nod_d_grp, comm_part)
!
      use set_parallel_file_name
      use const_neighbour_domain
      use const_node_comm_table
      use sel_part_nod_comm_input
      use check_domain_prop_4_part
!
      integer(kind = kint), intent(in) :: nprocs
!
      type(domain_group_4_partition), intent(in) :: nod_d_grp
      type(internal_4_partitioner), intent(in) :: itl_nod_part
!
      type(partitioner_comm_tables), intent(inout) :: comm_part
!
      type(communication_table) :: new_comm
      integer :: ip, id_rank
!
!
      if(comm_part%iflag_memory_conserve .eq. 0) then
        call alloc_nod_comm_tbl_part(nprocs, comm_part)
      end if
!
      call allocate_wk_neib_domain(nprocs)
!
      do ip = 1, nprocs
        id_rank = ip - 1
!
        call count_neib_domain_by_node                                  &
     &     (nod_d_grp, itl_nod_part, ip, nprocs, new_comm%num_neib)
!
        call alloc_neighbouring_id(new_comm)
        call set_neib_domain_by_node(nod_d_grp, itl_nod_part,           &
     &      ip, nprocs, new_comm%num_neib, new_comm%id_neib)
!
        call write_neighboring_pes(ip, new_comm)
!C
!C-- ASSEMBLE IMPORT pointers

        call const_nod_import_table_4_part                              &
     &     (ip, nod_d_grp, itl_nod_part, new_comm)
        call save_node_import_4_part(ip, new_comm, comm_part)
!
        call dealloc_import_table(new_comm)
      end do
!
      call deallocate_wk_neib_domain
!
      end subroutine gen_node_import_tables
!
!   --------------------------------------------------------------------
!
      subroutine gen_node_export_tables                                 &
     &         (nprocs, itl_nod_part, nod_d_grp, comm_part)
!
      use set_parallel_file_name
      use set_local_by_subdomain_tbl
      use const_node_comm_table
      use add_export_item_4_part
      use sel_part_nod_comm_input
!
      integer(kind = kint), intent(in) :: nprocs
      type(internal_4_partitioner), intent(in) :: itl_nod_part
!
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
      type(partitioner_comm_tables), intent(inout) :: comm_part
!
      type(communication_table) :: new_comm
      integer :: ip, id_rank
!
!C
!C +-------------------------------+
!C | update FILE : EXPORT pointers |
!C +-------------------------------+
!C===
      do ip = 1, nprocs
        id_rank = ip - 1
!C
!C-- "marking" with GLOBAL NODE ID

        if(iflag_debug .gt. 0) write(*,*)                               &
     &      'set_local_id_4_export ', id_rank
        call set_local_id_4_export(ip, itl_nod_part, nod_d_grp)
!
        call load_node_import_4_part(ip, comm_part, new_comm)
!
        call alloc_export_num(new_comm)
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &       'count_nod_export_item_4_part ', id_rank
        call count_nod_export_item_4_part(ip, new_comm, comm_part)
        call add_nod_export_item_4_part                                 &
     &     (nprocs, ip, new_comm, comm_part)
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &       's_cal_total_and_stacks ', id_rank
        call s_cal_total_and_stacks                                     &
     &     (new_comm%num_neib, new_comm%num_export, izero,              &
     &      new_comm%istack_export, new_comm%ntot_export)
!
        call alloc_export_item(new_comm)
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &       'set_nod_export_item_4_part ', id_rank
        call set_nod_export_item_4_part                                 &
     &     (ip, nod_d_grp, itl_nod_part, new_comm, comm_part)
!
        call save_node_export_4_part(ip, new_comm, comm_part)
        call dealloc_comm_table(new_comm)
      end do
!
      end subroutine gen_node_export_tables
!
!   --------------------------------------------------------------------
!
      end module generate_comm_tables

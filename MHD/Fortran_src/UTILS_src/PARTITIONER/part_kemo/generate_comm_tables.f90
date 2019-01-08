!generate_comm_tables.f90
!      module generate_comm_tables
!
!     Written by H. Matsui on Sep., 2007
!
!!      subroutine gen_node_import_tables                               &
!!     &         (nprocs, work_f_head, nod_d_grp)
!!        type(domain_group_4_partition), intent(in) :: nod_d_grp
!!      subroutine gen_node_export_tables                               &
!!     &         (nprocs, work_f_head, nod_d_grp)
!!        type(domain_group_4_partition), intent(inout) :: nod_d_grp
!
      module generate_comm_tables
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use t_domain_group_4_partition
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
     &         (nprocs, work_f_head, nod_d_grp)
!
      use t_comm_table
      use m_partitioner_comm_table
      use set_parallel_file_name
      use const_neighbour_domain
      use const_node_comm_table
      use sel_part_nod_comm_input
      use check_domain_prop_4_part
!
      integer(kind = kint), intent(in) :: nprocs
      character(len=kchara), intent(in) :: work_f_head
!
      type(domain_group_4_partition), intent(in) :: nod_d_grp
!
      type(communication_table) :: new_comm
      integer(kind = kint) :: ip, my_rank
!
!
      if(iflag_memory_conserve .eq. 0) then
        call alloc_nod_comm_tbl_part(nprocs)
      end if
!
      call allocate_wk_neib_domain(nprocs)
!
      do ip = 1, nprocs
        my_rank = ip - 1
!
        call count_neib_domain_by_node                                  &
     &     (nod_d_grp, ip, nprocs, new_comm%num_neib)
!
        call allocate_type_neib_id(new_comm)
        call set_neib_domain_by_node                                    &
     &     (nod_d_grp, ip, nprocs, new_comm%num_neib, new_comm%id_neib)
!
        call write_neighboring_pes(ip, new_comm)
!C
!C-- ASSEMBLE IMPORT pointers

        call const_nod_import_table_4_part(ip, nod_d_grp, new_comm)
        call save_node_import_4_part(ip, work_f_head, new_comm)
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
     &         (nprocs, work_f_head, nod_d_grp)
!
      use t_comm_table
      use m_partitioner_comm_table
      use set_parallel_file_name
      use set_local_by_subdomain_tbl
      use const_node_comm_table
      use add_export_item_4_part
      use sel_part_nod_comm_input
!
      integer(kind = kint), intent(in) :: nprocs
      character(len=kchara), intent(in) :: work_f_head
!
      type(domain_group_4_partition), intent(inout) :: nod_d_grp
!
      type(communication_table) :: new_comm
      integer(kind = kint) :: ip, my_rank
!
!C
!C +-------------------------------+
!C | update FILE : EXPORT pointers |
!C +-------------------------------+
!C===
      do ip = 1, nprocs
        my_rank = ip - 1
!C
!C-- "marking" with GLOBAL NODE ID

        if(iflag_debug .gt. 0) write(*,*)                               &
     &      'set_local_node_4_export ', my_rank
        call set_local_node_4_export(ip, nod_d_grp)
!
        call load_node_import_4_part(ip, work_f_head, new_comm)
!
        call allocate_type_export_num(new_comm)
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &       'count_nod_export_item_4_part ', my_rank
        call count_nod_export_item_4_part(ip, work_f_head, new_comm)
        call add_nod_export_item_4_part                                 &
     &     (nprocs, ip, work_f_head, new_comm)
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &       's_cal_total_and_stacks ', my_rank
        call s_cal_total_and_stacks                                     &
     &     (new_comm%num_neib, new_comm%num_export, izero,              &
     &      new_comm%istack_export, new_comm%ntot_export)
!
        call allocate_type_export_item(new_comm)
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &       'set_nod_export_item_4_part ', my_rank
        call set_nod_export_item_4_part                                 &
     &     (ip, work_f_head, nod_d_grp, new_comm)
!
        call save_node_export_4_part(ip, work_f_head, new_comm)
        call dealloc_comm_table(new_comm)
      end do
!
      end subroutine gen_node_export_tables
!
!   --------------------------------------------------------------------
!
      end module generate_comm_tables

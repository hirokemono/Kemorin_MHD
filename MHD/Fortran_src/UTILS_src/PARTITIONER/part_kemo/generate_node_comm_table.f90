!generate_node_comm_table.f90
!      module generate_node_comm_table
!
!     Written by H. Matsui on Sep., 2007
!
!      subroutine gen_node_import_tables(nprocs, work_f_head)
!      subroutine gen_node_export_tables(nprocs, work_f_head)
!
      module generate_node_comm_table
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
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
      subroutine gen_node_import_tables(nprocs, work_f_head)
!
      use m_2nd_nod_comm_table
      use m_partitioner_comm_table
      use set_parallel_file_name
      use const_neighbour_domain
      use const_node_comm_table
      use sel_part_nod_comm_input
      use check_domain_prop_4_part
!
      integer(kind = kint), intent(in) :: nprocs
      character(len=kchara), intent(in) :: work_f_head
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
        call count_neib_domain_by_node(ip, nprocs, num_neib_2)
!
        call allocate_2nd_neib_id
        call set_neib_domain_by_node(ip, nprocs, num_neib_2, id_neib_2)
!
        call write_neighboring_pes(ip)
!C
!C-- ASSEMBLE IMPORT pointers

        call const_nod_import_table_4_part(ip)
        call save_node_import_4_part(ip, work_f_head)
!
      end do
!
      call deallocate_wk_neib_domain
!
      end subroutine gen_node_import_tables
!
!   --------------------------------------------------------------------
!
      subroutine gen_node_export_tables(nprocs, work_f_head)
!
      use m_2nd_nod_comm_table
      use m_partitioner_comm_table
      use set_parallel_file_name
      use set_local_by_subdomain_tbl
      use const_node_comm_table
      use add_node_export_item_4_part
      use sel_part_nod_comm_input
!
      integer(kind = kint), intent(in) :: nprocs
      character(len=kchara), intent(in) :: work_f_head
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
        call set_local_node_4_export(ip)
!
        call load_node_import_4_part(ip, work_f_head)
!
        call allocate_2nd_nod_export_num
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &       'count_nod_export_item_4_part ', my_rank
        call count_nod_export_item_4_part(ip, work_f_head)
        call add_nod_export_item_4_part(nprocs, ip, work_f_head)
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &       's_cal_total_and_stacks ', my_rank
        call s_cal_total_and_stacks(num_neib_2, num_export_2, izero,    &
     &      istack_export_2, ntot_export_2)
!
        call allocate_2nd_nod_export_item
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &       'set_nod_export_item_4_part ', my_rank
        call set_nod_export_item_4_part(ip, work_f_head)
!
        call save_node_export_4_part(ip, work_f_head)
      end do
!
      end subroutine gen_node_export_tables
!
!   --------------------------------------------------------------------
!
      end module generate_node_comm_table

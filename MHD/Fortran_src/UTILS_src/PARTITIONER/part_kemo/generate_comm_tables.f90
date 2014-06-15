!generate_comm_tables.f90
!      module generate_comm_tables
!
!     Written by H. Matsui on Sep., 2007
!
!      subroutine gen_all_import_tables(nprocs, work_f_head)
!      subroutine gen_all_export_tables(nprocs, work_f_head)
!
      module generate_comm_tables
!
      use m_precision
!
      use m_constants
      use work_comm_table_IO
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
      subroutine gen_all_import_tables(nprocs, work_f_head)
!
      use m_2nd_geometry_data
      use m_partitioner_comm_table
      use set_parallel_file_name
      use const_neighbour_domain
      use const_all_comm_tables
      use sel_part_comm_tbl_input
      use check_domain_prop_4_part
!
      integer(kind = kint), intent(in) :: nprocs
      character(len=kchara), intent(in) :: work_f_head
      integer(kind = kint) :: ip, my_rank
!
!
      if(iflag_memory_conserve .eq. 0) then
        call alloc_all_comm_tbl_part(nprocs)
      end if
!
      call allocate_wk_neib_domain(nprocs)
!
      do ip = 1, nprocs
        my_rank = ip - 1
!
        call count_neib_domain_by_node(ip, nprocs, comm_2nd%num_neib)
!
        call allocate_type_neib_id(comm_2nd)
        call set_neib_domain_by_node(ip, nprocs, comm_2nd%num_neib, comm_2nd%id_neib)
!
        ele_comm_2nd%num_neib = comm_2nd%num_neib
        surf_comm_2nd%num_neib = comm_2nd%num_neib
        edge_comm_2nd%num_neib = comm_2nd%num_neib
!
        call allocate_type_neib_id(ele_comm_2nd)
        call allocate_type_neib_id(surf_comm_2nd)
        call allocate_type_neib_id(edge_comm_2nd)
!
        ele_comm_2nd%id_neib(1:comm_2nd%num_neib) =  comm_2nd%id_neib(1:comm_2nd%num_neib)
        surf_comm_2nd%id_neib(1:comm_2nd%num_neib) = comm_2nd%id_neib(1:comm_2nd%num_neib)
        edge_comm_2nd%id_neib(1:comm_2nd%num_neib) = comm_2nd%id_neib(1:comm_2nd%num_neib)
!
        call write_neighboring_pes(ip)
!C
!C-- ASSEMBLE IMPORT pointers

        call const_all_import_tbl_part(ip, nprocs)
        call save_all_import_4_part(ip, work_f_head)
!
      end do
!
      call deallocate_wk_neib_domain
!
      end subroutine gen_all_import_tables
!
!   --------------------------------------------------------------------
!
      subroutine gen_all_export_tables(nprocs, work_f_head)
!
      use m_2nd_geometry_data
      use m_partitioner_comm_table
      use set_parallel_file_name
      use set_local_by_subdomain_tbl
      use const_all_comm_tables
      use add_export_item_4_part
      use sel_part_comm_tbl_input
!
      integer(kind = kint), intent(in) :: nprocs
      character(len=kchara), intent(in) :: work_f_head
      integer(kind = kint) :: ip
!
!C
!C +-------------------------------+
!C | update FILE : EXPORT pointers |
!C +-------------------------------+
!C===
      do ip= 1, nprocs
!
!C-- "marking" with GLOBAL NODE ID

        call set_local_node_4_export(ip)
        call set_local_element_4_export(ip)
        call set_local_surface_4_export(ip)
        call set_local_edge_4_export(ip)
!
        call load_all_import_4_part(ip, work_f_head)
!
        call allocate_type_export_num(comm_2nd)
        call allocate_type_export_num(ele_comm_2nd)
        call allocate_type_export_num(surf_comm_2nd)
        call allocate_type_export_num(edge_comm_2nd)
!
        call count_all_export_item_4_part(ip, work_f_head)
        call add_all_export_item_4_part(nprocs, ip, work_f_head)
!
        call s_cal_total_and_stacks(comm_2nd%num_neib, comm_2nd%num_export,           &
     &      izero, comm_2nd%istack_export, comm_2nd%ntot_export)
        call s_cal_total_and_stacks(ele_comm_2nd%num_neib, ele_comm_2nd%num_export,   &
     &      izero, ele_comm_2nd%istack_export, ele_comm_2nd%ntot_export)
        call s_cal_total_and_stacks                                     &
     &     (surf_comm_2nd%num_neib, surf_comm_2nd%num_export,           &
     &      izero, surf_comm_2nd%istack_export, surf_comm_2nd%ntot_export)
        call s_cal_total_and_stacks(comm_2nd%num_neib, edge_comm_2nd%num_export, &
     &      izero, edge_comm_2nd%istack_export, edge_comm_2nd%ntot_export)
!
        call allocate_type_export_item(comm_2nd)
        call allocate_type_export_item(ele_comm_2nd)
        call allocate_type_export_item(surf_comm_2nd)
        call allocate_type_export_item(edge_comm_2nd)
!
        call set_all_export_item_4_part(ip, work_f_head)
!
        call save_all_export_4_part(ip, work_f_head)
      end do
!
      end subroutine gen_all_export_tables
!
!   --------------------------------------------------------------------
!
      end module generate_comm_tables

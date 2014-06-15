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
      use m_2nd_nod_comm_table
      use m_2nd_ele_comm_table
      use m_2nd_surf_comm_table
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
        call count_neib_domain_by_node(ip, nprocs, num_neib_2)
!
        call allocate_2nd_neib_id
        call set_neib_domain_by_node(ip, nprocs, num_neib_2, id_neib_2)
!
        num_neib_ele_2 = num_neib_2
        num_neib_surf_2 = num_neib_2
        edge_comm_2nd%num_neib = num_neib_2
!
        call allocate_2nd_ele_neib_id
        call allocate_2nd_surf_neib_id
        call allocate_type_neib_id(edge_comm_2nd)
!
        id_neib_ele_2(1:num_neib_2) =  id_neib_2(1:num_neib_2)
        id_neib_surf_2(1:num_neib_2) = id_neib_2(1:num_neib_2)
        edge_comm_2nd%id_neib(1:num_neib_2) = id_neib_2(1:num_neib_2)
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
      use m_2nd_nod_comm_table
      use m_2nd_ele_comm_table
      use m_2nd_surf_comm_table
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
        call allocate_2nd_nod_export_num
        call allocate_2nd_ele_export_num
        call allocate_2nd_surf_export_num
        call allocate_type_export_num(edge_comm_2nd)
!
        call count_all_export_item_4_part(ip, work_f_head)
        call add_all_export_item_4_part(nprocs, ip, work_f_head)
!
        call s_cal_total_and_stacks(num_neib_2, num_export_2,           &
     &      izero, istack_export_2, ntot_export_2)
        call s_cal_total_and_stacks(num_neib_ele_2, num_export_ele_2,   &
     &      izero, istack_export_ele_2, ntot_export_ele_2)
        call s_cal_total_and_stacks(num_neib_surf_2, num_export_surf_2, &
     &      izero, istack_export_surf_2, ntot_export_surf_2)
        call s_cal_total_and_stacks(num_neib_2, edge_comm_2nd%num_export, &
     &      izero, edge_comm_2nd%istack_export, edge_comm_2nd%ntot_export)
!
        call allocate_2nd_nod_export_item
        call allocate_2nd_ele_export_item
        call allocate_2nd_surf_export_item
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

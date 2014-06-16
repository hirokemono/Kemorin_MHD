!generate_comm_tables.f90
!      module generate_comm_tables
!
!     Written by H. Matsui on Sep., 2007
!
!      subroutine gen_all_import_tables(nprocs, work_f_head,            &
!     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!      subroutine gen_all_export_tables(nprocs, work_f_head,            &
!     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
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
      subroutine gen_all_import_tables(nprocs, work_f_head,             &
     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
      use t_comm_table
      use m_partitioner_comm_table
      use set_parallel_file_name
      use const_neighbour_domain
      use const_all_comm_tables
      use sel_part_comm_tbl_input
      use check_domain_prop_4_part
!
      integer(kind = kint), intent(in) :: nprocs
      character(len=kchara), intent(in) :: work_f_head
      type(communication_table), intent(inout) :: new_comm
      type(communication_table), intent(inout) :: new_ele_comm
      type(communication_table), intent(inout) :: new_surf_comm
      type(communication_table), intent(inout) :: new_edge_comm
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
        call count_neib_domain_by_node(ip, nprocs, new_comm%num_neib)
!
        call allocate_type_neib_id(new_comm)
        call set_neib_domain_by_node                                    &
     &      (ip, nprocs, new_comm%num_neib, new_comm%id_neib)
!
        new_ele_comm%num_neib = new_comm%num_neib
        new_surf_comm%num_neib = new_comm%num_neib
        new_edge_comm%num_neib = new_comm%num_neib
!
        call allocate_type_neib_id(new_ele_comm)
        call allocate_type_neib_id(new_surf_comm)
        call allocate_type_neib_id(new_edge_comm)
!
        new_ele_comm%id_neib(1:new_comm%num_neib)                       &
     &        =  new_comm%id_neib(1:new_comm%num_neib)
        new_surf_comm%id_neib(1:new_comm%num_neib)                      &
     &        = new_comm%id_neib(1:new_comm%num_neib)
        new_edge_comm%id_neib(1:new_comm%num_neib)                      &
     &        = new_comm%id_neib(1:new_comm%num_neib)
!
        call write_neighboring_pes(ip, new_comm)
!C
!C-- ASSEMBLE IMPORT pointers

        call const_all_import_tbl_part(ip, nprocs,                      &
     &      new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
        call save_all_import_4_part(ip, work_f_head,                    &
     &      new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
      end do
!
      call deallocate_wk_neib_domain
!
      end subroutine gen_all_import_tables
!
!   --------------------------------------------------------------------
!
      subroutine gen_all_export_tables(nprocs, work_f_head,             &
     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
      use t_comm_table
      use m_partitioner_comm_table
      use set_parallel_file_name
      use set_local_by_subdomain_tbl
      use const_all_comm_tables
      use add_export_item_4_part
      use sel_part_comm_tbl_input
!
      integer(kind = kint), intent(in) :: nprocs
      character(len=kchara), intent(in) :: work_f_head
      type(communication_table), intent(inout) :: new_comm
      type(communication_table), intent(inout) :: new_ele_comm
      type(communication_table), intent(inout) :: new_surf_comm
      type(communication_table), intent(inout) :: new_edge_comm
!
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
        call load_all_import_4_part(ip, work_f_head,                    &
     &      new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
        call allocate_type_export_num(new_comm)
        call allocate_type_export_num(new_ele_comm)
        call allocate_type_export_num(new_surf_comm)
        call allocate_type_export_num(new_edge_comm)
!
        call count_all_export_item_4_part(ip, work_f_head,              &
     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
        call add_all_export_item_4_part(nprocs, ip, work_f_head,        &
     &          new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
        call s_cal_total_and_stacks                                     &
     &     (new_comm%num_neib, new_comm%num_export,                     &
     &       izero, new_comm%istack_export, new_comm%ntot_export)
        call s_cal_total_and_stacks                                     &
     &     (new_ele_comm%num_neib, new_ele_comm%num_export, izero,      &
     &      new_ele_comm%istack_export, new_ele_comm%ntot_export)
        call s_cal_total_and_stacks                                     &
     &     (new_surf_comm%num_neib, new_surf_comm%num_export,           &
     &      izero, new_surf_comm%istack_export,                         &
     &      new_surf_comm%ntot_export)
        call s_cal_total_and_stacks                                     &
     &     (new_comm%num_neib, new_edge_comm%num_export, izero,         &
     &      new_edge_comm%istack_export, new_edge_comm%ntot_export)
!
        call allocate_type_export_item(new_comm)
        call allocate_type_export_item(new_ele_comm)
        call allocate_type_export_item(new_surf_comm)
        call allocate_type_export_item(new_edge_comm)
!
        call set_all_export_item_4_part(ip, work_f_head,                &
     &      new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
        call save_all_export_4_part(ip, work_f_head,                    &
     &      new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
      end do
!
      end subroutine gen_all_export_tables
!
!   --------------------------------------------------------------------
!
      end module generate_comm_tables

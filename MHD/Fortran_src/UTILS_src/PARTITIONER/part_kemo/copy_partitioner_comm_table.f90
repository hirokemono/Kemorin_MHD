!copy_partitioner_comm_table.f90
!     module copy_partitioner_comm_table
!
!      Written by H. Matsui on Sep., 2007
!
!      subroutine copy_all_import_to_mem                                &
!     &    (ip, new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!      subroutine copy_all_import_from_mem                              &
!     &    (ip, new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
!      subroutine copy_all_export_to_mem                                &
!     &    (ip, new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!      subroutine copy_all_export_from_mem                              &
!     &    (ip, new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
!      subroutine copy_all_import_num_tmp                               &
!     &    (ip, new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!      subroutine copy_all_import_item_tmp                              &
!     &    (ip, new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
      module copy_partitioner_comm_table
!
      use m_precision
!
      use t_comm_table
      use m_partitioner_comm_table
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine copy_all_import_to_mem                                 &
     &    (ip, new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
      use copy_part_nod_comm_tbl
!
      integer(kind = kint),  intent(in) :: ip 
!
      type(communication_table), intent(in) :: new_comm
      type(communication_table), intent(in) :: new_ele_comm
      type(communication_table), intent(in) :: new_surf_comm
      type(communication_table), intent(in) :: new_edge_comm
!
!
      ele_comm_tbl_part(ip)%num_neib = new_comm%num_neib
      ele_comm_tbl_part(ip)%ntot_import = new_ele_comm%ntot_import
!
      surf_comm_tbl_part(ip)%num_neib = new_comm%num_neib
      surf_comm_tbl_part(ip)%ntot_import = new_surf_comm%ntot_import
!
      edge_comm_tbl_part(ip)%num_neib = new_comm%num_neib
      edge_comm_tbl_part(ip)%ntot_import = new_edge_comm%ntot_import
!
      call allocate_type_neib_id( ele_comm_tbl_part(ip) )
      call allocate_type_import_num( ele_comm_tbl_part(ip) )
!
      call allocate_type_neib_id( surf_comm_tbl_part(ip) )
      call allocate_type_import_num( surf_comm_tbl_part(ip) )
!
      call allocate_type_neib_id( edge_comm_tbl_part(ip) )
      call allocate_type_import_num( edge_comm_tbl_part(ip) )
!
      ele_comm_tbl_part(ip)%id_neib(1:new_comm%num_neib)                &
     &       = new_comm%id_neib(1:new_comm%num_neib)
      ele_comm_tbl_part(ip)%istack_import(0:new_comm%num_neib)          &
     &       = new_ele_comm%num_import(0:new_comm%num_neib)
!
      surf_comm_tbl_part(ip)%id_neib(1:new_comm%num_neib)               &
     &       = new_comm%id_neib(1:new_comm%num_neib)
      surf_comm_tbl_part(ip)%istack_import(0:new_comm%num_neib)         &
     &       = new_surf_comm%istack_import(0:new_comm%num_neib)
!
      edge_comm_tbl_part(ip)%id_neib(1:new_comm%num_neib)               &
     &       = new_comm%id_neib(1:new_comm%num_neib)
      edge_comm_tbl_part(ip)%istack_import(0:new_comm%num_neib)         &
     &       = new_edge_comm%istack_import(0:new_comm%num_neib)
!
      call allocate_type_import_item( ele_comm_tbl_part(ip) )
      call allocate_type_import_item( surf_comm_tbl_part(ip) )
      call allocate_type_import_item( edge_comm_tbl_part(ip) )
!
      ele_comm_tbl_part(ip)%item_import(1:new_ele_comm%ntot_import)            &
     &       = new_ele_comm%item_import(1:new_ele_comm%ntot_import)
      surf_comm_tbl_part(ip)%item_import(1:new_surf_comm%ntot_import)   &
     &       = new_surf_comm%item_import(1:new_surf_comm%ntot_import)
      edge_comm_tbl_part(ip)%item_import(1:new_edge_comm%ntot_import)   &
     &       = new_edge_comm%item_import(1:new_edge_comm%ntot_import)
!
      call copy_node_import_to_mem(ip, new_comm)
!
      end subroutine copy_all_import_to_mem
!
!------------------------------------------------------------------
!
      subroutine copy_all_import_from_mem                               &
     &    (ip, new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
      use copy_part_nod_comm_tbl
!
      integer(kind = kint),  intent(in) :: ip 
      type(communication_table), intent(inout) :: new_comm
      type(communication_table), intent(inout) :: new_ele_comm
      type(communication_table), intent(inout) :: new_surf_comm
      type(communication_table), intent(inout) :: new_edge_comm
      integer(kind = kint) :: i
!
!
      call copy_node_import_from_mem(ip, new_comm)
!
      new_ele_comm%num_neib =  new_comm%num_neib
      new_surf_comm%num_neib = new_comm%num_neib
      new_edge_comm%num_neib = new_comm%num_neib
!
      new_ele_comm%ntot_import =  ele_comm_tbl_part(ip)%ntot_import
      new_surf_comm%ntot_import = surf_comm_tbl_part(ip)%ntot_import
      new_edge_comm%ntot_import = edge_comm_tbl_part(ip)%ntot_import
!
      call allocate_type_neib_id(new_ele_comm)
      call allocate_type_neib_id(new_surf_comm)
      call allocate_type_neib_id(new_edge_comm)
!
      call allocate_type_import_num(new_ele_comm)
      call allocate_type_import_num(new_surf_comm)
      call allocate_type_import_num(new_edge_comm)
!
      new_ele_comm%id_neib(1:new_comm%num_neib)                         &
     &       = ele_comm_tbl_part(ip)%id_neib(1:new_comm%num_neib)
      new_ele_comm%num_import(0:new_comm%num_neib)                      &
     &       = ele_comm_tbl_part(ip)%istack_import(0:new_comm%num_neib)
!
      new_surf_comm%id_neib(1:new_comm%num_neib)                         &
     &       = surf_comm_tbl_part(ip)%id_neib(1:new_comm%num_neib)
      new_surf_comm%istack_import(0:new_comm%num_neib)                  &
     &       = surf_comm_tbl_part(ip)%istack_import(0:new_comm%num_neib)
!
      new_edge_comm%id_neib(1:new_comm%num_neib)                        &
     &       = edge_comm_tbl_part(ip)%id_neib(1:new_comm%num_neib)
      new_edge_comm%istack_import(0:new_comm%num_neib)                  &
     &       = edge_comm_tbl_part(ip)%istack_import(0:new_comm%num_neib)
!
      do i = 1, new_comm%num_neib
          new_ele_comm%num_import(i) = new_ele_comm%num_import(i)       &
     &                          - new_ele_comm%num_import(i-1)
          new_surf_comm%num_import(i) = new_surf_comm%istack_import(i)  &
     &                          - new_surf_comm%istack_import(i-1)
          new_edge_comm%num_import(i) = new_edge_comm%istack_import(i)  &
     &                          - new_edge_comm%istack_import(i-1)
      end do
!
      call allocate_type_import_item(new_ele_comm)
      call allocate_type_import_item(new_surf_comm)
      call allocate_type_import_item(new_edge_comm)
!
      new_ele_comm%item_import(1:new_ele_comm%ntot_import)              &
     & =ele_comm_tbl_part(ip)%item_import(1:new_ele_comm%ntot_import)
      new_surf_comm%item_import(1:new_surf_comm%ntot_import)            &
     & =surf_comm_tbl_part(ip)%item_import(1:new_surf_comm%ntot_import)
      new_edge_comm%item_import(1:new_edge_comm%ntot_import)            &
     & =edge_comm_tbl_part(ip)%item_import(1:new_edge_comm%ntot_import)
!
      end subroutine copy_all_import_from_mem
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_all_export_to_mem                                 &
     &    (ip, new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
      use copy_part_nod_comm_tbl
!
      integer(kind = kint),  intent(in) :: ip 
      type(communication_table), intent(in) :: new_comm
      type(communication_table), intent(in) :: new_ele_comm
      type(communication_table), intent(in) :: new_surf_comm
      type(communication_table), intent(in) :: new_edge_comm
!
!
      ele_comm_tbl_part(ip)%ntot_export =  new_ele_comm%ntot_export
      surf_comm_tbl_part(ip)%ntot_export = new_surf_comm%ntot_export
      edge_comm_tbl_part(ip)%ntot_export = new_edge_comm%ntot_export
!
      call allocate_type_export_num( ele_comm_tbl_part(ip) )
      call allocate_type_export_num( surf_comm_tbl_part(ip) )
      call allocate_type_export_num( edge_comm_tbl_part(ip) )
!
      ele_comm_tbl_part(ip)%istack_export(0:new_comm%num_neib)          &
     &       = new_ele_comm%istack_export(0:new_comm%num_neib)
      surf_comm_tbl_part(ip)%istack_export(0:new_comm%num_neib)         &
     &       = new_surf_comm%istack_export(0:new_comm%num_neib)
      edge_comm_tbl_part(ip)%istack_export(0:new_comm%num_neib)         &
     &       = new_edge_comm%istack_export(0:new_comm%num_neib)
!
      call allocate_type_export_item( ele_comm_tbl_part(ip) )
      call allocate_type_export_item( surf_comm_tbl_part(ip) )
      call allocate_type_export_item( edge_comm_tbl_part(ip) )
!
      ele_comm_tbl_part(ip)%item_export(1:new_ele_comm%ntot_export)     &
     &       = new_ele_comm%item_export(1:new_ele_comm%ntot_export)
      surf_comm_tbl_part(ip)%item_export(1:new_surf_comm%ntot_export)   &
     &       = new_surf_comm%item_export(1:new_surf_comm%ntot_export)
      edge_comm_tbl_part(ip)%item_export(1:new_edge_comm%ntot_export)   &
     &       = new_edge_comm%item_export(1:new_edge_comm%ntot_export)
!
      call copy_node_export_to_mem(ip, new_comm)
!
      end subroutine copy_all_export_to_mem
!
!------------------------------------------------------------------
!
      subroutine copy_all_export_from_mem                               &
     &    (ip, new_comm, new_ele_comm, new_surf_comm, new_edge_comm)
!
      use copy_part_nod_comm_tbl
!
      integer(kind = kint),  intent(in) :: ip 
      type(communication_table), intent(inout) :: new_comm
      type(communication_table), intent(inout) :: new_ele_comm
      type(communication_table), intent(inout) :: new_surf_comm
      type(communication_table), intent(inout) :: new_edge_comm
!
      integer(kind = kint) :: i
!
!
      call copy_node_export_from_mem(ip, new_comm)
!
      new_ele_comm%ntot_export =  ele_comm_tbl_part(ip)%ntot_export
      new_surf_comm%ntot_export = surf_comm_tbl_part(ip)%ntot_export
      new_edge_comm%ntot_export = edge_comm_tbl_part(ip)%ntot_export
!
      call allocate_type_export_num(new_ele_comm)
      call allocate_type_export_num(new_surf_comm)
      call allocate_type_export_num(new_edge_comm)
!
      new_ele_comm%istack_export(0:new_comm%num_neib)                   &
     &     = ele_comm_tbl_part(ip)%istack_export(0:new_comm%num_neib)
      new_surf_comm%istack_export(0:new_comm%num_neib)                  &
     &     = surf_comm_tbl_part(ip)%istack_export(0:new_comm%num_neib)
      new_edge_comm%istack_export(0:new_comm%num_neib)                  &
     &     = edge_comm_tbl_part(ip)%istack_export(0:new_comm%num_neib)
!
      do i = 1, new_comm%num_neib
          new_ele_comm%num_export(i) = new_ele_comm%istack_export(i)    &
     &                          - new_ele_comm%istack_export(i-1)
          new_surf_comm%num_export(i) = new_surf_comm%istack_export(i)  &
     &                          - new_surf_comm%istack_export(i-1)
          new_edge_comm%num_export(i) = new_edge_comm%istack_export(i)  &
     &                          - new_edge_comm%istack_export(i-1)
      end do
!
      call allocate_type_export_item(new_ele_comm)
      call allocate_type_export_item(new_surf_comm)
      call allocate_type_export_item(new_edge_comm)
!
      new_ele_comm%item_export(1:new_ele_comm%ntot_export)              &
     & =ele_comm_tbl_part(ip)%item_export(1:new_ele_comm%ntot_export)
      new_surf_comm%item_export(1:new_surf_comm%ntot_export)            &
     & =surf_comm_tbl_part(ip)%item_export(1:new_surf_comm%ntot_export)
      new_edge_comm%item_export(1:new_edge_comm%ntot_export)            &
     & =edge_comm_tbl_part(ip)%item_export(1:new_edge_comm%ntot_export)
!
      end subroutine copy_all_export_from_mem
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_all_import_num_tmp(ip)
!
      use work_comm_table_IO
!
      integer(kind = kint), intent(in) :: ip
!
!
      NP_TMP = nod_comm_tbl_part(ip)%num_neib
!
      call allocate_all_import_num_tmp
!
      NEIB_TMP(1:NP_TMP) = nod_comm_tbl_part(ip)%id_neib(1:NP_TMP)
      ISTACK_NOD_TMP(0:NP_TMP)                                          &
     &        =  nod_comm_tbl_part(ip)%istack_import(0:NP_TMP)
      ISTACK_ELE_TMP(0:NP_TMP)                                          &
     &        =  ele_comm_tbl_part(ip)%istack_import(0:NP_TMP)
      ISTACK_SURF_TMP(0:NP_TMP)                                         &
     &        = surf_comm_tbl_part(ip)%istack_import(0:NP_TMP)
      ISTACK_EDGE_TMP(0:NP_TMP)                                         &
     &        = edge_comm_tbl_part(ip)%istack_import(0:NP_TMP)
!
      end subroutine copy_all_import_num_tmp
!
!   --------------------------------------------------------------------
!
      subroutine copy_all_import_item_tmp(ip)
!
      use work_comm_table_IO
!
      integer(kind = kint), intent(in) :: ip
!
!
      call allocate_all_import_item_tmp
!
      IMPORT_NOD_TMP(1:NTOT_NOD_TMP)                                    &
     &        =  nod_comm_tbl_part(ip)%item_import(1:NTOT_NOD_TMP)
      IMPORT_ELE_TMP(1:NTOT_ELE_TMP)                                    &
     &        =  ele_comm_tbl_part(ip)%item_import(1:NTOT_ELE_TMP)
      IMPORT_SURF_TMP(1:NTOT_SURF_TMP)                                  &
     &        =  surf_comm_tbl_part(ip)%item_import(1:NTOT_SURF_TMP)
      IMPORT_EDGE_TMP(1:NTOT_EDGE_TMP)                                  &
     &        =  edge_comm_tbl_part(ip)%item_import(1:NTOT_EDGE_TMP)
!
      end subroutine copy_all_import_item_tmp
!
!   --------------------------------------------------------------------
!
      end module copy_partitioner_comm_table

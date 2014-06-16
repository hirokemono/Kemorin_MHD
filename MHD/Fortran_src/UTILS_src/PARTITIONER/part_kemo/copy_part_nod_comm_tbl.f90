!copy_part_nod_comm_tbl.f90
!     module copy_part_nod_comm_tbl
!
!      Written by H. Matsui on Sep., 2007
!
!      subroutine copy_node_import_to_mem(ip, new_comm)
!      subroutine copy_node_import_from_mem(ip, new_comm)
!
!      subroutine copy_node_export_to_mem(ip, new_comm)
!      subroutine copy_node_export_from_mem(ip, new_comm)
!
!      subroutine copy_node_import_num_tmp(ip)
!      subroutine copy_node_import_item_tmp(ip)
!
      module copy_part_nod_comm_tbl
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
      subroutine copy_node_import_to_mem(ip, new_comm)
!
      integer(kind = kint),  intent(in) :: ip 
      type(communication_table), intent(in) :: new_comm
!
!
      nod_comm_tbl_part(ip)%num_neib = new_comm%num_neib
      nod_comm_tbl_part(ip)%ntot_import = new_comm%ntot_import
!
      call allocate_type_neib_id( nod_comm_tbl_part(ip) )
      call allocate_type_import_num( nod_comm_tbl_part(ip) )
!
      nod_comm_tbl_part(ip)%id_neib(1:new_comm%num_neib)                &
     &       = new_comm%id_neib(1:new_comm%num_neib)
      nod_comm_tbl_part(ip)%istack_import(0:new_comm%num_neib)          &
     &       = new_comm%istack_import(0:new_comm%num_neib)
!
      call allocate_type_import_item( nod_comm_tbl_part(ip) )
!
      nod_comm_tbl_part(ip)%item_import(1:new_comm%ntot_import)         &
     &       = new_comm%item_import(1:new_comm%ntot_import)
!
      end subroutine copy_node_import_to_mem
!
!------------------------------------------------------------------
!
      subroutine copy_node_import_from_mem(ip, new_comm)
!
      integer(kind = kint),  intent(in) :: ip
      type(communication_table), intent(inout) :: new_comm
      integer(kind = kint) :: i
!
!
      new_comm%num_neib = nod_comm_tbl_part(ip)%num_neib
      new_comm%ntot_import = nod_comm_tbl_part(ip)%ntot_import
!
      call allocate_type_neib_id(new_comm)
      call allocate_type_import_num(new_comm)
!
      new_comm%id_neib(1:new_comm%num_neib)                             &
     &       = nod_comm_tbl_part(ip)%id_neib(1:new_comm%num_neib)
      new_comm%istack_import(0:new_comm%num_neib)                       &
     &       = nod_comm_tbl_part(ip)%istack_import(0:new_comm%num_neib)
!
      do i = 1, new_comm%num_neib
        new_comm%num_import(i) = new_comm%istack_import(i)              &
     &                          - new_comm%istack_import(i-1)
      end do
!
      call allocate_type_import_item(new_comm)
!
      new_comm%item_import(1:new_comm%ntot_import)                      &
     &   = nod_comm_tbl_part(ip)%item_import(1:new_comm%ntot_import)
!
      end subroutine copy_node_import_from_mem
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_node_export_to_mem(ip, new_comm)
!
      integer(kind = kint),  intent(in) :: ip 
      type(communication_table), intent(in) :: new_comm
!
!
      nod_comm_tbl_part(ip)%ntot_export = new_comm%ntot_export
!
      call allocate_type_export_num( nod_comm_tbl_part(ip) )
!
      nod_comm_tbl_part(ip)%istack_export(0:new_comm%num_neib)          &
     &       = new_comm%istack_export(0:new_comm%num_neib)
!
      call allocate_type_export_item( nod_comm_tbl_part(ip) )
!
      nod_comm_tbl_part(ip)%item_export(1:new_comm%ntot_export)         &
     &       = new_comm%item_export(1:new_comm%ntot_export)
!
      end subroutine copy_node_export_to_mem
!
!------------------------------------------------------------------
!
      subroutine copy_node_export_from_mem(ip, new_comm)
!
      integer(kind = kint),  intent(in) :: ip
      type(communication_table), intent(inout) :: new_comm
!
      integer(kind = kint) :: i
!
!
      new_comm%ntot_export = nod_comm_tbl_part(ip)%ntot_export
!
      call allocate_type_export_num(new_comm)
!
      new_comm%istack_export(0:new_comm%num_neib)                       &
     &       = nod_comm_tbl_part(ip)%istack_export(0:new_comm%num_neib)
!
      do i = 1, new_comm%num_neib
        new_comm%num_export(i) = new_comm%istack_export(i)              &
     &                          - new_comm%istack_export(i-1)
      end do
!
      call allocate_type_export_item(new_comm)
!
      new_comm%item_export(1:new_comm%ntot_export)                      &
     &   = nod_comm_tbl_part(ip)%item_export(1:new_comm%ntot_export)
!
      end subroutine copy_node_export_from_mem
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine copy_node_import_num_tmp(ip)
!
      integer(kind = kint), intent(in) :: ip
!
!
      NP_TMP = nod_comm_tbl_part(ip)%num_neib
!
      call allocate_nod_import_num_tmp
!
      NEIB_TMP(1:NP_TMP) = nod_comm_tbl_part(ip)%id_neib(1:NP_TMP)
      ISTACK_NOD_TMP(0:NP_TMP)                                          &
     &        =  nod_comm_tbl_part(ip)%istack_import(0:NP_TMP)
!
      end subroutine copy_node_import_num_tmp
!
!-----------------------------------------------------------------------
!
      subroutine copy_node_import_item_tmp(ip)
!
      integer(kind = kint), intent(in) :: ip
!
!
      call allocate_nod_import_item_tmp
!
      IMPORT_NOD_TMP(1:NTOT_NOD_TMP)                                    &
     &        =  nod_comm_tbl_part(ip)%item_import(1:NTOT_NOD_TMP)
!
      end subroutine copy_node_import_item_tmp
!
!   --------------------------------------------------------------------
!
      end module copy_part_nod_comm_tbl

!copy_edge_comm_tbl_4_type.f90
!      module copy_edge_comm_tbl_4_type
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine copy_edge_comm_tbl_from_type(edge_comm)
!      subroutine copy_edge_comm_tbl_to_type(edge_comm)
!
!      subroutine copy_edge_import_from_type(edge_comm)
!      subroutine copy_edge_import_to_type(edge_comm)
!        type(communication_table), intent(inout) :: edge_comm
!
      module copy_edge_comm_tbl_4_type
!
      use m_precision
!
      use m_edge_comm_table
      use t_comm_table
      use copy_communication_table
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine copy_edge_comm_tbl_from_type(edge_comm)
!
      type(communication_table), intent(in) :: edge_comm
!
!
      num_neib_edge = edge_comm%num_neib
!
      call allocate_edge_neib_id
      call allocate_edge_import_num
      call allocate_edge_export_num
!
      call copy_num_communication(num_neib_edge, id_neib_edge,          &
     &    istack_import_edge, istack_export_edge, ntot_import_edge,     &
     &    ntot_export_edge, edge_comm%id_neib, edge_comm%istack_import, &
     &    edge_comm%istack_export)
      call copy_num_import_export(num_neib_edge, num_import_edge,       &
     &    num_export_edge, istack_import_edge, istack_export_edge)
!
      call allocate_edge_import_item
      call allocate_edge_export_item
!
      call copy_communication_item(ntot_import_edge, ntot_export_edge,  &
     &    item_import_edge, item_export_edge, edge_comm%item_import,    &
     &    edge_comm%item_export)
!
      end subroutine copy_edge_comm_tbl_from_type
!
!-----------------------------------------------------------------------
!
      subroutine copy_edge_comm_tbl_to_type(edge_comm)
!
      type(communication_table), intent(inout) :: edge_comm
!
!
      edge_comm%num_neib = num_neib_edge
!
      call allocate_type_comm_tbl_num(edge_comm)
!
      call copy_num_communication(edge_comm%num_neib,                   &
     &    edge_comm%id_neib, edge_comm%istack_import,                   &
     &    edge_comm%istack_export, edge_comm%ntot_import,               &
     &    edge_comm%ntot_export, id_neib_edge, istack_import_edge,      &
     &    istack_export_edge)
      call copy_num_import_export(edge_comm%num_neib,                   &
     &    edge_comm%num_import, edge_comm%num_export,                   &
     &    edge_comm%istack_import, edge_comm%istack_export)
!
      call allocate_type_comm_tbl_item(edge_comm)
!
      call copy_communication_item(edge_comm%ntot_import,               &
     &    edge_comm%ntot_export, edge_comm%item_import,                 &
     &    edge_comm%item_export, item_import_edge, item_export_edge)
!
      end subroutine copy_edge_comm_tbl_to_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_edge_import_from_type(edge_comm)
!
      type(communication_table), intent(in) :: edge_comm
!
!
      num_neib_edge = edge_comm%num_neib
!
      call allocate_edge_neib_id
      call allocate_edge_import_num
      call allocate_edge_export_num
!
      call copy_num_import(num_neib_edge, id_neib_edge,                 &
     &    istack_import_edge, istack_export_edge, ntot_import_edge,     &
     &    ntot_export_edge, edge_comm%id_neib, edge_comm%istack_import)
      call copy_num_import_export(num_neib_edge, num_import_edge,       &
     &    num_export_edge, istack_import_edge, istack_export_edge)
!
      call allocate_edge_import_item
      call allocate_edge_export_item
!
      call copy_communication_item(ntot_import_edge, ntot_export_edge,  &
     &    item_import_edge, item_export_edge, edge_comm%item_import,    &
     &    edge_comm%item_export)
!
      end subroutine copy_edge_import_from_type
!
!-----------------------------------------------------------------------
!
      subroutine copy_edge_import_to_type(edge_comm)
!
      type(communication_table), intent(inout) :: edge_comm
!
!
      edge_comm%num_neib = num_neib_edge
!
      call allocate_type_comm_tbl_num(edge_comm)
!
      call copy_num_import(edge_comm%num_neib, edge_comm%id_neib,       &
     &    edge_comm%istack_import, edge_comm%istack_export,             &
     &    edge_comm%ntot_import, edge_comm%ntot_export, id_neib_edge,   &
     &    istack_import_edge)
      call copy_num_import_export(edge_comm%num_neib,                   &
     &    edge_comm%num_import, edge_comm%num_export,                   &
     &    edge_comm%istack_import, edge_comm%istack_export)
!
      call allocate_type_comm_tbl_item(edge_comm)
!
      call copy_communication_item(edge_comm%ntot_import,               &
     &    edge_comm%ntot_export, edge_comm%item_import,                 &
     &    edge_comm%item_export, item_import_edge, item_export_edge)
!
      end subroutine copy_edge_import_to_type
!
!-----------------------------------------------------------------------
!
      end module copy_edge_comm_tbl_4_type

!
!      module set_2nd_edge_comm_tbl_4_IO
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine copy_2nd_edge_comm_tbl_from_IO
!      subroutine copy_2nd_edge_comm_tbl_to_IO(my_rank)
!
!      subroutine copy_2nd_edge_import_from_IO
!      subroutine copy_2nd_edge_import_to_IO(my_rank)
!
      module set_2nd_edge_comm_tbl_4_IO
!
      use m_precision
!
      use m_2nd_geometry_data
      use m_comm_data_IO
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
      subroutine copy_2nd_edge_comm_tbl_from_IO
!
      integer(kind = kint) :: i
!
!
      edge_comm_2nd%num_neib = num_neib_domain_IO
!
      call allocate_type_neib_id(edge_comm_2nd)
      call allocate_type_import_num(edge_comm_2nd)
      call allocate_type_export_num(edge_comm_2nd)
!
      call copy_num_communication(edge_comm_2nd%num_neib, edge_comm_2nd%id_neib,  &
     &    edge_comm_2nd%istack_import, edge_comm_2nd%istack_export,  &
     &    edge_comm_2nd%ntot_import, edge_comm_2nd%ntot_export,               &
     &    id_neib_domain_IO, istack_import_IO, istack_export_IO)
      call copy_num_import_export(edge_comm_2nd%num_neib,                      &
     &    edge_comm_2nd%num_import, edge_comm_2nd%num_export,           &
     &    edge_comm_2nd%istack_import, edge_comm_2nd%istack_export)
!
      call allocate_type_import_item(edge_comm_2nd)
      call allocate_type_export_item(edge_comm_2nd)
!
      call copy_communication_item(edge_comm_2nd%ntot_import,           &
     &    edge_comm_2nd%ntot_export, edge_comm_2nd%item_import,       &
     &    edge_comm_2nd%item_export, item_import_IO, item_export_IO)
!
      call deallocate_comm_item_IO
!
      end subroutine copy_2nd_edge_comm_tbl_from_IO
!
!-----------------------------------------------------------------------
!
      subroutine copy_2nd_edge_comm_tbl_to_IO(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      my_rank_IO = my_rank
      num_neib_domain_IO = edge_comm_2nd%num_neib
!
      call allocate_neib_comm_stack_IO
!
      call copy_num_communication(num_neib_domain_IO,                   &
     &    id_neib_domain_IO, istack_import_IO, istack_export_IO,        &
     &    ntot_import_IO, ntot_export_IO,                               &
     &    edge_comm_2nd%id_neib, edge_comm_2nd%istack_import,           &
     &    edge_comm_2nd%istack_export)
!
      call allocate_comm_item_IO
!
      call copy_communication_item(ntot_import_IO, ntot_export_IO,      &
     &    item_import_IO, item_export_IO,                               &
     &    edge_comm_2nd%item_import, edge_comm_2nd%item_export)
!
      end subroutine copy_2nd_edge_comm_tbl_to_IO
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_2nd_edge_import_from_IO
!
      integer(kind = kint) :: i
!
!
      edge_comm_2nd%num_neib = num_neib_domain_IO
!
      call allocate_type_neib_id(edge_comm_2nd)
      call allocate_type_import_num(edge_comm_2nd)
      call allocate_type_export_num(edge_comm_2nd)
!
      call copy_num_import(edge_comm_2nd%num_neib, edge_comm_2nd%id_neib,             &
     &    edge_comm_2nd%istack_import, edge_comm_2nd%istack_export,  &
     &    edge_comm_2nd%ntot_import, edge_comm_2nd%ntot_export,             &
     &    id_neib_domain_IO, istack_import_IO)
      call copy_num_import_export(edge_comm_2nd%num_neib,                      &
     &    edge_comm_2nd%num_import, edge_comm_2nd%num_export,           &
     &    edge_comm_2nd%istack_import, edge_comm_2nd%istack_export)
!
      call allocate_type_import_item(edge_comm_2nd)
      call allocate_type_export_item(edge_comm_2nd)
!
      call copy_communication_item(edge_comm_2nd%ntot_import,    &
     &    edge_comm_2nd%ntot_export, edge_comm_2nd%item_import,   &
     &    edge_comm_2nd%item_export, item_import_IO, item_export_IO)
!
      call deallocate_comm_item_IO
!
      end subroutine copy_2nd_edge_import_from_IO
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_2nd_edge_import_to_IO(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      my_rank_IO = my_rank
      num_neib_domain_IO = edge_comm_2nd%num_neib
!
      call allocate_neib_comm_stack_IO
!
      call copy_num_import(num_neib_domain_IO, id_neib_domain_IO,       &
     &    istack_import_IO, istack_export_IO,                           &
     &    ntot_import_IO, ntot_export_IO,                               &
     &    edge_comm_2nd%id_neib, edge_comm_2nd%istack_import)
!
      call allocate_comm_item_IO
!
      call copy_communication_item(ntot_import_IO, ntot_export_IO,      &
     &    item_import_IO, item_export_IO,                               &
     &    edge_comm_2nd%item_import, edge_comm_2nd%item_export)
!
      end subroutine copy_2nd_edge_import_to_IO
!
!-----------------------------------------------------------------------
!
      end module set_2nd_edge_comm_tbl_4_IO

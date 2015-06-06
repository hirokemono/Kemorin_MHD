!copy_nod_comm_tbl_4_type.f90
!      module copy_nod_comm_tbl_4_type
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine copy_node_comm_tbl_from_type(org_comm)
!      subroutine copy_node_comm_tbl_to_type(new_comm)
!
!      subroutine copy_node_import_from_type(org_comm)
!      subroutine copy_node_import_to_type(new_comm)
!        type(communication_table), intent(in) :: org_comm
!        type(communication_table), intent(inout) :: new_comm
!
      module copy_nod_comm_tbl_4_type
!
      use m_precision
!
      use m_nod_comm_table
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
      subroutine copy_node_comm_tbl_from_type(org_comm)
!
      type(communication_table), intent(in) :: org_comm
!
!
      nod_comm%num_neib = org_comm%num_neib
!
      call allocate_neib_id
      call allocate_nod_import_num
      call allocate_nod_export_num
!
      call copy_num_communication(nod_comm%num_neib, id_neib,                    &
     &    istack_import, istack_export, ntot_import, ntot_export,       &
     &    org_comm%id_neib, org_comm%istack_import,                     &
     &    org_comm%istack_export)
      call copy_num_import_export(nod_comm%num_neib, num_import, num_export,     &
     &    istack_import, istack_export)
!
      call allocate_nod_import_item
      call allocate_nod_export_item
!
      call copy_communication_item(ntot_import, ntot_export,            &
     &    item_import, item_export, org_comm%item_import,               &
     &    org_comm%item_export)
!
      end subroutine copy_node_comm_tbl_from_type
!
!-----------------------------------------------------------------------
!
      subroutine copy_node_comm_tbl_to_type(new_comm)
!
      type(communication_table), intent(inout) :: new_comm
!
!
      new_comm%num_neib = nod_comm%num_neib
!
      call allocate_type_comm_tbl_num(new_comm)
!
      call copy_num_communication(new_comm%num_neib,                   &
     &    new_comm%id_neib, new_comm%istack_import,                    &
     &    new_comm%istack_export, new_comm%ntot_import,                &
     &    new_comm%ntot_export, id_neib, istack_import, istack_export)
      call copy_num_import_export(new_comm%num_neib,                   &
     &    new_comm%num_import, new_comm%num_export,                    &
     &    new_comm%istack_import, new_comm%istack_export)
!
      call allocate_type_comm_tbl_item(new_comm)
!
      call copy_communication_item(new_comm%ntot_import,               &
     &    new_comm%ntot_export, new_comm%item_import,                  &
     &    new_comm%item_export, item_import, item_export)
!
      end subroutine copy_node_comm_tbl_to_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_node_import_from_type(org_comm)
!
      type(communication_table), intent(in) :: org_comm
!
!
      nod_comm%num_neib = org_comm%num_neib
!
      call allocate_neib_id
      call allocate_nod_import_num
      call allocate_nod_export_num
!
      call copy_num_import(nod_comm%num_neib, id_neib,                           &
     &    istack_import, istack_export, ntot_import, ntot_export,       &
     &    org_comm%id_neib, org_comm%istack_import)
      call copy_num_import_export(nod_comm%num_neib, num_import, num_export,     &
     &    istack_import, istack_export)
!
      call allocate_nod_import_item
      call allocate_nod_export_item
!
      call copy_communication_item(ntot_import, ntot_export,            &
     &    item_import, item_export, org_comm%item_import,               &
     &    org_comm%item_export)
!
      end subroutine copy_node_import_from_type
!
!-----------------------------------------------------------------------
!
      subroutine copy_node_import_to_type(new_comm)
!
      type(communication_table), intent(inout) :: new_comm
!
!
      new_comm%num_neib = nod_comm%num_neib
!
      call allocate_type_comm_tbl_num(new_comm)
!
      call copy_num_import(new_comm%num_neib, new_comm%id_neib,        &
     &    new_comm%istack_import, new_comm%istack_export,              &
     &    new_comm%ntot_import, new_comm%ntot_export, id_neib,         &
     &    istack_import)
      call copy_num_import_export(new_comm%num_neib,                   &
     &    new_comm%num_import, new_comm%num_export,                    &
     &    new_comm%istack_import, new_comm%istack_export)
!
      call allocate_type_comm_tbl_item(new_comm)
!
      call copy_communication_item(new_comm%ntot_import,               &
     &    new_comm%ntot_export, new_comm%item_import,                  &
     &    new_comm%item_export, item_import, item_export)
!
      end subroutine copy_node_import_to_type
!
!-----------------------------------------------------------------------
!
      end module copy_nod_comm_tbl_4_type

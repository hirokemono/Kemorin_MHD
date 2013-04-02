!copy_comm_tables_to_2nd.f90
!      module copy_comm_tables_to_2nd
!
!     Written by H. Matsui on Mar., 2008
!
!      subroutine copy_nod_comm_table_2_2nd
!      subroutine copy_nod_comm_table_from_2nd
!
      module copy_comm_tables_to_2nd
!
      use m_precision
!
      implicit  none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine copy_nod_comm_table_2_2nd
!
      use m_nod_comm_table
      use m_2nd_nod_comm_table
      use copy_communication_table
!
      num_neib_2 = num_neib
      call allocate_2nd_neib_id
      call allocate_2nd_nod_import_num
      call allocate_2nd_nod_export_num
!
      call copy_num_communication(num_neib_2, id_neib_2,                &
     &    istack_import_2, istack_export_2,                             &
     &    ntot_import_2, ntot_export_2,                                 &
     &    id_neib, istack_import, istack_export)
      call copy_num_import_export(num_neib_2,                           &
     &    num_import_2, num_export_2, istack_import_2, istack_export_2)
!
      call allocate_2nd_nod_import_item
      call allocate_2nd_nod_export_item
!
      call copy_communication_item(ntot_import_2, ntot_export_2,        &
     &    item_import_2, item_export_2, item_import, item_export)
!
      end subroutine copy_nod_comm_table_2_2nd
!
!------------------------------------------------------------------
!
      subroutine copy_nod_comm_table_from_2nd
!
      use m_nod_comm_table
      use m_2nd_nod_comm_table
      use copy_communication_table
!
      num_neib = num_neib_2
      call allocate_neib_id
      call allocate_nod_import_num
      call allocate_nod_export_num
!
      call copy_num_communication(num_neib, id_neib,                    &
     &    istack_import, istack_export, ntot_import, ntot_export,       &
     &    id_neib_2, istack_import_2, istack_export_2)
      call copy_num_import_export(num_neib,                             &
     &    num_import, num_export, istack_import, istack_export)
!
      call allocate_nod_import_item
      call allocate_nod_export_item
!
      call copy_communication_item(ntot_import, ntot_export,            &
     &    item_import, item_export, item_import_2, item_export_2)
!
      end subroutine copy_nod_comm_table_from_2nd
!
!------------------------------------------------------------------
!
      end module copy_comm_tables_to_2nd

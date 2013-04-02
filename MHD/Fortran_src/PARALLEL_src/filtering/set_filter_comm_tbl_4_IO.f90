!
!      module set_filter_comm_tbl_4_IO
!
!     Written by H. Matsui on Apr., 2008
!
!      subroutine copy_filter_comm_tbl_from_IO
!      subroutine copy_filter_comm_tbl_to_IO(my_rank)
!
!      subroutine copy_filter_import_from_IO
!      subroutine copy_filter_import_to_IO
!
      module set_filter_comm_tbl_4_IO
!
      use m_precision
!
      use m_nod_filter_comm_table
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
      subroutine copy_filter_comm_tbl_from_IO
!
      integer(kind = kint) :: i
!
!
      num_neib_filter = num_neib_domain_IO
!
      call allocate_neib_filter_id
      call allocate_filter_import_num
      call allocate_filter_export_num
!
      call copy_num_communication(num_neib_filter, id_neib_filter,      &
     &    istack_import_filter, istack_export_filter,                   &
     &    ntot_import_filter, ntot_export_filter,                       &
     &    id_neib_domain_IO, istack_import_IO, istack_export_IO)
      call copy_num_import_export(num_neib_filter,                      &
     &    num_import_filter, num_export_filter,                         &
     &    istack_import_filter, istack_export_filter)
!
      call allocate_filter_import_item
      call allocate_filter_export_item
!
      call copy_communication_item(ntot_import_filter,                 &
     &    ntot_export_filter, item_import_filter, item_export_filter,  &
     &    item_import_IO, item_export_IO)
!
      call deallocate_comm_item_IO
!
      end subroutine copy_filter_comm_tbl_from_IO
!
!-----------------------------------------------------------------------
!
      subroutine copy_filter_comm_tbl_to_IO(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      my_rank_IO = my_rank
      num_neib_domain_IO = num_neib_filter
!
      call allocate_neib_comm_stack_IO
!
      call copy_num_communication(num_neib_domain_IO,                   &
     &    id_neib_domain_IO, istack_import_IO, istack_export_IO,        &
     &    ntot_import_IO, ntot_export_IO,                               &
     &    id_neib_filter, istack_import_filter, istack_export_filter)
!
      call allocate_comm_item_IO
!
      call copy_communication_item(ntot_import_IO, ntot_export_IO,     &
     &    item_import_IO, item_export_IO, item_import_filter,          &
     &    item_export_filter)
!
      end subroutine copy_filter_comm_tbl_to_IO
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_filter_import_from_IO
!
      integer(kind = kint) :: i
!
!
      num_neib_filter = num_neib_domain_IO
!
      call allocate_neib_filter_id
      call allocate_filter_import_num
      call allocate_filter_export_num
!
      call copy_num_import(num_neib_filter, id_neib_filter,             &
     &    istack_import_filter, istack_export_filter,                   &
     &    ntot_import_filter, ntot_export_filter,                       &
     &    id_neib_domain_IO, istack_import_IO)
      call copy_num_import_export(num_neib_filter,                      &
     &    num_import_filter, num_export_filter,                         &
     &    istack_import_filter, istack_export_filter)
!
      call allocate_filter_import_item
      call allocate_filter_export_item
!
      call copy_communication_item(ntot_import_filter,                 &
     &    ntot_export_filter, item_import_filter, item_export_filter,  &
     &    item_import_IO, item_export_IO)
!
      call deallocate_comm_item_IO
!
      end subroutine copy_filter_import_from_IO
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_filter_import_to_IO(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
      my_rank_IO = my_rank
!
      num_neib_domain_IO = num_neib_filter
!
      call allocate_neib_comm_stack_IO
!
      call copy_num_import(num_neib_domain_IO, id_neib_domain_IO,       &
     &    istack_import_IO, istack_export_IO,                           &
     &    ntot_import_IO, ntot_export_IO,                               &
     &    id_neib_filter, istack_import_filter)
!
      call allocate_comm_item_IO
!
      call copy_communication_item(ntot_import_IO, ntot_export_IO,     &
     &    item_import_IO, item_export_IO, item_import_filter,          &
     &    item_export_filter)
!
      end subroutine copy_filter_import_to_IO
!
!-----------------------------------------------------------------------
!
      end module set_filter_comm_tbl_4_IO

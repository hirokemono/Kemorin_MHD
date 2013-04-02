!
!      module set_2nd_ele_comm_tbl_4_IO
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine copy_2nd_ele_comm_tbl_from_IO
!      subroutine copy_2nd_ele_comm_tbl_to_IO(my_rank)
!
!      subroutine copy_2nd_ele_import_from_IO
!      subroutine copy_2nd_ele_import_to_IO(my_rank)
!
      module set_2nd_ele_comm_tbl_4_IO
!
      use m_precision
!
      use m_2nd_ele_comm_table
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
      subroutine copy_2nd_ele_comm_tbl_from_IO
!
      integer(kind = kint) :: i
!
!
      num_neib_ele_2 = num_neib_domain_IO
!
      call allocate_2nd_ele_neib_id
      call allocate_2nd_ele_import_num
      call allocate_2nd_ele_export_num
!
      call copy_num_communication(num_neib_ele_2, id_neib_ele_2,        &
     &    istack_import_ele_2, istack_export_ele_2,                     &
     &    ntot_import_ele_2, ntot_export_ele_2,                         &
     &    id_neib_domain_IO, istack_import_IO, istack_export_IO)
      call copy_num_import_export(num_neib_ele_2,                       &
     &    num_import_ele_2, num_export_ele_2,                           &
     &    istack_import_ele_2, istack_export_ele_2)
!
      call allocate_2nd_ele_import_item
      call allocate_2nd_ele_export_item
!
      call copy_communication_item(ntot_import_ele_2,                   &
     &    ntot_export_ele_2, item_import_ele_2, item_export_ele_2,      &
     &    item_import_IO, item_export_IO)
!
      call deallocate_comm_item_IO
!
      end subroutine copy_2nd_ele_comm_tbl_from_IO
!
!-----------------------------------------------------------------------
!
      subroutine copy_2nd_ele_comm_tbl_to_IO(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      my_rank_IO = my_rank
      num_neib_domain_IO = num_neib_ele_2
!
      call allocate_neib_comm_stack_IO
!
      call copy_num_communication(num_neib_domain_IO,                   &
     &    id_neib_domain_IO, istack_import_IO, istack_export_IO,        &
     &    ntot_import_IO, ntot_export_IO,                               &
     &    id_neib_ele_2, istack_import_ele_2, istack_export_ele_2)
!
      call allocate_comm_item_IO
!
      call copy_communication_item(ntot_import_IO, ntot_export_IO,      &
     &    item_import_IO, item_export_IO,                               &
     &    item_import_ele_2, item_export_ele_2)
!
      end subroutine copy_2nd_ele_comm_tbl_to_IO
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_2nd_ele_import_from_IO
!
      integer(kind = kint) :: i
!
!
      num_neib_ele_2 = num_neib_domain_IO
!
      call allocate_2nd_ele_neib_id
      call allocate_2nd_ele_import_num
      call allocate_2nd_ele_export_num
!
      call copy_num_import(num_neib_ele_2, id_neib_ele_2,               &
     &    istack_import_ele_2, istack_export_ele_2,                     &
     &    ntot_import_ele_2, ntot_export_ele_2,                         &
     &    id_neib_domain_IO, istack_import_IO)
      call copy_num_import_export(num_neib_ele_2,                       &
     &    num_import_ele_2, num_export_ele_2,                           &
     &    istack_import_ele_2, istack_export_ele_2)
!
      call allocate_2nd_ele_import_item
      call allocate_2nd_ele_export_item
!
      call copy_communication_item(ntot_import_ele_2,                   &
     &    ntot_export_ele_2, item_import_ele_2, item_export_ele_2,      &
     &    item_import_IO, item_export_IO)
!
      call deallocate_comm_item_IO
!
      end subroutine copy_2nd_ele_import_from_IO
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_2nd_ele_import_to_IO(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      my_rank_IO = my_rank
      num_neib_domain_IO = num_neib_ele_2
!
      call allocate_neib_comm_stack_IO
!
      call copy_num_import(num_neib_domain_IO, id_neib_domain_IO,       &
     &    istack_import_IO, istack_export_IO,                           &
     &    ntot_import_IO, ntot_export_IO,                               &
     &    id_neib_ele_2, istack_import_ele_2)
!
      call allocate_comm_item_IO
!
      call copy_communication_item(ntot_import_IO, ntot_export_IO,      &
     &    item_import_IO, item_export_IO,                               &
     &    item_import_ele_2, item_export_ele_2)
!
!
      end subroutine copy_2nd_ele_import_to_IO
!
!-----------------------------------------------------------------------
!
      end module set_2nd_ele_comm_tbl_4_IO

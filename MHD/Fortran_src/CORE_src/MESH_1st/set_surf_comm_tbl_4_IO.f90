!
!      module set_surf_comm_tbl_4_IO
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine copy_surf_comm_table_from_IO
!      subroutine copy_surf_comm_tbl_to_IO(my_rank)
!
!      subroutine copy_surf_import_from_IO
!      subroutine copy_surf_import_to_IO(my_rank)
!
      module set_surf_comm_tbl_4_IO
!
      use m_precision
!
      use m_surf_comm_table
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
      subroutine copy_surf_comm_table_from_IO
!
!
      num_neib_surf = num_neib_domain_IO
!
      call allocate_surf_neib_id
      call allocate_surf_import_num
      call allocate_surf_export_num
!
      call copy_num_communication(num_neib_surf, id_neib_surf,          &
     &    istack_import_surf, istack_export_surf,                       &
     &    ntot_import_surf, ntot_export_surf,                           &
     &    id_neib_domain_IO, istack_import_IO, istack_export_IO)
      call copy_num_import_export(num_neib_surf,                        &
     &    num_import_surf, num_export_surf,                             &
     &    istack_import_surf, istack_export_surf)
!
      call allocate_surf_import_item
      call allocate_surf_export_item
!
      call copy_communication_item(ntot_import_surf, ntot_export_surf,  &
     &    item_import_surf, item_export_surf,                           &
     &    item_import_IO, item_export_IO)
!
      call deallocate_comm_item_IO
!
      end subroutine copy_surf_comm_table_from_IO
!
!-----------------------------------------------------------------------
!
      subroutine copy_surf_comm_tbl_to_IO(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      my_rank_IO = my_rank
      num_neib_domain_IO = num_neib_surf
!
      call allocate_neib_comm_stack_IO
!
      call copy_num_communication(num_neib_domain_IO,                   &
     &    id_neib_domain_IO, istack_import_IO, istack_export_IO,        &
     &    ntot_import_IO, ntot_export_IO,                               &
     &    id_neib_surf, istack_import_surf, istack_export_surf)
!
      call allocate_comm_item_IO
!
      call copy_communication_item(ntot_import_IO, ntot_export_IO,      &
     &    item_import_IO, item_export_IO,                               &
     &    item_import_surf, item_export_surf)
!
      end subroutine copy_surf_comm_tbl_to_IO
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_surf_import_from_IO
!
!
      num_neib_surf = num_neib_domain_IO
!
      call allocate_surf_neib_id
      call allocate_surf_import_num
      call allocate_surf_export_num
!
      call copy_num_import(num_neib_surf, id_neib_surf,                 &
     &    istack_import_surf, istack_export_surf,                       &
     &    ntot_import_surf, ntot_export_surf,                           &
     &    id_neib_domain_IO, istack_import_IO)
      call copy_num_import_export(num_neib_surf,                        &
     &    num_import_surf, num_export_surf,                             &
     &    istack_import_surf, istack_export_surf)
!
      call allocate_surf_import_item
      call allocate_surf_export_item
!
      call copy_communication_item(ntot_import_surf, ntot_export_surf,  &
     &    item_import_surf, item_export_surf,                           &
     &    item_import_IO, item_export_IO)
!
      call deallocate_comm_item_IO
!
      end subroutine copy_surf_import_from_IO
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_surf_import_to_IO(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      my_rank_IO = my_rank
      num_neib_domain_IO = num_neib_surf
!
      call allocate_neib_comm_stack_IO
!
      call copy_num_import(num_neib_domain_IO, id_neib_domain_IO,       &
     &    istack_import_IO, istack_export_IO,                           &
     &    ntot_import_IO, ntot_export_IO,                               &
     &    id_neib_surf, istack_import_surf)
!
      call allocate_comm_item_IO
!
      call copy_communication_item(ntot_import_IO, ntot_export_IO,      &
     &    item_import_IO, item_export_IO,                               &
     &    item_import_surf, item_export_surf)
!
      end subroutine copy_surf_import_to_IO
!
!-----------------------------------------------------------------------
!
      end module set_surf_comm_tbl_4_IO

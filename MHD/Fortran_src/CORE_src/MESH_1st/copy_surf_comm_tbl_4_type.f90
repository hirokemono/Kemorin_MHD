!copy_surf_comm_tbl_4_type.f90
!      module copy_surf_comm_tbl_4_type
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine copy_surf_comm_tbl_from_type(surf_comm)
!      subroutine copy_surf_comm_tbl_to_type(surf_comm)
!
!      subroutine copy_surf_import_from_type(surf_comm)
!      subroutine copy_surf_import_to_type(surf_comm)
!        type(communication_table), intent(inout) :: surf_comm
!
      module copy_surf_comm_tbl_4_type
!
      use m_precision
!
      use m_surf_comm_table
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
      subroutine copy_surf_comm_tbl_from_type(surf_comm)
!
      type(communication_table), intent(in) :: surf_comm
!
!
      num_neib_surf = surf_comm%num_neib
!
      call allocate_surf_neib_id
      call allocate_surf_import_num
      call allocate_surf_export_num
!
      call copy_num_communication(num_neib_surf, id_neib_surf,          &
     &    istack_import_surf, istack_export_surf, ntot_import_surf,     &
     &    ntot_export_surf, surf_comm%id_neib, surf_comm%istack_import, &
     &    surf_comm%istack_export)
      call copy_num_import_export(num_neib_surf, num_import_surf,       &
     &    num_export_surf, istack_import_surf, istack_export_surf)
!
      call allocate_surf_import_item
      call allocate_surf_export_item
!
      call copy_communication_item(ntot_import_surf, ntot_export_surf,  &
     &    item_import_surf, item_export_surf, surf_comm%item_import,    &
     &    surf_comm%item_export)
!
      end subroutine copy_surf_comm_tbl_from_type
!
!-----------------------------------------------------------------------
!
      subroutine copy_surf_comm_tbl_to_type(surf_comm)
!
      type(communication_table), intent(inout) :: surf_comm
!
!
      surf_comm%num_neib = num_neib_surf
!
      call allocate_type_comm_tbl_num(surf_comm)
!
      call copy_num_communication(surf_comm%num_neib,                   &
     &    surf_comm%id_neib, surf_comm%istack_import,                   &
     &    surf_comm%istack_export, surf_comm%ntot_import,               &
     &    surf_comm%ntot_export, id_neib_surf, istack_import_surf,      &
     &    istack_export_surf)
      call copy_num_import_export(surf_comm%num_neib,                   &
     &    surf_comm%num_import, surf_comm%num_export,                   &
     &    surf_comm%istack_import, surf_comm%istack_export)
!
      call allocate_type_comm_tbl_item(surf_comm)
!
      call copy_communication_item(surf_comm%ntot_import,               &
     &    surf_comm%ntot_export, surf_comm%item_import,                 &
     &    surf_comm%item_export, item_import_surf, item_export_surf)
!
      end subroutine copy_surf_comm_tbl_to_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_surf_import_from_type(surf_comm)
!
      type(communication_table), intent(in) :: surf_comm
!
!
      num_neib_surf = surf_comm%num_neib
!
      call allocate_surf_neib_id
      call allocate_surf_import_num
      call allocate_surf_export_num
!
      call copy_num_import(num_neib_surf, id_neib_surf,                 &
     &    istack_import_surf, istack_export_surf, ntot_import_surf,     &
     &    ntot_export_surf, surf_comm%id_neib, surf_comm%istack_import)
      call copy_num_import_export(num_neib_surf, num_import_surf,       &
     &    num_export_surf, istack_import_surf, istack_export_surf)
!
      call allocate_surf_import_item
      call allocate_surf_export_item
!
      call copy_communication_item(ntot_import_surf, ntot_export_surf,  &
     &    item_import_surf, item_export_surf, surf_comm%item_import,    &
     &    surf_comm%item_export)
!
      end subroutine copy_surf_import_from_type
!
!-----------------------------------------------------------------------
!
      subroutine copy_surf_import_to_type(surf_comm)
!
      type(communication_table), intent(inout) :: surf_comm
!
!
      surf_comm%num_neib = num_neib_surf
!
      call allocate_type_comm_tbl_num(surf_comm)
!
      call copy_num_import(surf_comm%num_neib, surf_comm%id_neib,       &
     &    surf_comm%istack_import, surf_comm%istack_export,             &
     &    surf_comm%ntot_import, surf_comm%ntot_export, id_neib_surf,   &
     &    istack_import_surf)
      call copy_num_import_export(surf_comm%num_neib,                   &
     &    surf_comm%num_import, surf_comm%num_export,                   &
     &    surf_comm%istack_import, surf_comm%istack_export)
!
      call allocate_type_comm_tbl_item(surf_comm)
!
      call copy_communication_item(surf_comm%ntot_import,               &
     &    surf_comm%ntot_export, surf_comm%item_import,                 &
     &    surf_comm%item_export, item_import_surf, item_export_surf)
!
      end subroutine copy_surf_import_to_type
!
!-----------------------------------------------------------------------
!
      end module copy_surf_comm_tbl_4_type

!copy_ele_comm_tbl_4_type.f90
!      module copy_ele_comm_tbl_4_type
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine copy_ele_comm_tbl_from_type(ele_comm)
!      subroutine copy_ele_comm_tbl_to_type(ele_comm)
!
!      subroutine copy_ele_import_from_type(ele_comm)
!      subroutine copy_ele_import_to_type(ele_comm)
!        type(communication_table), intent(inout) :: ele_comm
!
      module copy_ele_comm_tbl_4_type
!
      use m_precision
!
      use m_ele_comm_table
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
      subroutine copy_ele_comm_tbl_from_type(ele_comm)
!
      type(communication_table), intent(in) :: ele_comm
!
!
      num_neib_ele = ele_comm%num_neib
!
      call allocate_ele_neib_id
      call allocate_ele_import_num
      call allocate_ele_export_num
!
      call copy_num_communication(num_neib_ele, id_neib_ele,            &
     &    istack_import_ele, istack_export_ele, ntot_import_ele,        &
     &    ntot_export_ele, ele_comm%id_neib, ele_comm%istack_import,    &
     &    ele_comm%istack_export)
      call copy_num_import_export(num_neib_ele, num_import_ele,         &
     &    num_export_ele, istack_import_ele, istack_export_ele)
!
      call allocate_ele_import_item
      call allocate_ele_export_item
!
      call copy_communication_item(ntot_import_ele, ntot_export_ele,    &
     &    item_import_ele, item_export_ele, ele_comm%item_import,       &
     &    ele_comm%item_export)
!
      end subroutine copy_ele_comm_tbl_from_type
!
!-----------------------------------------------------------------------
!
      subroutine copy_ele_comm_tbl_to_type(ele_comm)
!
      type(communication_table), intent(inout) :: ele_comm
!
!
      ele_comm%num_neib = num_neib_ele
!
      call allocate_type_comm_tbl_num(ele_comm)
!
      call copy_num_communication(ele_comm%num_neib,                   &
     &    ele_comm%id_neib, ele_comm%istack_import,                    &
     &    ele_comm%istack_export, ele_comm%ntot_import,                &
     &    ele_comm%ntot_export, id_neib_ele, istack_import_ele,        &
     &    istack_export_ele)
      call copy_num_import_export(ele_comm%num_neib,                   &
     &    ele_comm%num_import, ele_comm%num_export,                    &
     &    ele_comm%istack_import, ele_comm%istack_export)
!
      call allocate_type_comm_tbl_item(ele_comm)
!
      call copy_communication_item(ele_comm%ntot_import,               &
     &    ele_comm%ntot_export, ele_comm%item_import,                  &
     &    ele_comm%item_export, item_import_ele, item_export_ele)
!
      end subroutine copy_ele_comm_tbl_to_type
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine copy_ele_import_from_type(ele_comm)
!
      type(communication_table), intent(in) :: ele_comm
!
!
      num_neib_ele = ele_comm%num_neib
!
      call allocate_ele_neib_id
      call allocate_ele_import_num
      call allocate_ele_export_num
!
      call copy_num_import(num_neib_ele, id_neib_ele,                   &
     &    istack_import_ele, istack_export_ele, ntot_import_ele,        &
     &    ntot_export_ele, ele_comm%id_neib, ele_comm%istack_import)
      call copy_num_import_export(num_neib_ele, num_import_ele,         &
     &    num_export_ele, istack_import_ele, istack_export_ele)
!
      call allocate_ele_import_item
      call allocate_ele_export_item
!
      call copy_communication_item(ntot_import_ele, ntot_export_ele,    &
     &    item_import_ele, item_export_ele, ele_comm%item_import,       &
     &    ele_comm%item_export)
!
      end subroutine copy_ele_import_from_type
!
!-----------------------------------------------------------------------
!
      subroutine copy_ele_import_to_type(ele_comm)
!
      type(communication_table), intent(inout) :: ele_comm
!
!
      ele_comm%num_neib = num_neib_ele
!
      call allocate_type_comm_tbl_num(ele_comm)
!
      call copy_num_import(ele_comm%num_neib, ele_comm%id_neib,        &
     &    ele_comm%istack_import, ele_comm%istack_export,              &
     &    ele_comm%ntot_import, ele_comm%ntot_export, id_neib_ele,     &
     &    istack_import_ele)
      call copy_num_import_export(ele_comm%num_neib,                   &
     &    ele_comm%num_import, ele_comm%num_export,                    &
     &    ele_comm%istack_import, ele_comm%istack_export)
!
      call allocate_type_comm_tbl_item(ele_comm)
!
      call copy_communication_item(ele_comm%ntot_import,               &
     &    ele_comm%ntot_export, ele_comm%item_import,                  &
     &    ele_comm%item_export, item_import_ele, item_export_ele)
!
      end subroutine copy_ele_import_to_type
!
!-----------------------------------------------------------------------
!
      end module copy_ele_comm_tbl_4_type

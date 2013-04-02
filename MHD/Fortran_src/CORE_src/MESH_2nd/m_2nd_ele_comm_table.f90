!
!      module m_2nd_ele_comm_table
!
!     Written by H. Matsui on Aug., 2007
!
!      subroutine allocate_2nd_ele_neib_id
!      subroutine deallocate_2nd_ele_neib_id
!
!      subroutine allocate_2nd_ele_import_num
!      subroutine allocate_2nd_ele_export_num
!      subroutine allocate_2nd_ele_import_item
!      subroutine allocate_2nd_ele_export_item
!
!      subroutine deallocate_2nd_ele_import
!      subroutine deallocate_2nd_ele_export
!      subroutine deallocate_2nd_ele_import_num
!      subroutine deallocate_2nd_ele_export_num
!
      module m_2nd_ele_comm_table
!
      use m_precision
!
      implicit  none
!
      integer(kind = kint) :: num_neib_ele_2
      integer(kind = kint), pointer :: id_neib_ele_2(:)
!
      integer(kind = kint) :: ntot_import_ele_2
      integer(kind = kint), pointer :: num_import_ele_2(:)
      integer(kind = kint), pointer :: istack_import_ele_2(:)
      integer(kind = kint), pointer :: item_import_ele_2(:)
!
      integer(kind = kint) :: ntot_export_ele_2
      integer(kind = kint), pointer :: num_export_ele_2(:)
      integer(kind = kint), pointer :: istack_export_ele_2(:)
      integer(kind = kint), pointer :: item_export_ele_2(:)
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_2nd_ele_neib_id
!
      allocate(id_neib_ele_2(num_neib_ele_2))
!
      if (num_neib_ele_2 .gt. 0) id_neib_ele_2 = -1
!
      end subroutine allocate_2nd_ele_neib_id
!
!------------------------------------------------------------------
!
      subroutine deallocate_2nd_ele_neib_id
!
      deallocate(id_neib_ele_2)
!
      end subroutine deallocate_2nd_ele_neib_id
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_2nd_ele_import_num
!
!
      allocate(num_import_ele_2(num_neib_ele_2))
      allocate(istack_import_ele_2(0:num_neib_ele_2))
!
      if (num_neib_ele_2 .gt. 0) num_import_ele_2 = 0
      istack_import_ele_2 = 0
!
      end subroutine allocate_2nd_ele_import_num
!
!------------------------------------------------------------------
!
      subroutine allocate_2nd_ele_export_num
!
!
      allocate(num_export_ele_2(num_neib_ele_2))
      allocate(istack_export_ele_2(0:num_neib_ele_2))
!
      if (num_neib_ele_2 .gt. 0) num_export_ele_2 = 0
      istack_export_ele_2 = 0
!
      end subroutine allocate_2nd_ele_export_num
!
!------------------------------------------------------------------
!
      subroutine allocate_2nd_ele_import_item
!
!
      ntot_import_ele_2 = istack_import_ele_2(num_neib_ele_2)
      allocate(item_import_ele_2(ntot_import_ele_2))
!
      if (ntot_import_ele_2 .gt. 0) item_import_ele_2 = 0
!
      end subroutine allocate_2nd_ele_import_item
!
!------------------------------------------------------------------
!
      subroutine allocate_2nd_ele_export_item
!
!
      ntot_export_ele_2 = istack_export_ele_2(num_neib_ele_2)
      allocate(item_export_ele_2(ntot_export_ele_2))
!
      if (ntot_export_ele_2 .gt. 0) item_export_ele_2 = 0
!
      end subroutine allocate_2nd_ele_export_item
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_2nd_ele_import
!
!
      call deallocate_2nd_ele_import_num
      deallocate(item_import_ele_2)
!
      end subroutine deallocate_2nd_ele_import
!
!------------------------------------------------------------------
!
      subroutine deallocate_2nd_ele_export
!
!
      call deallocate_2nd_ele_export_num
      deallocate(item_export_ele_2)
!
      end subroutine deallocate_2nd_ele_export
!
!------------------------------------------------------------------
!
      subroutine deallocate_2nd_ele_import_num
!
!
      deallocate(num_import_ele_2)
      deallocate(istack_import_ele_2)
!
      end subroutine deallocate_2nd_ele_import_num
!
!------------------------------------------------------------------
!
      subroutine deallocate_2nd_ele_export_num
!
!
      deallocate(num_export_ele_2)
      deallocate(istack_export_ele_2)
!
      end subroutine deallocate_2nd_ele_export_num
!
!------------------------------------------------------------------
!
      end module m_2nd_ele_comm_table

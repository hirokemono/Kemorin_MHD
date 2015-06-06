!>@file   m_nod_comm_table.f90
!!@brief  module m_nod_comm_table
!!
!!@author H. Matsui and H. Okuda
!!@date Programmed in 2000
!!@n     Modified in 2006 
!
!> @brief Communication table for node
!!
!!@verbatim
!!      subroutine allocate_neib_id
!!
!!      subroutine allocate_nod_import_num
!!      subroutine allocate_nod_export_num
!!      subroutine allocate_nod_import_item
!!      subroutine allocate_nod_export_item
!!
!!      subroutine deallocate_neib_id
!!
!!      subroutine deallocate_nod_import_item
!!      subroutine deallocate_nod_export_item
!!@endverbatim
!
      module m_nod_comm_table
!
      use m_precision
      use t_comm_table
!
      implicit  none
!
!> data structure for node communication table
      type(communication_table), save :: nod_comm
!
!
!>     import node count for each neighbor pe (i-th pe)
      integer(kind = kint), allocatable, target :: num_import(:)
!>     local id for import node                     (i-th)
      integer(kind = kint), allocatable, target :: item_import(:)
!
!>     export node count for each neighbor pe (i-th pe)
      integer(kind = kint), allocatable, target :: num_export(:)
!>     local id for export node                     (i-th)
      integer(kind = kint), allocatable, target :: item_export(:)
!
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_neib_id
!
      allocate(nod_comm%id_neib(nod_comm%num_neib))
      if (nod_comm%num_neib .gt. 0) nod_comm%id_neib = -1
!
      end subroutine allocate_neib_id
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_nod_import_num
!
      allocate(num_import(nod_comm%num_neib))
      allocate(nod_comm%istack_import(0:nod_comm%num_neib))
!
      if (nod_comm%num_neib .gt. 0) num_import = 0
      nod_comm%istack_import = 0
!
      end subroutine allocate_nod_import_num
!
!------------------------------------------------------------------
!
      subroutine allocate_nod_export_num
!
      allocate(num_export(nod_comm%num_neib))
      allocate(nod_comm%istack_export(0:nod_comm%num_neib))
!
      if (nod_comm%num_neib .gt. 0) num_export = 0
      nod_comm%istack_export = 0
!
      end subroutine allocate_nod_export_num
!
!------------------------------------------------------------------
!
      subroutine allocate_nod_import_item
!
      nod_comm%ntot_import = nod_comm%istack_import(nod_comm%num_neib)
      allocate(item_import(nod_comm%ntot_import))
!
      if (nod_comm%ntot_import .gt. 0) item_import = 0
!
      end subroutine allocate_nod_import_item
!
!------------------------------------------------------------------
!
      subroutine allocate_nod_export_item
!
      nod_comm%ntot_export = nod_comm%istack_export(nod_comm%num_neib)
      allocate(item_export(nod_comm%ntot_export))
!
      if (nod_comm%ntot_export .gt. 0) item_export = 0
!
      end subroutine allocate_nod_export_item
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_neib_id
!
      deallocate(nod_comm%id_neib)
!
      end subroutine deallocate_neib_id
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_nod_import_item
!
      deallocate(num_import)
      deallocate(nod_comm%istack_import)
      deallocate(item_import)
!
      end subroutine deallocate_nod_import_item
!
!------------------------------------------------------------------
!
      subroutine deallocate_nod_export_item
!
      deallocate(num_export)
      deallocate(nod_comm%istack_export)
      deallocate(item_export)
!
      end subroutine deallocate_nod_export_item
!
!------------------------------------------------------------------
!
      end module m_nod_comm_table

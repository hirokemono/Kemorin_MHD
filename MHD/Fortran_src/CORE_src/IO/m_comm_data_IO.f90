!>@file   m_comm_data_IO.f90
!!@brief  module m_comm_data_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Array for communication table IO
!!
!!@verbatim
!!      subroutine allocate_neib_comm_stack_IO
!!      subroutine allocate_comm_item_IO
!!      subroutine deallocate_comm_item_IO
!!
!!      subroutine allocate_neib_domain_IO
!!      subroutine allocate_import_stack_IO
!!      subroutine allocate_export_stack_IO
!!      subroutine allocate_import_item_IO
!!      subroutine allocate_export_item_IO
!!      subroutine allocate_import_work_IO
!!      subroutine allocate_export_work_IO
!!
!!      subroutine deallocate_neib_domain_IO
!!      subroutine deallocate_import_item_IO
!!      subroutine deallocate_export_item_IO
!!      subroutine deallocate_import_work_IO
!!      subroutine deallocate_export_work_IO
!!@verbatim
!
      module m_comm_data_IO
!
      use m_precision
      use t_comm_table
!
      implicit none
!
!> data structure for communication table IO
      type(communication_table), save :: comm_IO
!
      integer(kind = kint) :: my_rank_IO
!
!      integer(kind = kint) :: num_neib_domain_IO
!      integer(kind = kint), allocatable :: id_neib_domain_IO(:)
!      integer(kind = kint), allocatable :: istack_import_IO(:)
!      integer(kind = kint), allocatable :: istack_export_IO(:)
!
!      integer(kind = kint) :: ntot_import_IO
!      integer(kind = kint), allocatable :: item_import_IO(:)
!
!      integer(kind = kint) :: ntot_export_IO
!      integer(kind = kint), allocatable :: item_export_IO(:)
!
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine allocate_neib_comm_stack_IO
!
      call allocate_neib_domain_IO
      call allocate_import_stack_IO
      call allocate_export_stack_IO
!
      end subroutine allocate_neib_comm_stack_IO
!
!------------------------------------------------------------------
!
      subroutine allocate_comm_item_IO
!
      call allocate_import_item_IO
      call allocate_export_item_IO
!
      end subroutine allocate_comm_item_IO
!
!------------------------------------------------------------------
!
      subroutine deallocate_comm_item_IO
!
      call deallocate_import_item_IO
      call deallocate_export_item_IO
      call deallocate_neib_domain_IO
!
      end subroutine deallocate_comm_item_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine allocate_neib_domain_IO
!
      allocate(comm_IO%id_neib(comm_IO%num_neib))
!
      if (comm_IO%num_neib .gt. 0) comm_IO%id_neib = 0
!
      end subroutine allocate_neib_domain_IO
!
!------------------------------------------------------------------
!
      subroutine allocate_import_stack_IO
!
      allocate(comm_IO%istack_import(0:comm_IO%num_neib))
      comm_IO%istack_import = 0
!
      end subroutine allocate_import_stack_IO
!
!------------------------------------------------------------------
!
      subroutine allocate_export_stack_IO
!
      allocate(comm_IO%istack_export(0:comm_IO%num_neib))
      comm_IO%istack_export = 0
!
      end subroutine allocate_export_stack_IO
!
!------------------------------------------------------------------
!
      subroutine allocate_import_item_IO
!
      allocate(comm_IO%item_import(comm_IO%ntot_import))
      if (comm_IO%ntot_import.gt.0) comm_IO%item_import = 0
!
      end subroutine allocate_import_item_IO
!
!------------------------------------------------------------------
!
      subroutine allocate_export_item_IO
!
      allocate(comm_IO%item_export(comm_IO%ntot_export))
      if (comm_IO%ntot_export.gt.0)  comm_IO%item_export = 0
!
      end subroutine allocate_export_item_IO
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine deallocate_neib_domain_IO
!
      deallocate(comm_IO%id_neib)
!
      end subroutine deallocate_neib_domain_IO
!
!------------------------------------------------------------------
!
      subroutine deallocate_import_item_IO
!
      deallocate(comm_IO%item_import)
      deallocate(comm_IO%istack_import)
!
      end subroutine deallocate_import_item_IO
!
!------------------------------------------------------------------
!
      subroutine deallocate_export_item_IO
!
      deallocate(comm_IO%item_export)
      deallocate(comm_IO%istack_export)
!
      end subroutine deallocate_export_item_IO
!
!------------------------------------------------------------------
!
      end module m_comm_data_IO

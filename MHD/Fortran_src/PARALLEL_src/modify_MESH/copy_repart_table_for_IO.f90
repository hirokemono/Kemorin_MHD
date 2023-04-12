!>@file   copy_repart_table_for_IO.f90
!!      module copy_repart_table_for_IO
!!
!!@author  H. Matsui
!!@date Programmed in Apr., 2022
!
!>@brief copy repatition data for data IO
!!
!!@verbatim
!!      subroutine import_IO_to_repart_table(import_IO, part_tbl, ierr)
!!        type(communication_table), intent(in) :: import_IO
!!        type(calypso_comm_table), intent(inout) :: part_tbl
!!        integer(kind = kint), intent(inout) :: ierr
!!@endverbatim
      module copy_repart_table_for_IO
!
      use m_precision
      use m_machine_parameter
      use t_comm_table
      use t_calypso_comm_table
!
      implicit none
!
!------------------------------------------------------------------
!
       contains
!
!-----------------------------------------------------------------------
!
      subroutine repart_table_to_export_IO(part_tbl, export_IO)
!
      type(calypso_comm_table), intent(in) :: part_tbl
      type(communication_table), intent(inout) :: export_IO
!
!
      export_IO%num_neib =          part_tbl%nrank_export
      call alloc_neighbouring_id(export_IO)
      call alloc_export_num(export_IO)
!
      export_IO%istack_export(0) = part_tbl%istack_export(0)
      if(part_tbl%nrank_export .gt. 0) then
!$omp parallel workshare
        export_IO%id_neib(1:part_tbl%nrank_export)                      &
     &          = part_tbl%irank_export(1:part_tbl%nrank_export)
        export_IO%istack_export(1:part_tbl%nrank_export)                &
     &          = part_tbl%istack_export(1:part_tbl%nrank_export)
!$omp end parallel workshare
      end if
!
      export_IO%ntot_export = part_tbl%ntot_export
      call alloc_export_item(export_IO)
!
      if(part_tbl%ntot_export .gt. 0) then
!$omp parallel workshare
        export_IO%item_export(1:part_tbl%ntot_export)                 &
     &               = part_tbl%item_export(1:part_tbl%ntot_export)
!$omp end parallel workshare
      end if
!
      end subroutine repart_table_to_export_IO
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine export_IO_to_repart_table                              &
     &         (irank_read, export_IO, part_tbl)
!
      integer(kind= kint), intent(in) :: irank_read
      type(communication_table), intent(in) :: export_IO
      type(calypso_comm_table), intent(inout) :: part_tbl
!
!
      part_tbl%iflag_self_copy = 0
      part_tbl%nrank_export =    export_IO%num_neib
      call alloc_calypso_export_num(part_tbl)
!
      part_tbl%istack_export(0) = export_IO%istack_export(0)
      if(part_tbl%nrank_export .gt. 0) then
!$omp parallel workshare
        part_tbl%irank_export(1:part_tbl%nrank_export)                  &
     &          = export_IO%id_neib(1:part_tbl%nrank_export)
        part_tbl%istack_export(1:part_tbl%nrank_export)                 &
     &          = export_IO%istack_export(1:part_tbl%nrank_export)
!$omp end parallel workshare
!
        if(part_tbl%irank_export(part_tbl%nrank_export)                 &
     &                  .eq. irank_read) part_tbl%iflag_self_copy = 1
      end if
!
      part_tbl%ntot_export = export_IO%ntot_export
      call alloc_calypso_export_item(part_tbl)
!
      if(part_tbl%ntot_export .gt. 0) then
!$omp parallel workshare
        part_tbl%item_export(1:part_tbl%ntot_export)                    &
     &               = export_IO%item_export(1:part_tbl%ntot_export)
!$omp end parallel workshare
      end if
!
      end subroutine export_IO_to_repart_table
!
!-----------------------------------------------------------------------
!
      subroutine repart_table_to_import_IO(part_tbl, import_IO)
!
      type(calypso_comm_table), intent(in) :: part_tbl
      type(communication_table), intent(inout) :: import_IO
!
!
      import_IO%num_neib =         part_tbl%nrank_import
      call alloc_neighbouring_id(import_IO)
      call alloc_import_num(import_IO)
      import_IO%istack_import(0) = part_tbl%istack_import(0)
!
      if(part_tbl%nrank_import .gt. 0) then
!$omp parallel workshare
        import_IO%id_neib(1:part_tbl%nrank_import)                      &
     &          = part_tbl%irank_import(1:part_tbl%nrank_import)
        import_IO%istack_import(1:part_tbl%nrank_import)                &
     &          = part_tbl%istack_import(1:part_tbl%nrank_import)
!$omp end parallel workshare
      end if
!
      import_IO%ntot_import = part_tbl%ntot_import
      call alloc_import_item(import_IO)
!
      if(part_tbl%ntot_import .gt. 0) then
!$omp parallel workshare
        import_IO%item_import(1:part_tbl%ntot_import)                   &
     &      = part_tbl%item_import(1:part_tbl%ntot_import)
!$omp end parallel workshare
      end if
!
      end subroutine repart_table_to_import_IO
!
!-----------------------------------------------------------------------
!
      subroutine import_IO_to_repart_table(import_IO, part_tbl, ierr)
!
      type(communication_table), intent(in) :: import_IO
      type(calypso_comm_table), intent(inout) :: part_tbl
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: nnod_rev
!
!
      part_tbl%nrank_import = import_IO%num_neib
      call alloc_calypso_import_num(part_tbl)
      part_tbl%istack_import(0) = import_IO%istack_import(0)
!
      if(part_tbl%nrank_import .gt. 0) then
!$omp parallel workshare
        part_tbl%irank_import(1:part_tbl%nrank_import)                  &
     &          = import_IO%id_neib(1:part_tbl%nrank_import)
        part_tbl%istack_import(1:part_tbl%nrank_import)                 &
     &          = import_IO%istack_import(1:part_tbl%nrank_import)
!$omp end parallel workshare
      end if
!
      part_tbl%ntot_import = import_IO%ntot_import
      call alloc_calypso_import_item(part_tbl)
!
      if(part_tbl%ntot_import .gt. 0) then
!$omp parallel workshare
        part_tbl%item_import(1:part_tbl%ntot_import)                    &
     &          =  import_IO%item_import(1:part_tbl%ntot_import)
!$omp end parallel workshare
      end if
!
      nnod_rev = maxval(part_tbl%item_import)
      call alloc_calypso_import_rev(nnod_rev, part_tbl)
      call set_calypso_import_rev(part_tbl, ierr)
!
      end subroutine import_IO_to_repart_table
!
!-----------------------------------------------------------------------
!
      end module copy_repart_table_for_IO

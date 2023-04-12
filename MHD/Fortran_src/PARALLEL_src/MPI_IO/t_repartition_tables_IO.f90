!>@file  t_repartition_tables_IO.f90
!!      module t_repartition_tables_IO
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2022
!
!>@brief Structures for repartition table IO
!!
!!@verbatim
!!      subroutine dealloc_repartition_tables_IO(repart_IOs)
!!        type(repartition_tables_IO), intent(inout) :: repart_IOs
!!      subroutine copy_repart_tbl_to_repart_IOs                        &
!!     &         (nod_repart_tbl, ele_repart_tbl,                       &
!!     &          new_nod_comm, new_ele_comm, repart_IOs)
!!        type(calypso_comm_table), intent(in) :: nod_repart_tbl
!!        type(calypso_comm_table), intent(in) :: ele_repart_tbl
!!        type(communication_table), intent(in) :: new_nod_comm
!!        type(communication_table), intent(in) :: new_ele_comm
!!        type(repartition_tables_IO), intent(inout) :: repart_IOs
!!      subroutine copy_repart_IOs_to_repart_tbl(irank_read, repart_IOs,&
!!     &          nod_repart_tbl, ele_repart_tbl,                       &
!!     &          new_nod_comm, new_ele_comm, ierr)
!!        integer(kind= kint), intent(in) :: irank_read
!!        type(repartition_tables_IO), intent(in) :: repart_IOs
!!        type(calypso_comm_table), intent(inout) :: nod_repart_tbl
!!        type(calypso_comm_table), intent(inout) :: ele_repart_tbl
!!        type(communication_table), intent(inout) :: new_nod_comm
!!        type(communication_table), intent(inout) :: new_ele_comm
!!        integer(kind = kint), intent(inout) :: ierr
!!@endverbatim
!!
!!@param id_rank  MPI rank
!
      module t_repartition_tables_IO
!
      use m_precision
      use m_machine_parameter
!
      use t_comm_table
      use t_calypso_comm_table
!
      implicit none
!
!>      Structures for repartition table IO
      type repartition_tables_IO
!>        Send table to repartitioned node address
        type(communication_table) :: nod_repart_import
!>        Send table from original node address
        type(communication_table) :: nod_repart_export
!
!>        Recieve table to repartitioned element address
        type(communication_table) :: ele_repart_import
!>        Recieve table from original element address
        type(communication_table) :: ele_repart_export
!
!>        Communication table fe repartitioned node
        type(communication_table) :: nod_comm_IO
!>        Communication table fe repartitioned element
        type(communication_table) :: ele_comm_IO
      end type repartition_tables_IO
!
!-----------------------------------------------------------------------
!
       contains
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_repartition_tables_IO(repart_IOs)
!
      type(repartition_tables_IO), intent(inout) :: repart_IOs
!
!
      call dealloc_export_table(repart_IOs%nod_repart_export)
      call dealloc_neib_id(repart_IOs%nod_repart_export)
!
      call dealloc_import_table(repart_IOs%nod_repart_import)
      call dealloc_neib_id(repart_IOs%nod_repart_import)
!
      call dealloc_export_table(repart_IOs%ele_repart_export)
      call dealloc_neib_id(repart_IOs%ele_repart_export)
!
      call dealloc_import_table(repart_IOs%ele_repart_import)
      call dealloc_neib_id(repart_IOs%ele_repart_import)
!
!
      call dealloc_export_table(repart_IOs%nod_comm_IO)
      call dealloc_import_table(repart_IOs%nod_comm_IO)
      call dealloc_neib_id(repart_IOs%nod_comm_IO)
!
      call dealloc_export_table(repart_IOs%ele_comm_IO)
      call dealloc_import_table(repart_IOs%ele_comm_IO)
      call dealloc_neib_id(repart_IOs%ele_comm_IO)
!
      end subroutine dealloc_repartition_tables_IO
!
!-----------------------------------------------------------------------
!
      subroutine copy_repart_tbl_to_repart_IOs                          &
     &         (nod_repart_tbl, ele_repart_tbl,                         &
     &          new_nod_comm, new_ele_comm, repart_IOs)
!
      use copy_repart_table_for_IO
!
      type(calypso_comm_table), intent(in) :: nod_repart_tbl
      type(calypso_comm_table), intent(in) :: ele_repart_tbl
      type(communication_table), intent(in) :: new_nod_comm
      type(communication_table), intent(in) :: new_ele_comm
!
      type(repartition_tables_IO), intent(inout) :: repart_IOs
!
!
      call repart_table_to_export_IO                                    &
     &   (nod_repart_tbl, repart_IOs%nod_repart_export)
      call repart_table_to_import_IO                                    &
     &   (nod_repart_tbl, repart_IOs%nod_repart_import)
!
      call repart_table_to_export_IO                                    &
     &   (ele_repart_tbl, repart_IOs%ele_repart_export)
      call repart_table_to_import_IO                                    &
     &   (ele_repart_tbl, repart_IOs%ele_repart_import)
!
      call copy_comm_tbl_type(new_nod_comm, repart_IOs%nod_comm_IO)
      call copy_comm_tbl_type(new_ele_comm, repart_IOs%ele_comm_IO)
!
      end subroutine copy_repart_tbl_to_repart_IOs
!
!-----------------------------------------------------------------------
!
      subroutine copy_repart_IOs_to_repart_tbl(irank_read, repart_IOs,  &
     &          nod_repart_tbl, ele_repart_tbl,                         &
     &          new_nod_comm, new_ele_comm, ierr)
!
      use copy_repart_table_for_IO
!
      integer(kind= kint), intent(in) :: irank_read
      type(repartition_tables_IO), intent(in) :: repart_IOs
!
      type(calypso_comm_table), intent(inout) :: nod_repart_tbl
      type(calypso_comm_table), intent(inout) :: ele_repart_tbl
      type(communication_table), intent(inout) :: new_nod_comm
      type(communication_table), intent(inout) :: new_ele_comm
      integer(kind = kint), intent(inout) :: ierr
!
!
      call export_IO_to_repart_table                                    &
     &   (irank_read, repart_IOs%nod_repart_export, nod_repart_tbl)
      call import_IO_to_repart_table(repart_IOs%nod_repart_import,      &
     &    nod_repart_tbl, ierr)
       if(ierr .gt. 0) return
!
      call export_IO_to_repart_table                                    &
     &   (irank_read, repart_IOs%ele_repart_export, ele_repart_tbl)
      call import_IO_to_repart_table(repart_IOs%ele_repart_import,      &
     &    ele_repart_tbl, ierr)
       if(ierr .gt. 0) return
!
      call copy_comm_tbl_type(repart_IOs%nod_comm_IO, new_nod_comm)
      call copy_comm_tbl_type(repart_IOs%ele_comm_IO, new_ele_comm)
!
      end subroutine copy_repart_IOs_to_repart_tbl
!
!-----------------------------------------------------------------------
!
      end module t_repartition_tables_IO

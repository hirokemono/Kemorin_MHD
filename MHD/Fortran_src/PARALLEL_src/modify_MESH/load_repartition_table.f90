!>@file  load_repartition_table.f90
!!      module load_repartition_table
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2022
!
!>@brief Structures for repartition table IO
!!
!!@verbatim
!!      subroutine output_repart_table(repart_file, new_numele,         &
!!     &          nod_repart_tbl, ele_repart_tbl,                       &
!!     &          new_nod_comm, new_ele_comm)
!!        type(field_IO_params), intent(in) ::  repart_file
!!        type(calypso_comm_table), intent(in) :: nod_repart_tbl
!!        type(calypso_comm_table), intent(in) :: ele_repart_tbl
!!        type(communication_table), intent(in) :: new_nod_comm
!!        type(communication_table), intent(in) :: new_ele_comm
!!        integer(kind = kint), intent(in) :: new_numele
!!      subroutine s_load_repartition_table(repart_file,                &
!!     &          new_numele, nod_repart_tbl, ele_repart_tbl,           &
!!     &          new_nod_comm, new_ele_comm)
!!        type(field_IO_params), intent(in) ::  repart_file
!!        integer(kind = kint), intent(inout) :: new_numele
!!        type(calypso_comm_table), intent(inout) :: nod_repart_tbl
!!        type(calypso_comm_table), intent(inout) :: ele_repart_tbl
!!        type(communication_table), intent(inout) :: new_nod_comm
!!        type(communication_table), intent(inout) :: new_ele_comm
!!@endverbatim
!!
      module load_repartition_table
!
      use m_precision
      use m_machine_parameter
      use calypso_mpi
!
      use t_file_IO_parameter
      use t_comm_table
      use t_calypso_comm_table
      use t_repartition_tables_IO
!
      implicit none
!
!-----------------------------------------------------------------------
!
       contains
!
!-----------------------------------------------------------------------
!
      subroutine output_repart_table(repart_file, new_numele,           &
     &          nod_repart_tbl, ele_repart_tbl,                         &
     &          new_nod_comm, new_ele_comm)
!
      use sel_repartition_table_IO
!
      integer(kind = kint), intent(in) :: new_numele
      type(field_IO_params), intent(in) ::  repart_file
      type(calypso_comm_table), intent(in) :: nod_repart_tbl
      type(calypso_comm_table), intent(in) :: ele_repart_tbl
      type(communication_table), intent(in) :: new_nod_comm
      type(communication_table), intent(in) :: new_ele_comm
!
      type(repartition_tables_IO) :: repart_IOs
!
!
      call copy_repart_tbl_to_repart_IOs                                &
     &   (new_numele, nod_repart_tbl, ele_repart_tbl,                   &
     &    new_nod_comm, new_ele_comm, repart_IOs)
      call sel_mpi_write_repart_tbl_file(repart_file, repart_IOs)
      call dealloc_repartition_tables_IO(repart_IOs)
!
      end subroutine output_repart_table
!
!-----------------------------------------------------------------------
!
      subroutine s_load_repartition_table(repart_file,                  &
     &          new_numele, nod_repart_tbl, ele_repart_tbl,             &
     &          new_nod_comm, new_ele_comm)
!
      use m_error_IDs
      use sel_repartition_table_IO
!
      type(field_IO_params), intent(in) ::  repart_file
!
      integer(kind = kint), intent(inout) :: new_numele
      type(calypso_comm_table), intent(inout) :: nod_repart_tbl
      type(calypso_comm_table), intent(inout) :: ele_repart_tbl
      type(communication_table), intent(inout) :: new_nod_comm
      type(communication_table), intent(inout) :: new_ele_comm
!
      type(repartition_tables_IO) :: repart_IOs
      integer(kind= kint) :: ierr
!
!
      call sel_mpi_read_repart_tbl_file(repart_file, repart_IOs)
      call copy_repart_IOs_to_repart_tbl(my_rank, repart_IOs,           &
     &    new_numele, nod_repart_tbl, ele_repart_tbl,                   &
     &    new_nod_comm, new_ele_comm, ierr)
      call dealloc_repartition_tables_IO(repart_IOs)
!
      if(ierr .gt. 0) then
        call calypso_mpi_abort(ierr_repart,                             &
     &                         'Failed repatition table loading')
      end if
!
      end subroutine s_load_repartition_table
!
!-----------------------------------------------------------------------
!
      end module load_repartition_table

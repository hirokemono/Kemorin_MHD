!>@file   gz_MPI_repart_table_file_IO.f90
!!      module gz_MPI_repart_table_file_IO
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2022
!
!>@brief File IO for communication table
!!
!!@verbatim
!!      subroutine gz_mpi_read_repart_tbl_file                          &
!!     &         (num_pe, id_rank, file_name, repart_IOs)
!!        integer, intent(in) :: num_pe, id_rank
!!        character(len=kchara), intent(in) :: file_name
!!        type(repartition_tables_IO), intent(inout) :: repart_IOs
!!      subroutine gz_mpi_write_repart_tbl_file(file_name, repart_IOs)
!!        character(len=kchara), intent(in) :: file_name
!!        type(repartition_tables_IO), intent(in) :: repart_IOs
!!
!!      subroutine gz_mpi_read_repart_tbl_file_b                        &
!!     &         (num_pe, id_rank, file_name, repart_IOs)
!!        integer, intent(in) :: num_pe, id_rank
!!        character(len=kchara), intent(in) :: file_name
!!        type(repartition_tables_IO), intent(inout) :: repart_IOs
!!      subroutine gz_mpi_write_repart_tbl_file_b(file_name, repart_IOs)
!!        character(len=kchara), intent(in) :: file_name
!!        type(repartition_tables_IO), intent(in) :: repart_IOs
!!@endverbatim
!!
!!@param id_rank  MPI rank
!
      module gz_MPI_repart_table_file_IO
!
      use m_precision
      use m_machine_parameter
      use t_calypso_mpi_IO_param
      use t_repartition_tables_IO
!
      implicit none
!
      type(calypso_MPI_IO_params), save, private :: IO_param
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_repart_tbl_file                            &
     &         (num_pe, id_rank, file_name, repart_IOs)
!
      use gz_MPI_comm_table_IO
!
      integer, intent(in) :: num_pe, id_rank
      character(len=kchara), intent(in) :: file_name
      type(repartition_tables_IO), intent(inout) :: repart_IOs
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read gzipped merged ascii element comm file: ',                &
     &   trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, num_pe, id_rank, IO_param)
      call gz_mpi_read_calypso_comm_tbl(IO_param,                       &
     &    repart_IOs%nod_repart_import, repart_IOs%nod_repart_export)
      call gz_mpi_read_calypso_comm_tbl(IO_param,                       &
     &    repart_IOs%ele_repart_import, repart_IOs%ele_repart_export)
!
      call gz_mpi_read_comm_table(IO_param, repart_IOs%nod_comm_IO)
      call gz_mpi_read_comm_table(IO_param, repart_IOs%ele_comm_IO)
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_read_repart_tbl_file
!
!------------------------------------------------------------------
!
      subroutine gz_mpi_write_repart_tbl_file(file_name, repart_IOs)
!
      use gz_MPI_comm_table_IO
!
      character(len=kchara), intent(in) :: file_name
      type(repartition_tables_IO), intent(in) :: repart_IOs
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write gzipped merged ascii element comm file: ',               &
     &   trim(file_name)
!
      call open_write_mpi_file(file_name, IO_param)
      call gz_mpi_write_calypso_comm_tbl(IO_param,                      &
     &    repart_IOs%nod_repart_import, repart_IOs%nod_repart_export)
      call gz_mpi_write_calypso_comm_tbl(IO_param,                      &
     &    repart_IOs%ele_repart_import, repart_IOs%ele_repart_export)
!
      call gz_mpi_write_comm_table(IO_param, repart_IOs%nod_comm_IO)
      call gz_mpi_write_comm_table(IO_param, repart_IOs%ele_comm_IO)
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_write_repart_tbl_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine gz_mpi_read_repart_tbl_file_b                          &
     &         (num_pe, id_rank, file_name, repart_IOs)
!
      use gz_MPI_comm_table_IO_b
!
      integer, intent(in) :: num_pe, id_rank
      character(len=kchara), intent(in) :: file_name
      type(repartition_tables_IO), intent(inout) :: repart_IOs
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read gzipped merged binary element comm file: ',               &
     &   trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, num_pe, id_rank, IO_param)
      call gz_mpi_read_calypso_comm_tbl_b(IO_param,                     &
     &    repart_IOs%nod_repart_import, repart_IOs%nod_repart_export)
      call gz_mpi_read_calypso_comm_tbl_b(IO_param,                     &
     &    repart_IOs%ele_repart_import, repart_IOs%ele_repart_export)
!
      call gz_mpi_read_comm_table_b(IO_param, repart_IOs%nod_comm_IO)
      call gz_mpi_read_comm_table_b(IO_param, repart_IOs%ele_comm_IO)
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_read_repart_tbl_file_b
!
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_write_repart_tbl_file_b(file_name, repart_IOs)
!
      use gz_MPI_comm_table_IO_b
!
      character(len=kchara), intent(in) :: file_name
      type(repartition_tables_IO), intent(in) :: repart_IOs
!
!
      call open_write_mpi_file(file_name, IO_param)
      call gz_mpi_write_calypso_comm_tbl_b(IO_param,                    &
     &    repart_IOs%nod_repart_import, repart_IOs%nod_repart_export)
      call gz_mpi_write_calypso_comm_tbl_b(IO_param,                    &
     &    repart_IOs%ele_repart_import, repart_IOs%ele_repart_export)
!
      call gz_mpi_write_comm_table_b(IO_param, repart_IOs%nod_comm_IO)
      call gz_mpi_write_comm_table_b(IO_param, repart_IOs%ele_comm_IO)
      call close_mpi_file(IO_param)
!
!
      end subroutine gz_mpi_write_repart_tbl_file_b
!
!------------------------------------------------------------------
!
      end module gz_MPI_repart_table_file_IO

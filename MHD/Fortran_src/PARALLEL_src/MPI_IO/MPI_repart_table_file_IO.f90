!>@file   MPI_repart_table_file_IO.f90
!!      module MPI_repart_table_file_IO
!!
!!@author  H. Matsui
!!@date Programmed in Aug., 2022
!
!>@brief File IO for communication table
!!
!!@verbatim
!!      subroutine mpi_read_repart_tbl_file                             &
!!     &         (num_pe, id_rank, file_name, repart_IOs)
!!        integer, intent(in) :: num_pe, id_rank
!!        character(len=kchara), intent(in) :: file_name
!!        type(repartition_tables_IO), intent(inout) :: repart_IOs
!!      subroutine mpi_write_repart_tbl_file(file_name, repart_IOs)
!!        character(len=kchara), intent(in) :: file_name
!!        type(repartition_tables_IO), intent(in) :: repart_IOs
!!
!!      subroutine mpi_read_repart_tbl_file_b                           &
!!     &         (num_pe, id_rank, file_name, repart_IOs)
!!        integer, intent(in) :: num_pe, id_rank
!!        character(len=kchara), intent(in) :: file_name
!!        type(repartition_tables_IO), intent(inout) :: repart_IOs
!!      subroutine mpi_write_repart_tbl_file_b(file_name, repart_IOs)
!!        character(len=kchara), intent(in) :: file_name
!!        type(repartition_tables_IO), intent(in) :: repart_IOs
!!@endverbatim
!!
!!@param id_rank  MPI rank
!
      module MPI_repart_table_file_IO
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
      subroutine mpi_read_repart_tbl_file                               &
     &         (num_pe, id_rank, file_name, repart_IOs)
!
      use MPI_comm_table_IO
      use MPI_ascii_data_IO
      use m_fem_mesh_labels
!
      integer, intent(in) :: num_pe, id_rank
      character(len=kchara), intent(in) :: file_name
      type(repartition_tables_IO), intent(inout) :: repart_IOs
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read merged ascii element comm file: ', trim(file_name)
!
      call open_read_mpi_file                                           &
     &   (file_name, num_pe, id_rank, IO_param)
      call mpi_read_calypso_comm_tbl(IO_param,                          &
     &    repart_IOs%nod_repart_import, repart_IOs%nod_repart_export)
!
      call mpi_skip_read(IO_param, len(hd_fem_elem()))
      call mpi_read_num_of_data(IO_param, repart_IOs%new_numele)
      call mpi_read_calypso_comm_tbl(IO_param,                          &
     &    repart_IOs%ele_repart_import, repart_IOs%ele_repart_export)
!
      call mpi_read_comm_table(IO_param, repart_IOs%nod_comm_IO)
      call mpi_read_comm_table(IO_param, repart_IOs%ele_comm_IO)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_read_repart_tbl_file
!
!------------------------------------------------------------------
!
      subroutine mpi_write_repart_tbl_file(file_name, repart_IOs)
!
      use MPI_comm_table_IO
      use MPI_ascii_data_IO
      use m_fem_mesh_labels
!
      character(len=kchara), intent(in) :: file_name
      type(repartition_tables_IO), intent(in) :: repart_IOs
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write merged ascii element comm file: ', trim(file_name)
!
      call open_write_mpi_file(file_name, IO_param)
      call mpi_write_calypso_comm_tbl(IO_param,                         &
     &    repart_IOs%ele_repart_import, repart_IOs%ele_repart_export)
!
      call mpi_write_charahead                                          &
     &   (IO_param, len(hd_fem_elem()), hd_fem_elem())
      call mpi_write_num_of_data(IO_param, repart_IOs%new_numele)
      call mpi_write_calypso_comm_tbl(IO_param,                         &
     &    repart_IOs%ele_repart_import, repart_IOs%ele_repart_export)
!
      call mpi_write_comm_table(IO_param, repart_IOs%nod_comm_IO)
      call mpi_write_comm_table(IO_param, repart_IOs%ele_comm_IO)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_write_repart_tbl_file
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      subroutine mpi_read_repart_tbl_file_b                             &
     &         (num_pe, id_rank, file_name, repart_IOs)
!
      use MPI_comm_table_IO_b
      use MPI_ascii_data_IO
!
      integer, intent(in) :: num_pe, id_rank
      character(len=kchara), intent(in) :: file_name
      type(repartition_tables_IO), intent(inout) :: repart_IOs
!
!
      if(id_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Read merged binary element comm file: ', trim(file_name)
!
      call open_read_mpi_file_b(file_name, num_pe, id_rank, IO_param)
      call mpi_read_calypso_comm_tbl_b(IO_param,                        &
     &    repart_IOs%nod_repart_import, repart_IOs%nod_repart_export)
!
      call mpi_read_one_integer_b(IO_param, repart_IOs%new_numele)
      call mpi_read_calypso_comm_tbl_b(IO_param,                        &
     &    repart_IOs%ele_repart_import, repart_IOs%ele_repart_export)
!
      call mpi_read_comm_table_b(IO_param, repart_IOs%nod_comm_IO)
      call mpi_read_comm_table_b(IO_param, repart_IOs%ele_comm_IO)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_read_repart_tbl_file_b
!
!------------------------------------------------------------------
!
      subroutine mpi_write_repart_tbl_file_b(file_name, repart_IOs)
!
      use MPI_comm_table_IO_b
      use MPI_ascii_data_IO
!
      character(len=kchara), intent(in) :: file_name
      type(repartition_tables_IO), intent(in) :: repart_IOs
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &  'Write merged binary element comm file: ', trim(file_name)
!
      call open_write_mpi_file_b(file_name, IO_param)
      call mpi_write_calypso_comm_tbl_b(IO_param,                       &
     &    repart_IOs%nod_repart_import, repart_IOs%nod_repart_export)
!
      call mpi_write_one_integer_b(IO_param, repart_IOs%new_numele)
      call mpi_write_calypso_comm_tbl_b(IO_param,                       &
     &    repart_IOs%ele_repart_import, repart_IOs%ele_repart_export)
!
      call mpi_write_comm_table_b(IO_param, repart_IOs%nod_comm_IO)
      call mpi_write_comm_table_b(IO_param, repart_IOs%ele_comm_IO)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_write_repart_tbl_file_b
!
!------------------------------------------------------------------
!
      end module MPI_repart_table_file_IO

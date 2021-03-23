!>@file  gz_MPI_itp_table_file_IO_b.f90
!!       module gz_MPI_itp_table_file_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in Sep. 2012
!!
!> @brief gzipped binary file IO for interpolation
!!
!!@verbatim
!!      subroutine write_gz_mpi_itp_table_file_b(gzip_name, itp_tbl_IO)
!!        type(interpolate_table), intent(in) :: itp_tbl_IO
!!      subroutine write_gz_mpi_dbl_itp_tbl_file_b                      &
!!     &         (gzip_name, itp_tbl1_IO, itp_tbl2_IO)
!!        type(interpolate_table), intent(inout) :: itp_tbl1_IO
!!        type(interpolate_table), intent(inout) :: itp_tbl2_IO
!!
!!      subroutine read_gz_mpi_itp_table_file_b                         &
!!     &          (gzip_name, id_rank, num_pe, itp_tbl_IO)
!!        type(interpolate_table), intent(inout) :: itp_tbl_IO
!!      subroutine read_gz_mpi_dbl_itp_tbl_file_b                       &
!!     &          (gzip_name, id_rank, num_pe, itp_tbl1_IO, itp_tbl2_IO)
!!        type(interpolate_table), intent(inout) :: itp_tbl1_IO
!!        type(interpolate_table), intent(inout) :: itp_tbl2_IO
!!@endverbatim
!
      module gz_MPI_itp_table_file_IO_b
!
      use m_precision
      use m_error_IDs
!
      use calypso_mpi
      use t_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
      use t_binary_IO_buffer
      use t_calypso_mpi_IO_param
!
      use t_buffer_4_gzip
!
      implicit none
!
      type(calypso_MPI_IO_params), save, private :: IO_param1
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_mpi_itp_table_file_b(gzip_name, itp_tbl_IO)
!
      use gz_MPI_itp_table_data_IO_b
      use gz_MPI_binary_datum_IO
!
      character(len=kchara), intent(in) :: gzip_name
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write merged  gzipped binary interpolation file: ',           &
     &    trim(gzip_name)
!
      call open_write_gz_mpi_file_b(gzip_name, IO_param1)
      call write_gz_mpi_each_itp_table_b(IO_param1, itp_tbl_IO)
      call close_mpi_file(IO_param1)
!
      end subroutine write_gz_mpi_itp_table_file_b
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_mpi_dbl_itp_tbl_file_b                        &
     &         (gzip_name, itp_tbl1_IO, itp_tbl2_IO)
!
      use gz_MPI_itp_table_data_IO_b
      use gz_MPI_binary_datum_IO
!
      character(len=kchara), intent(in) :: gzip_name
!
      type(interpolate_table), intent(inout) :: itp_tbl1_IO
      type(interpolate_table), intent(inout) :: itp_tbl2_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write merged  gzipped binary interpolation file: ',           &
     &    trim(gzip_name)
!
      call open_write_gz_mpi_file_b(gzip_name, IO_param1)
      call write_gz_mpi_each_itp_table_b(IO_param1, itp_tbl1_IO)
      call write_gz_mpi_each_itp_table_b(IO_param1, itp_tbl2_IO)
      call close_mpi_file(IO_param1)
!
      end subroutine write_gz_mpi_dbl_itp_tbl_file_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_gz_mpi_itp_table_file_b                           &
     &          (gzip_name, id_rank, num_pe, itp_tbl_IO)
!
      use gz_MPI_itp_table_data_IO_b
      use gz_MPI_binary_datum_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank, num_pe
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read merged gzipped binary interpolation file: ',             &
     &    trim(gzip_name)
!
      call open_read_gz_mpi_file_b                                      &
     &   (gzip_name, num_pe, id_rank, IO_param1)
      call read_gz_mpi_each_itp_table_b(IO_param1, itp_tbl_IO)
      call close_mpi_file(IO_param1)
!
      end subroutine read_gz_mpi_itp_table_file_b
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_mpi_dbl_itp_tbl_file_b                         &
     &          (gzip_name, id_rank, num_pe, itp_tbl1_IO, itp_tbl2_IO)
!
      use gz_MPI_itp_table_data_IO_b
      use gz_MPI_binary_datum_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank, num_pe
!
      type(interpolate_table), intent(inout) :: itp_tbl1_IO
      type(interpolate_table), intent(inout) :: itp_tbl2_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read merged gzipped binary interpolation file: ',             &
     &    trim(gzip_name)
!
      call open_read_gz_mpi_file_b                                      &
     &   (gzip_name, num_pe, id_rank, IO_param1)
      call read_gz_mpi_each_itp_table_b(IO_param1, itp_tbl1_IO)
      call read_gz_mpi_each_itp_table_b(IO_param1, itp_tbl2_IO)
      call close_mpi_file(IO_param1)
!
      end subroutine read_gz_mpi_dbl_itp_tbl_file_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_gz_mpi_each_itp_table_b(IO_param, itp_tbl_IO)
!
      use gz_MPI_itp_table_data_IO_b
      use gz_MPI_binary_datum_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table), intent(inout) :: itp_tbl_IO
!
!
      call gz_mpi_write_itp_domain_dest_b                               &
     &   (IO_param, itp_tbl_IO%tbl_dest)
      call gz_mpi_write_itp_table_dest_b(IO_param, itp_tbl_IO%tbl_dest)
!
      call gz_mpi_write_itp_domain_org_b(IO_param, itp_tbl_IO%tbl_org)
      call gz_mpi_write_itp_table_org_b(IO_param, itp_tbl_IO%tbl_org)
      call gz_mpi_write_itp_coefs_org_b(IO_param, itp_tbl_IO%tbl_org)
!
      end subroutine write_gz_mpi_each_itp_table_b
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_mpi_each_itp_table_b(IO_param, itp_tbl_IO)
!
      use gz_MPI_itp_table_data_IO_b
      use gz_MPI_binary_datum_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table), intent(inout) :: itp_tbl_IO
!
!
      call gz_mpi_read_itp_domain_dest_b(IO_param, itp_tbl_IO%tbl_dest)
      call gz_mpi_read_itp_table_dest_b(IO_param, itp_tbl_IO%tbl_dest)
!
      call gz_mpi_read_itp_domain_org_b(IO_param, itp_tbl_IO%tbl_org)
      call gz_mpi_read_itp_table_org_b(IO_param, itp_tbl_IO%tbl_org)
      call gz_mpi_read_itp_coefs_org_b(IO_param, itp_tbl_IO%tbl_org)
!
      end subroutine read_gz_mpi_each_itp_table_b
!
!-----------------------------------------------------------------------
!
      end module gz_MPI_itp_table_file_IO_b

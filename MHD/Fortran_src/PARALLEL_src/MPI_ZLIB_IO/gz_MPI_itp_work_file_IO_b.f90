!>@file  gz_MPI_itp_work_file_IO_b.f90
!!       module gz_MPI_itp_work_file_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in Sep. 2012
!!
!> @brief gzipped binary file IO for interpolation
!!
!!@verbatim
!!      subroutine wrt_gz_mpi_itp_coef_dest_file_b                      &
!!     &        (gzip_name, id_rank, IO_itp_dest, IO_itp_c_dest)
!!      subroutine read_gz_mpi_itp_coef_dst_file_b                      &
!!     &        (gzip_name, id_rank, num_pe, IO_itp_dest, IO_itp_c_dest)
!!      subroutine read_gz_mpi_itp_tbl_dest_file_b                      &
!!     &         (gzip_name, id_rank, num_pe, IO_itp_dest)
!!      subroutine read_gz_mpi_itp_dmn_dest_file_b                      &
!!     &         (gzip_name, id_rank, num_pe, IO_itp_dest)
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!!@endverbatim
!
      module gz_MPI_itp_work_file_IO_b
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
      type(calypso_MPI_IO_params), save, private :: IO_param
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine wrt_gz_mpi_itp_coef_dest_file_b                        &
     &         (gzip_name, id_rank, IO_itp_dest, IO_itp_c_dest)
!
      use gz_MPI_itp_table_data_IO_b
      use gz_MPI_binary_datum_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write gzipped binary export coefs file: ', trim(gzip_name)
!
      call open_write_gz_mpi_file_b(gzip_name, IO_param)
      call gz_mpi_write_itp_domain_dest_b(IO_param, IO_itp_dest)
      call gz_mpi_write_itp_table_dest_b(IO_param, IO_itp_dest)
      call gz_mpi_write_itp_coefs_dest_b                                &
     &   (IO_param, IO_itp_dest, IO_itp_c_dest)
      call close_mpi_file(IO_param)
!
      end subroutine wrt_gz_mpi_itp_coef_dest_file_b
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_mpi_itp_coef_dst_file_b                        &
     &        (gzip_name, id_rank, num_pe, IO_itp_dest, IO_itp_c_dest)
!
      use gz_MPI_itp_table_data_IO_b
      use gz_MPI_binary_datum_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank, num_pe
!
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
! 
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary export coefs file: ', trim(gzip_name)
!
      call gz_mpi_read_itp_domain_dest_b(IO_param, IO_itp_dest)
      call gz_mpi_read_itp_table_dest_b(IO_param, IO_itp_dest)
      call gz_mpi_read_itp_coefs_dest_b                                 &
     &   (IO_param, IO_itp_dest, IO_itp_c_dest)
      call close_mpi_file(IO_param)
!
      end subroutine read_gz_mpi_itp_coef_dst_file_b
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_mpi_itp_tbl_dest_file_b                        &
     &         (gzip_name, id_rank, num_pe, IO_itp_dest)
!
      use gz_MPI_itp_table_data_IO_b
      use gz_MPI_binary_datum_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank, num_pe
!
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary interapolate export file: ',              &
     &    trim(gzip_name)
!
      call open_read_gz_mpi_file_b                                      &
     &   (gzip_name, num_pe, id_rank, IO_param)
      call gz_mpi_read_itp_domain_dest_b(IO_param, IO_itp_dest)
      call gz_mpi_read_itp_table_dest_b(IO_param, IO_itp_dest)
      call close_mpi_file(IO_param)
!
      end subroutine read_gz_mpi_itp_tbl_dest_file_b
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_mpi_itp_dmn_dest_file_b                        &
     &         (gzip_name, id_rank, num_pe, IO_itp_dest)
!
      use gz_MPI_itp_table_data_IO_b
      use gz_MPI_binary_datum_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank, num_pe
!
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary export domain file: ', trim(gzip_name)
!
      call open_read_gz_mpi_file_b                                      &
     &   (gzip_name, num_pe, id_rank, IO_param)
      call gz_mpi_read_itp_domain_dest_b(IO_param, IO_itp_dest)
      call close_mpi_file(IO_param)
!
      end subroutine read_gz_mpi_itp_dmn_dest_file_b
!
!-----------------------------------------------------------------------
!
      end module gz_MPI_itp_work_file_IO_b

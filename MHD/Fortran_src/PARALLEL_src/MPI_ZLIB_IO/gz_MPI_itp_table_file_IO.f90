!>@file  gz_MPI_itp_table_file_IO.f90
!!       module gz_MPI_itp_table_file_IO
!!
!!@author H. Matsui
!!@date   Programmed in Sep. 2012
!!
!> @brief gzipped file IO for interpolation
!!
!!@verbatim
!!      subroutine gz_mpi_write_itp_table_file(gzip_name, itp_tbl_IO)
!!      subroutine gz_mpi_read_itp_table_file                           &
!!     &         (gzip_name, id_rank, num_pe, itp_tbl_IO)
!!        type(interpolate_table), intent(inout) :: itp_tbl_IO
!!
!!      subroutine gz_mpi_wrt_itp_coefs_dest_file                       &
!!     &         (gzip_name, IO_itp_dest, IO_itp_c_dest)
!!      subroutine gz_mpi_read_itp_coefs_dest_file                      &
!!     &        (gzip_name, id_rank, num_pe, IO_itp_dest, IO_itp_c_dest)
!!      subroutine gz_mpi_read_itp_tbl_dest_file                        &
!!     &         (gzip_name, id_rank, num_pe, IO_itp_dest)
!!      subroutine gz_mpi_read_itp_dmn_dest_file                        &
!!     &         (gzip_name, id_rank, num_pe, IO_itp_dest)
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!!@endverbatim
!
      module gz_MPI_itp_table_file_IO
!
      use m_precision
      use m_error_IDs
!
      use t_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
      use t_calypso_mpi_IO_param
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
      subroutine gz_mpi_write_itp_table_file(gzip_name, itp_tbl_IO)
!
      use gz_MPI_itp_table_data_IO
      use gz_MPI_binary_datum_IO
!
      character(len=kchara), intent(in) :: gzip_name
      type(interpolate_table), intent(in) :: itp_tbl_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write merged gzipped interpolation file: ', trim(gzip_name)
!
      call open_write_mpi_file(gzip_name, IO_param)
!
      call gz_mpi_write_itp_domain_dest(IO_param, itp_tbl_IO%tbl_dest)
      call gz_mpi_write_itp_table_dest(IO_param, itp_tbl_IO%tbl_dest)
!
      call gz_mpi_write_itp_domain_org(IO_param, itp_tbl_IO%tbl_org)
      call gz_mpi_write_itp_table_org(IO_param, itp_tbl_IO%tbl_org)
      call gz_mpi_write_itp_coefs_org(IO_param, itp_tbl_IO%tbl_org)
!
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_write_itp_table_file
!
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_read_itp_table_file                             &
     &         (gzip_name, id_rank, num_pe, itp_tbl_IO)
!
      use gz_MPI_itp_table_data_IO
      use gz_MPI_binary_datum_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank, num_pe
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read merged gzipped interpolation file: ', trim(gzip_name)
!
      call open_read_mpi_file                                           &
     &   (gzip_name, num_pe, id_rank, IO_param)
!
      call gz_mpi_read_itp_domain_dest(IO_param, itp_tbl_IO%tbl_dest)
      call gz_mpi_read_itp_table_dest(IO_param, itp_tbl_IO%tbl_dest)
!
      call gz_mpi_read_itp_domain_org(IO_param, itp_tbl_IO%tbl_org)
      call gz_mpi_read_itp_table_org(IO_param, itp_tbl_IO%tbl_org)
      call gz_mpi_read_itp_coefs_org(IO_param, itp_tbl_IO%tbl_org)
!
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_read_itp_table_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_wrt_itp_coefs_dest_file                         &
     &         (gzip_name, IO_itp_dest, IO_itp_c_dest)
!
      use gz_MPI_itp_table_data_IO
      use gz_MPI_binary_datum_IO
!
      character(len=kchara), intent(in) :: gzip_name
!
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write binary export coefs file: ', trim(gzip_name)
!
      call open_write_mpi_file(gzip_name, IO_param)
!
      call gz_mpi_write_itp_domain_dest(IO_param, IO_itp_dest)
      call gz_mpi_write_itp_table_dest(IO_param, IO_itp_dest)
      call gz_mpi_write_itp_coefs_dest                                  &
     &   (IO_param, IO_itp_dest, IO_itp_c_dest)
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_wrt_itp_coefs_dest_file
!
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_read_itp_coefs_dest_file                        &
     &        (gzip_name, id_rank, num_pe, IO_itp_dest, IO_itp_c_dest)
!
      use gz_MPI_itp_table_data_IO
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
     &   'Read gzipped export coefs file: ', trim(gzip_name)
!
      call open_read_mpi_file                                           &
     &   (gzip_name, num_pe, id_rank, IO_param)
!
      call gz_mpi_read_itp_domain_dest(IO_param, IO_itp_dest)
      call gz_mpi_read_itp_table_dest(IO_param, IO_itp_dest)
      call gz_mpi_read_itp_coefs_dest                                   &
     &   (IO_param, IO_itp_dest, IO_itp_c_dest)
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_read_itp_coefs_dest_file
!
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_read_itp_tbl_dest_file                          &
     &         (gzip_name, id_rank, num_pe, IO_itp_dest)
!
      use gz_MPI_itp_table_data_IO
      use gz_MPI_binary_datum_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank, num_pe
!
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped interapolate export file: ', trim(gzip_name)
!
      call open_read_mpi_file                                           &
     &   (gzip_name, num_pe, id_rank, IO_param)
!
      call gz_mpi_read_itp_domain_dest(IO_param, IO_itp_dest)
      call gz_mpi_read_itp_table_dest(IO_param, IO_itp_dest)
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_read_itp_tbl_dest_file
!
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_read_itp_dmn_dest_file                          &
     &         (gzip_name, id_rank, num_pe, IO_itp_dest)
!
      use gz_MPI_itp_table_data_IO
      use gz_MPI_binary_datum_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank, num_pe
!
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped export domain file: ', trim(gzip_name)
!
      call open_read_mpi_file                                           &
     &   (gzip_name, num_pe, id_rank, IO_param)
!
      call gz_mpi_read_itp_domain_dest(IO_param, IO_itp_dest)
      call close_mpi_file(IO_param)
!
      end subroutine gz_mpi_read_itp_dmn_dest_file
!
!-----------------------------------------------------------------------
!
      end module gz_MPI_itp_table_file_IO

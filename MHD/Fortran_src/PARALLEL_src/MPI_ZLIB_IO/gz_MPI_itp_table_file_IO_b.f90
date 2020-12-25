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
!!      subroutine read_gz_mpi_itp_table_file_b                         &
!!     &          (gzip_name, id_rank, num_pe, itp_tbl_IO)
!!        type(interpolate_table), intent(inout) :: itp_tbl_IO
!!
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
      type(buffer_4_gzip), private :: zbuf_itp
      type(calypso_MPI_IO_params), save, private :: IO_param
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
     &   'Write gzipped binary interpolation file: ', trim(gzip_name)
!
      call open_write_gz_mpi_file_b(gzip_name, IO_param)
!
      call gz_mpi_write_itp_domain_dest_b                               &
     &   (IO_param, itp_tbl_IO%tbl_dest)
      call gz_mpi_write_itp_table_dest_b(IO_param, itp_tbl_IO%tbl_dest)
!
      call gz_mpi_write_itp_domain_org_b(IO_param, itp_tbl_IO%tbl_org)
      call gz_mpi_write_itp_table_org_b(IO_param, itp_tbl_IO%tbl_org)
      call gz_mpi_write_itp_coefs_org_b(IO_param, itp_tbl_IO%tbl_org)
!
      call close_mpi_file(IO_param)
!
      call dealloc_itp_table_org(itp_tbl_IO%tbl_org)
      call dealloc_itp_num_org(itp_tbl_IO%tbl_org)
!
      call dealloc_itp_table_dest(itp_tbl_IO%tbl_dest)
      call dealloc_itp_num_dest(itp_tbl_IO%tbl_dest)
!
      end subroutine write_gz_mpi_itp_table_file_b
!
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
     &   'Read gzipped binary interpolation file: ', trim(gzip_name)
!
      call open_read_gz_mpi_file_b                                      &
     &   (gzip_name, num_pe, id_rank, IO_param)
!
      call gz_mpi_read_itp_domain_dest_b(IO_param, itp_tbl_IO%tbl_dest)
      call gz_mpi_read_itp_table_dest_b(IO_param, itp_tbl_IO%tbl_dest)
!
      call gz_mpi_read_itp_domain_org_b(IO_param, itp_tbl_IO%tbl_org)
      call gz_mpi_read_itp_table_org_b(IO_param, itp_tbl_IO%tbl_org)
      call gz_mpi_read_itp_coefs_org_b(IO_param, itp_tbl_IO%tbl_org)
!
      call close_mpi_file(IO_param)
!
      end subroutine read_gz_mpi_itp_table_file_b
!
!-----------------------------------------------------------------------
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
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
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
      call dealloc_itp_coef_dest(IO_itp_c_dest)
      call dealloc_itp_coef_stack(IO_itp_c_dest)
      call dealloc_itp_table_dest(IO_itp_dest)
      call dealloc_itp_num_dest(IO_itp_dest)
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
      end module gz_MPI_itp_table_file_IO_b

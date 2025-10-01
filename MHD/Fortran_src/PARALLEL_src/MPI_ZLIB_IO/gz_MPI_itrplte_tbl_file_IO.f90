!>@file  gz_MPI_itrplte_tbl_file_IO.f90
!!       module gz_MPI_itrplte_tbl_file_IO
!!
!!@author H. Matsui
!!@date   Programmed in Sep. 2012
!!
!> @brief gzipped file IO for interpolation
!!
!!@verbatim
!!      subroutine gz_mpi_wt_itp_tbl_coef_file_a(gzip_name, itp_tbl_IO)
!!      subroutine gz_mpi_wt_itp_tbl_idx_file_a(gzip_name, itp_tbl_IO)
!!        type(interpolate_table), intent(in) :: itp_tbl_IO
!!      subroutine gz_mpi_wt_dbl_itbl_coef_file_a                       &
!!     &         (gzip_name, itp_tbl1_IO, itp_tbl2_IO)
!!      subroutine gz_mpi_wt_dbl_itbl_idx_file_a                        &
!!     &         (gzip_name, itp_tbl1_IO, itp_tbl2_IO)
!!        type(interpolate_table), intent(in) :: itp_tbl1_IO
!!        type(interpolate_table), intent(in) :: itp_tbl2_IO
!!
!!      subroutine gz_mpi_rd_itp_tbl_coef_file_a                        &
!!     &         (gzip_name, id_rank, num_pe, itp_tbl_IO)
!!      subroutine gz_mpi_rd_itp_tbl_idx_file_a                         &
!!     &         (gzip_name, id_rank, num_pe, itp_tbl_IO)
!!        type(interpolate_table), intent(inout) :: itp_tbl_IO
!!      subroutine gz_mpi_rd_dbl_itbl_coef_file_a                       &
!!     &         (gzip_name, id_rank, num_pe, itp_tbl1_IO, itp_tbl2_IO)
!!      subroutine gz_mpi_rd_dbl_itbl_idx_file_a                        &
!!     &         (gzip_name, id_rank, num_pe, itp_tbl1_IO, itp_tbl2_IO)
!!        type(interpolate_table), intent(inout) :: itp_tbl1_IO
!!        type(interpolate_table), intent(inout) :: itp_tbl2_IO
!!@endverbatim
!
      module gz_MPI_itrplte_tbl_file_IO
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
      type(calypso_MPI_IO_params), save, private :: IO_param1
!
      private :: gz_mpi_read_each_itp_table
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_wt_itp_tbl_coef_file_a(gzip_name, itp_tbl_IO)
!
      use gz_MPI_binary_datum_IO
      use gz_MPI_itp_table_org_data_IO
!
      character(len=kchara), intent(in) :: gzip_name
      type(interpolate_table), intent(in) :: itp_tbl_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write merged gzipped interpolation file: ', trim(gzip_name)
!
      call open_write_mpi_file(gzip_name, IO_param1)
      call gz_mpi_write_each_itp_table(IO_param1, itp_tbl_IO)
      call gz_mpi_write_itp_coefs_org(IO_param1, itp_tbl_IO%tbl_org)
      call close_mpi_file(IO_param1)
!
      end subroutine gz_mpi_wt_itp_tbl_coef_file_a
!
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_wt_itp_tbl_idx_file_a(gzip_name, itp_tbl_IO)
!
      use gz_MPI_binary_datum_IO
!
      character(len=kchara), intent(in) :: gzip_name
      type(interpolate_table), intent(in) :: itp_tbl_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write merged gzipped interpolation file: ', trim(gzip_name)
!
      call open_write_mpi_file(gzip_name, IO_param1)
      call gz_mpi_write_each_itp_table(IO_param1, itp_tbl_IO)
      call close_mpi_file(IO_param1)
!
      end subroutine gz_mpi_wt_itp_tbl_idx_file_a
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_wt_dbl_itbl_coef_file_a                         &
     &         (gzip_name, itp_tbl1_IO, itp_tbl2_IO)
!
      use gz_MPI_binary_datum_IO
      use gz_MPI_itp_table_org_data_IO
!
      character(len=kchara), intent(in) :: gzip_name
      type(interpolate_table), intent(in) :: itp_tbl1_IO
      type(interpolate_table), intent(in) :: itp_tbl2_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write merged gzipped interpolation file: ', trim(gzip_name)
!
      call open_write_mpi_file(gzip_name, IO_param1)
      call gz_mpi_write_each_itp_table(IO_param1, itp_tbl1_IO)
      call gz_mpi_write_itp_coefs_org(IO_param1, itp_tbl1_IO%tbl_org)
!
      call gz_mpi_write_each_itp_table(IO_param1, itp_tbl2_IO)
      call gz_mpi_write_itp_coefs_org(IO_param1, itp_tbl2_IO%tbl_org)
      call close_mpi_file(IO_param1)
!
      end subroutine gz_mpi_wt_dbl_itbl_coef_file_a
!
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_wt_dbl_itbl_idx_file_a                          &
     &         (gzip_name, itp_tbl1_IO, itp_tbl2_IO)
!
      use gz_MPI_binary_datum_IO
      use gz_MPI_itp_table_org_data_IO
!
      character(len=kchara), intent(in) :: gzip_name
      type(interpolate_table), intent(in) :: itp_tbl1_IO
      type(interpolate_table), intent(in) :: itp_tbl2_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write merged gzipped interpolation file: ', trim(gzip_name)
!
      call open_write_mpi_file(gzip_name, IO_param1)
      call gz_mpi_write_each_itp_table(IO_param1, itp_tbl1_IO)
      call gz_mpi_write_each_itp_table(IO_param1, itp_tbl2_IO)
      call close_mpi_file(IO_param1)
!
      end subroutine gz_mpi_wt_dbl_itbl_idx_file_a
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_rd_itp_tbl_coef_file_a                          &
     &         (gzip_name, id_rank, num_pe, itp_tbl_IO)
!
      use gz_MPI_binary_datum_IO
      use gz_MPI_itp_table_org_data_IO
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
     &   (gzip_name, num_pe, id_rank, IO_param1)
      call gz_mpi_read_each_itp_table(IO_param1, itp_tbl_IO)
      call gz_mpi_read_itp_coefs_org(IO_param1, itp_tbl_IO%tbl_org)
      call close_mpi_file(IO_param1)
!
      end subroutine gz_mpi_rd_itp_tbl_coef_file_a
!
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_rd_itp_tbl_idx_file_a                           &
     &         (gzip_name, id_rank, num_pe, itp_tbl_IO)
!
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
     &   (gzip_name, num_pe, id_rank, IO_param1)
      call gz_mpi_read_each_itp_table(IO_param1, itp_tbl_IO)
      call close_mpi_file(IO_param1)
!
      end subroutine gz_mpi_rd_itp_tbl_idx_file_a
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_rd_dbl_itbl_coef_file_a                         &
     &         (gzip_name, id_rank, num_pe, itp_tbl1_IO, itp_tbl2_IO)
!
      use gz_MPI_binary_datum_IO
      use gz_MPI_itp_table_org_data_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank, num_pe
!
      type(interpolate_table), intent(inout) :: itp_tbl1_IO
      type(interpolate_table), intent(inout) :: itp_tbl2_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read merged gzipped interpolation file: ', trim(gzip_name)
!
      call open_read_mpi_file                                           &
     &   (gzip_name, num_pe, id_rank, IO_param1)
      call gz_mpi_read_each_itp_table(IO_param1, itp_tbl1_IO)
      call gz_mpi_read_itp_coefs_org(IO_param1, itp_tbl1_IO%tbl_org)
!
      call gz_mpi_read_each_itp_table(IO_param1, itp_tbl2_IO)
      call gz_mpi_read_itp_coefs_org(IO_param1, itp_tbl2_IO%tbl_org)
      call close_mpi_file(IO_param1)
!
      end subroutine gz_mpi_rd_dbl_itbl_coef_file_a
!
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_rd_dbl_itbl_idx_file_a                          &
     &         (gzip_name, id_rank, num_pe, itp_tbl1_IO, itp_tbl2_IO)
!
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
     &   'Read merged gzipped interpolation file: ', trim(gzip_name)
!
      call open_read_mpi_file                                           &
     &   (gzip_name, num_pe, id_rank, IO_param1)
      call gz_mpi_read_each_itp_table(IO_param1, itp_tbl1_IO)
      call gz_mpi_read_each_itp_table(IO_param1, itp_tbl2_IO)
      call close_mpi_file(IO_param1)
!
      end subroutine gz_mpi_rd_dbl_itbl_idx_file_a
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_write_each_itp_table(IO_param, itp_tbl_IO)
!
      use gz_MPI_itp_table_org_data_IO
      use gz_MPI_itp_table_dst_data_IO
      use gz_MPI_binary_datum_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table), intent(in) :: itp_tbl_IO
!
!
      call gz_mpi_write_itp_domain_dest(IO_param, itp_tbl_IO%tbl_dest)
      call gz_mpi_write_itp_table_dest(IO_param, itp_tbl_IO%tbl_dest)
!
      call gz_mpi_write_itp_domain_org(IO_param, itp_tbl_IO%tbl_org)
      call gz_mpi_write_itp_table_org(IO_param, itp_tbl_IO%tbl_org)
      call gz_mpi_write_itp_index_org(IO_param, itp_tbl_IO%tbl_org)
!
      end subroutine gz_mpi_write_each_itp_table
!
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_read_each_itp_table(IO_param, itp_tbl_IO)
!
      use gz_MPI_itp_table_org_data_IO
      use gz_MPI_itp_table_dst_data_IO
      use gz_MPI_binary_datum_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table), intent(inout) :: itp_tbl_IO
!
!
      call gz_mpi_read_itp_domain_dest(IO_param, itp_tbl_IO%tbl_dest)
      call gz_mpi_read_itp_table_dest(IO_param, itp_tbl_IO%tbl_dest)
!
      call gz_mpi_read_itp_domain_org(IO_param, itp_tbl_IO%tbl_org)
      call gz_mpi_read_itp_table_org(IO_param, itp_tbl_IO%tbl_org)
      call gz_mpi_read_itp_index_org(IO_param, itp_tbl_IO%tbl_org)
!
      end subroutine gz_mpi_read_each_itp_table
!
!-----------------------------------------------------------------------
!
      end module gz_MPI_itrplte_tbl_file_IO

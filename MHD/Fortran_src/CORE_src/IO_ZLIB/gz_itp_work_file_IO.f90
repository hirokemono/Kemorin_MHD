!>@file  gz_itp_work_file_IO.f90
!!       module gz_itp_work_file_IO
!!
!!@author H. Matsui
!!@date   Programmed in Sep. 2012
!!
!> @brief gzipped file IO for interpolation
!!
!!@verbatim
!!      subroutine gz_write_itp_coefs_dest_file                         &
!!     &         (gzip_name, id_rank, IO_itp_dest, IO_itp_c_dest)
!!        type(interpolate_table_dest), intent(in) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
!!      subroutine gz_read_itp_coefs_dest_file                          &
!!     &         (gzip_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!!      subroutine gz_read_itp_table_dest_file                          &
!!     &         (gzip_name, id_rank, IO_itp_dest, ierr)
!!      subroutine gz_read_itp_domain_dest_file                         &
!!     &         (gzip_name, id_rank, IO_itp_dest, ierr)
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!!@endverbatim
!
      module gz_itp_work_file_IO
!
      use m_precision
      use m_error_IDs
!
      use t_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
!
      use gz_itp_table_data_IO
      use gz_binary_IO
!
      implicit none
!
      type(buffer_4_gzip), private :: zbuf_itp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine gz_write_itp_coefs_dest_file                           &
     &         (gzip_name, id_rank, IO_itp_dest, IO_itp_c_dest)
!
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
!
!
      call open_wt_gzfile_a(gzip_name, zbuf_itp)
!
      call write_gz_itp_table_dest(id_rank, IO_itp_dest, zbuf_itp)
      call write_gz_itp_coefs_dest                                      &
     &   (IO_itp_dest, IO_itp_c_dest, zbuf_itp)
      call close_gzfile_a(zbuf_itp)
!
      end subroutine gz_write_itp_coefs_dest_file
!
!-----------------------------------------------------------------------
!
      subroutine gz_read_itp_coefs_dest_file                            &
     &         (gzip_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
      integer(kind = kint) :: n_rank_file
!
!
      call open_rd_gzfile_a(gzip_name, zbuf_itp)
!
      call read_gz_itp_domain_dest(n_rank_file, IO_itp_dest, zbuf_itp)
      call read_gz_itp_table_dest(IO_itp_dest, zbuf_itp)
      call read_gz_itp_coefs_dest(IO_itp_dest, IO_itp_c_dest, zbuf_itp)
      call close_gzfile_a(zbuf_itp)
!
      ierr = 0
      if (n_rank_file .ne. id_rank) ierr = ierr_file
!
      end subroutine gz_read_itp_coefs_dest_file
!
!-----------------------------------------------------------------------
!
      subroutine gz_read_itp_table_dest_file                            &
     &         (gzip_name, id_rank, IO_itp_dest, ierr)
!
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
      integer(kind = kint) :: n_rank_file
!
!
      call open_rd_gzfile_a(gzip_name, zbuf_itp)
!
      call read_gz_itp_domain_dest(n_rank_file, IO_itp_dest, zbuf_itp)
      call read_gz_itp_table_dest(IO_itp_dest, zbuf_itp)
      call close_gzfile_a(zbuf_itp)
!
      ierr = 0
      if (n_rank_file .ne. id_rank) ierr = ierr_file
!
      end subroutine gz_read_itp_table_dest_file
!
!-----------------------------------------------------------------------
!
      subroutine gz_read_itp_domain_dest_file                           &
     &         (gzip_name, id_rank, IO_itp_dest, ierr)
!
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
      integer(kind = kint) :: n_rank_file
!
!
      call open_rd_gzfile_a(gzip_name, zbuf_itp)
!
      call read_gz_itp_domain_dest(n_rank_file, IO_itp_dest, zbuf_itp)
      call close_gzfile_a(zbuf_itp)
!
      ierr = 0
      if (n_rank_file .ne. id_rank) ierr = ierr_file
!
      end subroutine gz_read_itp_domain_dest_file
!
!-----------------------------------------------------------------------
!
      end module gz_itp_work_file_IO

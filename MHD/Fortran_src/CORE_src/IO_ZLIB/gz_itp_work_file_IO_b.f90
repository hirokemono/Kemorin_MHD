!>@file  gz_itp_work_file_IO_b.f90
!!       module gz_itp_work_file_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in Sep. 2012
!!
!> @brief gzipped binary file IO for interpolation
!!
!!@verbatim
!!      subroutine write_gz_itp_coefs_dest_file_b                       &
!!     &         (gzip_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!!      subroutine write_gz_itp_idx_dest_file_b                         &
!!     &         (gzip_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!!        type(interpolate_table_dest), intent(in) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
!!      subroutine read_gz_itp_coefs_dest_file_b                        &
!!     &         (gzip_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!!      subroutine read_gz_itp_idx_dest_file_b                          &
!!     &         (gzip_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!!      subroutine read_gz_itp_table_dest_file_b                        &
!!     &         (gzip_name, id_rank, IO_itp_dest, ierr)
!!      subroutine read_gz_itp_domain_dest_file_b                       &
!!     &         (gzip_name, id_rank, IO_itp_dest, ierr)
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!!@endverbatim
!
      module gz_itp_work_file_IO_b
!
      use m_precision
      use m_error_IDs
!
      use t_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
      use t_binary_IO_buffer
!
      use t_buffer_4_gzip
!
      implicit none
!
      type(buffer_4_gzip), private, save :: zbuf_itp
      character, pointer, private, save :: FPz_itw
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_itp_coefs_dest_file_b                         &
     &         (gzip_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!
      use gz_itp_table_dest_data_IO_b
      use gzip_file_access
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
      integer(kind = kint), intent(inout) :: ierr
!
!
      call open_wt_gzfile_b(FPz_itw, gzip_name, zbuf_itp)
      if(zbuf_itp%ierr_zlib .gt. 0) go to 99
      call write_gz_itp_table_dest_b(FPz_itw, id_rank,                  &
     &                               IO_itp_dest, zbuf_itp)
      if(zbuf_itp%ierr_zlib .gt. 0) go to 99
!
      call write_gz_itp_idx_dest_b                                      &
     &   (FPz_itw, IO_itp_dest, IO_itp_c_dest, zbuf_itp)
      if(zbuf_itp%ierr_zlib .gt. 0) go to 99
!
      call write_gz_itp_coefs_dest_b                                    &
     &   (FPz_itw, IO_itp_dest, IO_itp_c_dest, zbuf_itp)
!
  99  continue
      call close_gzfile_b(FPz_itw)
      ierr = zbuf_itp%ierr_zlib
!
      end subroutine write_gz_itp_coefs_dest_file_b
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_itp_idx_dest_file_b                           &
     &         (gzip_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!
      use gz_itp_table_dest_data_IO_b
      use gzip_file_access
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
      integer(kind = kint), intent(inout) :: ierr
!
!
      call open_wt_gzfile_b(FPz_itw, gzip_name, zbuf_itp)
      if(zbuf_itp%ierr_zlib .gt. 0) go to 99
      call write_gz_itp_table_dest_b(FPz_itw, id_rank,                  &
     &                               IO_itp_dest, zbuf_itp)
      if(zbuf_itp%ierr_zlib .gt. 0) go to 99
!
      call write_gz_itp_idx_dest_b                                      &
     &   (FPz_itw, IO_itp_dest, IO_itp_c_dest, zbuf_itp)
!
  99  continue
      call close_gzfile_b(FPz_itw)
      ierr = zbuf_itp%ierr_zlib
!
      end subroutine write_gz_itp_idx_dest_file_b
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_coefs_dest_file_b                          &
     &         (gzip_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!
      use gz_itp_table_dest_data_IO_b
      use gzip_file_access
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
      call open_rd_gzfile_b(FPz_itw, gzip_name, id_rank, zbuf_itp)
      if(zbuf_itp%ierr_zlib .ne. 0) goto 99
      call read_gz_itp_domain_dest_b                                    &
     &   (FPz_itw, zbuf_itp, n_rank_file, IO_itp_dest)
      if(zbuf_itp%ierr_zlib .gt. 0) goto 99
!
      call read_gz_itp_table_dest_b(FPz_itw, zbuf_itp, IO_itp_dest)
      if(zbuf_itp%ierr_zlib .ne. 0) goto 99
!
      call read_gz_itp_idx_dest_b                                       &
     &   (FPz_itw, zbuf_itp, IO_itp_dest, IO_itp_c_dest)
      if(zbuf_itp%ierr_zlib .ne. 0) goto 99
!
      call read_gz_itp_coefs_dest_b                                     &
     &   (FPz_itw, zbuf_itp, IO_itp_dest, IO_itp_c_dest)
!
  99  continue
      call close_gzfile_b(FPz_itw)
      ierr = zbuf_itp%ierr_zlib
!
      if (n_rank_file .ne. id_rank) ierr = ierr_file
!
      end subroutine read_gz_itp_coefs_dest_file_b
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_idx_dest_file_b                            &
     &         (gzip_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!
      use gz_itp_table_dest_data_IO_b
      use gzip_file_access
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
      call open_rd_gzfile_b(FPz_itw, gzip_name, id_rank, zbuf_itp)
      if(zbuf_itp%ierr_zlib .ne. 0) goto 99
      call read_gz_itp_domain_dest_b                                    &
     &   (FPz_itw, zbuf_itp, n_rank_file, IO_itp_dest)
      if(zbuf_itp%ierr_zlib .gt. 0) goto 99
!
      call read_gz_itp_table_dest_b(FPz_itw, zbuf_itp, IO_itp_dest)
      if(zbuf_itp%ierr_zlib .ne. 0) goto 99
!
      call read_gz_itp_idx_dest_b                                       &
     &   (FPz_itw, zbuf_itp, IO_itp_dest, IO_itp_c_dest)
      if(zbuf_itp%ierr_zlib .ne. 0) goto 99
!
      call read_gz_itp_coefs_dest_b                                     &
     &   (FPz_itw, zbuf_itp, IO_itp_dest, IO_itp_c_dest)
!
  99  continue
      call close_gzfile_b(FPz_itw)
      ierr = zbuf_itp%ierr_zlib
!
      if (n_rank_file .ne. id_rank) ierr = ierr_file
!
      end subroutine read_gz_itp_idx_dest_file_b
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_table_dest_file_b                          &
     &         (gzip_name, id_rank, IO_itp_dest, ierr)
!
      use gz_itp_table_dest_data_IO_b
      use gzip_file_access
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
      call open_rd_gzfile_b(FPz_itw, gzip_name, id_rank, zbuf_itp)
      if(zbuf_itp%ierr_zlib .ne. 0) goto 99
      call read_gz_itp_domain_dest_b                                    &
     &   (FPz_itw, zbuf_itp, n_rank_file, IO_itp_dest)
      if(zbuf_itp%ierr_zlib .gt. 0) goto 99
!
      call read_gz_itp_table_dest_b(FPz_itw, zbuf_itp, IO_itp_dest)
!
  99  continue
      call close_gzfile_b(FPz_itw)
      ierr = zbuf_itp%ierr_zlib
!
      if (n_rank_file .ne. id_rank) ierr = ierr_file
!
      end subroutine read_gz_itp_table_dest_file_b
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_domain_dest_file_b                         &
     &         (gzip_name, id_rank, IO_itp_dest, ierr)
!
      use gz_itp_table_dest_data_IO_b
      use gzip_file_access
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
      call open_rd_gzfile_b(FPz_itw, gzip_name, id_rank, zbuf_itp)
      if(zbuf_itp%ierr_zlib .ne. 0) goto 99
      call read_gz_itp_domain_dest_b                                    &
     &   (FPz_itw, zbuf_itp, n_rank_file, IO_itp_dest)
!
  99  continue
      call close_gzfile_b(FPz_itw)
      ierr = zbuf_itp%ierr_zlib
!
      if (n_rank_file .ne. id_rank) ierr = ierr_file
!
      end subroutine read_gz_itp_domain_dest_file_b
!
!-----------------------------------------------------------------------
!
      end module gz_itp_work_file_IO_b

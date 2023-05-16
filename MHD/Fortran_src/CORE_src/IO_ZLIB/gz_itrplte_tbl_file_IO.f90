!>@file  gz_itrplte_tbl_file_IO.f90
!!       module gz_itrplte_tbl_file_IO
!!
!!@author H. Matsui
!!@date   Programmed in Sep. 2012
!!
!> @brief gzipped file IO for interpolation
!!
!!@verbatim
!!      subroutine gz_write_itp_table_coef_file_a                       &
!!     &         (gzip_name, id_rank, itp_tbl_IO)
!!        type(interpolate_table), intent(in) :: itp_tbl_IO
!!      subroutine gz_write_itp_table_idx_file_a                        &
!!     &         (gzip_name, id_rank, itp_tbl_IO)
!!      subroutine gz_wt_dbl_itp_tbl_coef_file_a                        &
!!     &         (gzip_name, id_rank, itp_tbl1_IO, itp_tbl2_IO)
!!      subroutine gz_wt_dbl_itp_tbl_idx_file_a                         &
!!     &         (gzip_name, id_rank, itp_tbl1_IO, itp_tbl2_IO)
!!        type(interpolate_table), intent(in) :: itp_tbl1_IO
!!        type(interpolate_table), intent(in) :: itp_tbl2_IO
!!
!!      subroutine gz_read_itp_table_coef_file_a                        &
!!     &         (gzip_name, id_rank, itp_tbl_IO, ierr)
!!      subroutine gz_read_itp_table_idx_file_a                         &
!!     &         (gzip_name, id_rank, itp_tbl_IO, ierr)
!!        type(interpolate_table), intent(inout) :: itp_tbl_IO
!!      subroutine gz_read_dbl_itp_table_file                           &
!!     &         (gzip_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
!!      subroutine gz_rd_dbl_itp_tbl_idx_file_a                         &
!!     &         (gzip_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
!!        type(interpolate_table), intent(inout) :: itp_tbl1_IO
!!        type(interpolate_table), intent(inout) :: itp_tbl2_IO
!!@endverbatim
!
      module gz_itrplte_tbl_file_IO
!
      use m_precision
      use m_error_IDs
!
      use t_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
!
      use gz_binary_IO
!
      implicit none
!
      type(buffer_4_gzip), private :: zbuf_itp1
      character, pointer, private, save :: FPz_tbl
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine gz_write_itp_table_coef_file_a                         &
     &         (gzip_name, id_rank, itp_tbl_IO)
!
      use skip_gz_comment
      use gz_itrplte_table_data_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(interpolate_table), intent(in) :: itp_tbl_IO
!
!
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Write gzipped interpolation table file: ', trim(gzip_name)
      call open_wt_gzfile_a(FPz_tbl, gzip_name, zbuf_itp1)
!
      call gz_write_each_itp_coef_table_a(FPz_tbl, id_rank,             &
     &                             itp_tbl_IO, zbuf_itp1)
!
      call close_gzfile_a(FPz_tbl, zbuf_itp1)
!
      end subroutine gz_write_itp_table_coef_file_a
!
!-----------------------------------------------------------------------
!
      subroutine gz_write_itp_table_idx_file_a                          &
     &         (gzip_name, id_rank, itp_tbl_IO)
!
      use skip_gz_comment
      use gz_itrplte_table_data_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(interpolate_table), intent(in) :: itp_tbl_IO
!
!
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Write gzipped interpolation table file: ', trim(gzip_name)
      call open_wt_gzfile_a(FPz_tbl, gzip_name, zbuf_itp1)
!
      call gz_write_each_itp_idx_table_a(FPz_tbl, id_rank,              &
     &                             itp_tbl_IO, zbuf_itp1)
!
      call close_gzfile_a(FPz_tbl, zbuf_itp1)
!
      end subroutine gz_write_itp_table_idx_file_a
!
!-----------------------------------------------------------------------
!
      subroutine gz_wt_dbl_itp_tbl_coef_file_a                          &
     &         (gzip_name, id_rank, itp_tbl1_IO, itp_tbl2_IO)
!
      use skip_gz_comment
      use gz_itrplte_table_data_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(interpolate_table), intent(in) :: itp_tbl1_IO
      type(interpolate_table), intent(in) :: itp_tbl2_IO
!
!
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Write gzipped interpolation table file: ', trim(gzip_name)
      call open_wt_gzfile_a(FPz_tbl, gzip_name, zbuf_itp1)
!
      call gz_write_each_itp_coef_table_a(FPz_tbl, id_rank,             &
     &                             itp_tbl1_IO, zbuf_itp1)
      call gz_write_each_itp_coef_table_a(FPz_tbl, id_rank,             &
     &                             itp_tbl2_IO, zbuf_itp1)
!
      call close_gzfile_a(FPz_tbl, zbuf_itp1)
!
      end subroutine gz_wt_dbl_itp_tbl_coef_file_a
!
!-----------------------------------------------------------------------
!
      subroutine gz_wt_dbl_itp_tbl_idx_file_a                           &
     &         (gzip_name, id_rank, itp_tbl1_IO, itp_tbl2_IO)
!
      use skip_gz_comment
      use gz_itrplte_table_data_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(interpolate_table), intent(in) :: itp_tbl1_IO
      type(interpolate_table), intent(in) :: itp_tbl2_IO
!
!
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Write gzipped interpolation table file: ', trim(gzip_name)
      call open_wt_gzfile_a(FPz_tbl, gzip_name, zbuf_itp1)
!
      call gz_write_each_itp_idx_table_a(FPz_tbl, id_rank,              &
     &                             itp_tbl1_IO, zbuf_itp1)
      call gz_write_each_itp_idx_table_a(FPz_tbl, id_rank,              &
     &                             itp_tbl2_IO, zbuf_itp1)
!
      call close_gzfile_a(FPz_tbl, zbuf_itp1)
!
      end subroutine gz_wt_dbl_itp_tbl_idx_file_a
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine gz_read_itp_table_coef_file_a                          &
     &         (gzip_name, id_rank, itp_tbl_IO, ierr)
!
      use skip_gz_comment
      use gz_itrplte_table_data_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 0
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Read gzipped interpolation table file: ', trim(gzip_name)
      call open_rd_gzfile_a(FPz_tbl, gzip_name, zbuf_itp1)
!
      call gz_read_each_itp_coef_table_a(FPz_tbl, id_rank,              &
     &                            itp_tbl_IO, zbuf_itp1, ierr)
!
      call close_gzfile_a(FPz_tbl, zbuf_itp1)
!
      end subroutine gz_read_itp_table_coef_file_a
!
!-----------------------------------------------------------------------
!
      subroutine gz_read_itp_table_idx_file_a                           &
     &         (gzip_name, id_rank, itp_tbl_IO, ierr)
!
      use skip_gz_comment
      use gz_itrplte_table_data_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 0
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Read gzipped interpolation table file: ', trim(gzip_name)
      call open_rd_gzfile_a(FPz_tbl, gzip_name, zbuf_itp1)
!
      call gz_read_each_itp_idx_table_a(FPz_tbl, id_rank,               &
     &                            itp_tbl_IO, zbuf_itp1, ierr)
!
      call close_gzfile_a(FPz_tbl, zbuf_itp1)
!
      end subroutine gz_read_itp_table_idx_file_a
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine gz_rd_dbl_itp_tbl_coef_file_a                          &
     &         (gzip_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
!
      use skip_gz_comment
      use gz_itrplte_table_data_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table), intent(inout) :: itp_tbl1_IO
      type(interpolate_table), intent(inout) :: itp_tbl2_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 0
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Read gzipped interpolation table file: ', trim(gzip_name)
      call open_rd_gzfile_a(FPz_tbl, gzip_name, zbuf_itp1)
!
      call gz_read_each_itp_coef_table_a                                &
     &   (FPz_tbl, id_rank, itp_tbl1_IO, zbuf_itp1, ierr)
      if(ierr .gt. 0) go to 99
!
      call gz_read_each_itp_coef_table_a                                &
     &   (FPz_tbl, id_rank, itp_tbl2_IO, zbuf_itp1, ierr)
!
  99  continue
      call close_gzfile_a(FPz_tbl, zbuf_itp1)
!
      end subroutine gz_rd_dbl_itp_tbl_coef_file_a
!
!-----------------------------------------------------------------------
!
      subroutine gz_rd_dbl_itp_tbl_idx_file_a                           &
     &         (gzip_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
!
      use skip_gz_comment
      use gz_itrplte_table_data_IO
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table), intent(inout) :: itp_tbl1_IO
      type(interpolate_table), intent(inout) :: itp_tbl2_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      ierr = 0
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Read gzipped interpolation table file: ', trim(gzip_name)
      call open_rd_gzfile_a(FPz_tbl, gzip_name, zbuf_itp1)
!
      call gz_read_each_itp_idx_table_a                                 &
     &   (FPz_tbl, id_rank, itp_tbl1_IO, zbuf_itp1, ierr)
      if(ierr .gt. 0) go to 99
!
      call gz_read_each_itp_idx_table_a                                 &
     &   (FPz_tbl, id_rank, itp_tbl2_IO, zbuf_itp1, ierr)
!
  99  continue
      call close_gzfile_a(FPz_tbl, zbuf_itp1)
!
      end subroutine gz_rd_dbl_itp_tbl_idx_file_a
!
!-----------------------------------------------------------------------
!
      end module gz_itrplte_tbl_file_IO

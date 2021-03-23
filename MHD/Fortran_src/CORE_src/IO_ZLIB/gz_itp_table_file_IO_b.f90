!>@file  gz_itp_table_file_IO_b.f90
!!       module gz_itp_table_file_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in Sep. 2012
!!
!> @brief gzipped binary file IO for interpolation
!!
!!@verbatim
!!      subroutine write_gz_itp_table_file_b                            &
!!     &         (gzip_name, id_rank, itp_tbl_IO, ierr)
!!        type(interpolate_table), intent(in) :: itp_tbl_IO
!!      subroutine write_gz_dbl_itp_table_file_b                        &
!!     &         (gzip_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
!!        type(interpolate_table), intent(in) :: itp_tbl1_IO
!!        type(interpolate_table), intent(in) :: itp_tbl2_IO
!!
!!      subroutine read_gz_itp_table_file_b                             &
!!     &          (gzip_name, id_rank, itp_tbl_IO, ierr)
!!        type(interpolate_table), intent(inout) :: itp_tbl_IO
!!      subroutine read_gz_dbl_itp_table_file_b                         &
!!     &          (gzip_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
!!        type(interpolate_table), intent(inout) :: itp_tbl1_IO
!!        type(interpolate_table), intent(inout) :: itp_tbl2_IO
!!@endverbatim
!
      module gz_itp_table_file_IO_b
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
      type(buffer_4_gzip), private :: zbuf_itp1
!
      private :: write_gz_each_itp_table_b, read_gz_each_itp_table_b
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_itp_table_file_b                              &
     &         (gzip_name, id_rank, itp_tbl_IO, ierr)
!
      use gz_itp_table_data_IO_b
      use gzip_file_access
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(interpolate_table), intent(in) :: itp_tbl_IO
!
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Write gzipped binary interpolation table file: ',              &
     &   trim(gzip_name)
      call open_wt_gzfile_b(gzip_name, zbuf_itp1)
      if(zbuf_itp1%ierr_zlib .gt. 0) go to 99
!
      call write_gz_each_itp_table_b(id_rank, itp_tbl_IO, zbuf_itp1)
!
  99  continue
      call close_gzfile_b
      ierr = zbuf_itp1%ierr_zlib
!
      end subroutine write_gz_itp_table_file_b
!
!-----------------------------------------------------------------------
!
      subroutine write_gz_dbl_itp_table_file_b                          &
     &         (gzip_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
!
      use gz_itp_table_data_IO_b
      use gzip_file_access
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(interpolate_table), intent(in) :: itp_tbl1_IO
      type(interpolate_table), intent(in) :: itp_tbl2_IO
!
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Write gzipped binary interpolation table file: ',              &
     &   trim(gzip_name)
      call open_wt_gzfile_b(gzip_name, zbuf_itp1)
      if(zbuf_itp1%ierr_zlib .gt. 0) go to 99
!
      call write_gz_each_itp_table_b(id_rank, itp_tbl1_IO, zbuf_itp1)
      if(zbuf_itp1%ierr_zlib .gt. 0) go to 99
!
      call write_gz_each_itp_table_b(id_rank, itp_tbl2_IO, zbuf_itp1)
!
  99  continue
      call close_gzfile_b
      ierr = zbuf_itp1%ierr_zlib
!
      end subroutine write_gz_dbl_itp_table_file_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_gz_itp_table_file_b                               &
     &          (gzip_name, id_rank, itp_tbl_IO, ierr)
!
      use gz_itp_table_data_IO_b
      use gzip_file_access
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Read gzipped binary interpolation table file: ',               &
     &   trim(gzip_name)
      call open_rd_gzfile_b(gzip_name, id_rank, zbuf_itp1)
      if(zbuf_itp1%ierr_zlib .ne. 0) go to 99
!
      call read_gz_each_itp_table_b(id_rank, itp_tbl_IO, zbuf_itp1)
!
  99  continue
      call close_gzfile_b
      ierr = zbuf_itp1%ierr_zlib
!
      end subroutine read_gz_itp_table_file_b
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_dbl_itp_table_file_b                           &
     &          (gzip_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
!
      use gz_itp_table_data_IO_b
      use gzip_file_access
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table), intent(inout) :: itp_tbl1_IO
      type(interpolate_table), intent(inout) :: itp_tbl2_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Read gzipped binary interpolation table file: ',               &
     &   trim(gzip_name)
      call open_rd_gzfile_b(gzip_name, id_rank, zbuf_itp1)
      if(zbuf_itp1%ierr_zlib .ne. 0) go to 99
!
      call read_gz_each_itp_table_b(id_rank, itp_tbl1_IO, zbuf_itp1)
      if(zbuf_itp1%ierr_zlib .ne. 0) go to 99
!
      call read_gz_each_itp_table_b(id_rank, itp_tbl2_IO, zbuf_itp1)
!
  99  continue
      call close_gzfile_b
      ierr = zbuf_itp1%ierr_zlib
!
      end subroutine read_gz_dbl_itp_table_file_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_gz_each_itp_table_b                              &
     &         (id_rank, itp_tbl_IO, zbuf_itp)
!
      use gz_itp_table_data_IO_b
      use gzip_file_access
!
      integer, intent(in) :: id_rank
      type(interpolate_table), intent(in) :: itp_tbl_IO
      type(buffer_4_gzip), intent(inout) :: zbuf_itp
!
!
      call write_gz_itp_table_dest_b                                    &
     &   (id_rank, itp_tbl_IO%tbl_dest, zbuf_itp)
      if(zbuf_itp%ierr_zlib .gt. 0) return
!
      call write_gz_itp_table_org_b                                     &
     &    (id_rank, itp_tbl_IO%tbl_org, zbuf_itp)
      if(zbuf_itp%ierr_zlib .gt. 0) return
      call write_gz_itp_coefs_org_b(itp_tbl_IO%tbl_org, zbuf_itp)
!
      end subroutine write_gz_each_itp_table_b
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_each_itp_table_b                               &
     &         (id_rank, itp_tbl_IO, zbuf_itp)
!
      use gz_itp_table_data_IO_b
      use gzip_file_access
!
      integer, intent(in) :: id_rank
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
      type(buffer_4_gzip), intent(inout) :: zbuf_itp
!
      integer(kind = kint) :: n_rank_file
!
!
      call read_gz_itp_domain_dest_b                                    &
     &   (zbuf_itp, n_rank_file, itp_tbl_IO%tbl_dest)
      if(zbuf_itp%ierr_zlib .gt. 0) return
!
      call read_gz_itp_table_dest_b(zbuf_itp, itp_tbl_IO%tbl_dest)
      if(zbuf_itp%ierr_zlib .ne. 0) return
!
      call read_gz_itp_domain_org_b                                     &
     &    (zbuf_itp, n_rank_file, itp_tbl_IO%tbl_org)
      if(zbuf_itp%ierr_zlib .gt. 0) return
!
      call read_gz_itp_table_org_b(zbuf_itp, itp_tbl_IO%tbl_org)
      if(zbuf_itp%ierr_zlib .ne. 0) return
!
      call read_gz_itp_coefs_org_b(zbuf_itp, itp_tbl_IO%tbl_org)
      if (n_rank_file .ne. id_rank) zbuf_itp%ierr_zlib = n_rank_file
!
      end subroutine read_gz_each_itp_table_b
!
!-----------------------------------------------------------------------
!
      end module gz_itp_table_file_IO_b

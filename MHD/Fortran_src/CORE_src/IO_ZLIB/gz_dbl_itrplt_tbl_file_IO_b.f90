!>@file   gz_dbl_itrplt_tbl_file_IO_b.f90
!!       module gz_dbl_itrplt_tbl_file_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in Sep. 2012
!!
!> @brief gzipped binary file IO for double interpolation table file
!!
!!@verbatim
!!      subroutine gz_wt_dbl_itp_tbl_coef_file_b(gzip_name, id_rank,    &
!!     &          itp_tbl1_org_IO, itp_tbl1_dest_IO,                    &
!!     &          itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
!!      subroutine gz_wt_dbl_itp_tbl_idx_file_b(gzip_name, id_rank,     &
!!     &          itp_tbl1_org_IO, itp_tbl1_dest_IO,                    &
!!     &          itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
!!        character(len=kchara), intent(in) :: gzip_name
!!        integer, intent(in) :: id_rank
!!        type(interpolate_table_org), intent(in) :: itp_tbl1_org_IO
!!        type(interpolate_table_dest), intent(in) :: itp_tbl1_dest_IO
!!        type(interpolate_table_org), intent(in) :: itp_tbl2_org_IO
!!        type(interpolate_table_dest), intent(in) :: itp_tbl2_dest_IO
!!        integer(kind = kint), intent(inout) :: ierr
!!
!!      subroutine gz_rd_dbl_itp_tbl_coef_file_b(gzip_name, id_rank,    &
!!     &          itp_tbl1_org_IO, itp_tbl1_dest_IO,                    &
!!     &          itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
!!      subroutine gz_rd_dbl_itp_tbl_idx_file_b(gzip_name, id_rank,     &
!!     &          itp_tbl1_org_IO, itp_tbl1_dest_IO,                    &
!!     &          itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
!!        character(len=kchara), intent(in) :: gzip_name
!!        integer, intent(in) :: id_rank
!!        type(interpolate_table_org), intent(inout) :: itp_tbl1_org_IO
!!        type(interpolate_table_dest), intent(inout) :: itp_tbl1_dest_IO
!!        type(interpolate_table_org), intent(inout) :: itp_tbl2_org_IO
!!        type(interpolate_table_dest), intent(inout) :: itp_tbl2_dest_IO
!!        integer(kind = kint), intent(inout) :: ierr
!!@endverbatim
!
      module gz_dbl_itrplt_tbl_file_IO_b
!
      use m_precision
      use m_error_IDs
!
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
      use t_binary_IO_buffer
!
      use t_buffer_4_gzip
!
      implicit none
!
      type(buffer_4_gzip), private, save :: zbuf_itp1
      character, pointer, private, save :: FPz_itp
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine gz_wt_dbl_itp_tbl_coef_file_b(gzip_name, id_rank,      &
     &          itp_tbl1_org_IO, itp_tbl1_dest_IO,                      &
     &          itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
!
      use gzip_file_access
      use gz_binary_IO
      use gz_itrplte_table_data_IO_b
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(interpolate_table_org), intent(in) :: itp_tbl1_org_IO
      type(interpolate_table_dest), intent(in) :: itp_tbl1_dest_IO
      type(interpolate_table_org), intent(in) :: itp_tbl2_org_IO
      type(interpolate_table_dest), intent(in) :: itp_tbl2_dest_IO
!
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Write gzipped binary interpolation table file: ',              &
     &   trim(gzip_name)
      call open_wt_gzfile_b(FPz_itp, gzip_name, zbuf_itp1)
      if(zbuf_itp1%ierr_zlib .gt. 0) go to 99
!
      call write_gz_each_itp_coef_table_b(FPz_itp, id_rank,             &
     &    itp_tbl1_org_IO, itp_tbl1_dest_IO, zbuf_itp1)
      if(zbuf_itp1%ierr_zlib .gt. 0) go to 99
!
      call write_gz_each_itp_coef_table_b(FPz_itp, id_rank,             &
     &    itp_tbl2_org_IO, itp_tbl2_dest_IO, zbuf_itp1)
!
  99  continue
      call close_gzfile_b(FPz_itp)
      ierr = zbuf_itp1%ierr_zlib
!
      end subroutine gz_wt_dbl_itp_tbl_coef_file_b
!
!-----------------------------------------------------------------------
!
      subroutine gz_wt_dbl_itp_tbl_idx_file_b(gzip_name, id_rank,       &
     &          itp_tbl1_org_IO, itp_tbl1_dest_IO,                      &
     &          itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
!
      use gzip_file_access
      use gz_binary_IO
      use gz_itrplte_table_data_IO_b
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
      type(interpolate_table_org), intent(in) :: itp_tbl1_org_IO
      type(interpolate_table_dest), intent(in) :: itp_tbl1_dest_IO
      type(interpolate_table_org), intent(in) :: itp_tbl2_org_IO
      type(interpolate_table_dest), intent(in) :: itp_tbl2_dest_IO
!
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Write gzipped binary interpolation table file: ',              &
     &   trim(gzip_name)
      call open_wt_gzfile_b(FPz_itp, gzip_name, zbuf_itp1)
      if(zbuf_itp1%ierr_zlib .gt. 0) go to 99
!
      call write_gz_each_itp_idx_table_b(FPz_itp, id_rank,              &
     &    itp_tbl1_org_IO, itp_tbl1_dest_IO, zbuf_itp1)
      if(zbuf_itp1%ierr_zlib .gt. 0) go to 99
!
      call write_gz_each_itp_idx_table_b(FPz_itp, id_rank,              &
     &    itp_tbl2_org_IO, itp_tbl2_dest_IO, zbuf_itp1)
!
  99  continue
      call close_gzfile_b(FPz_itp)
      ierr = zbuf_itp1%ierr_zlib
!
      end subroutine gz_wt_dbl_itp_tbl_idx_file_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine gz_rd_dbl_itp_tbl_coef_file_b(gzip_name, id_rank,      &
     &          itp_tbl1_org_IO, itp_tbl1_dest_IO,                      &
     &          itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
!
      use gzip_file_access
      use gz_binary_IO
      use gz_itrplte_table_data_IO_b
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table_org), intent(inout) :: itp_tbl1_org_IO
      type(interpolate_table_dest), intent(inout) :: itp_tbl1_dest_IO
      type(interpolate_table_org), intent(inout) :: itp_tbl2_org_IO
      type(interpolate_table_dest), intent(inout) :: itp_tbl2_dest_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Read gzipped binary interpolation table file: ',               &
     &   trim(gzip_name)
      call open_rd_gzfile_b(FPz_itp, gzip_name, id_rank, zbuf_itp1)
      if(zbuf_itp1%ierr_zlib .ne. 0) go to 99
!
      call read_gz_each_itp_coef_table_b(FPz_itp, id_rank,              &
     &    itp_tbl1_org_IO, itp_tbl1_dest_IO, zbuf_itp1)
      if(zbuf_itp1%ierr_zlib .ne. 0) go to 99
!
      call read_gz_each_itp_coef_table_b(FPz_itp, id_rank,              &
     &    itp_tbl2_org_IO, itp_tbl2_dest_IO, zbuf_itp1)
!
  99  continue
      call close_gzfile_b(FPz_itp)
      ierr = zbuf_itp1%ierr_zlib
!
      end subroutine gz_rd_dbl_itp_tbl_coef_file_b
!
!-----------------------------------------------------------------------
!
      subroutine gz_rd_dbl_itp_tbl_idx_file_b(gzip_name, id_rank,       &
     &          itp_tbl1_org_IO, itp_tbl1_dest_IO,                      &
     &          itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
!
      use gzip_file_access
      use gz_binary_IO
      use gz_itrplte_table_data_IO_b
!
      character(len=kchara), intent(in) :: gzip_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table_org), intent(inout) :: itp_tbl1_org_IO
      type(interpolate_table_dest), intent(inout) :: itp_tbl1_dest_IO
      type(interpolate_table_org), intent(inout) :: itp_tbl2_org_IO
      type(interpolate_table_dest), intent(inout) :: itp_tbl2_dest_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Read gzipped binary interpolation table file: ',               &
     &   trim(gzip_name)
      call open_rd_gzfile_b(FPz_itp, gzip_name, id_rank, zbuf_itp1)
      if(zbuf_itp1%ierr_zlib .ne. 0) go to 99
!
      call read_gz_each_itp_idx_table_b(FPz_itp, id_rank,               &
     &    itp_tbl1_org_IO, itp_tbl1_dest_IO, zbuf_itp1)
      if(zbuf_itp1%ierr_zlib .ne. 0) go to 99
!
      call read_gz_each_itp_idx_table_b(FPz_itp, id_rank,               &
     &    itp_tbl2_org_IO, itp_tbl2_dest_IO, zbuf_itp1)
!
  99  continue
      call close_gzfile_b(FPz_itp)
      ierr = zbuf_itp1%ierr_zlib
!
      end subroutine gz_rd_dbl_itp_tbl_idx_file_b
!
!-----------------------------------------------------------------------
!
      end module gz_dbl_itrplt_tbl_file_IO_b

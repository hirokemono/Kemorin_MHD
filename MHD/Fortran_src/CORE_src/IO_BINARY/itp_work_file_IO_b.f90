!>@file   itp_work_file_IO_b.f90
!!@brief  module itp_work_file_IO_b
!!
!!@author H. Matsui
!!@date Programmed in Sep. 2006
!!
!>@brief  Binary interpolation file IO
!!
!!@verbatim
!!      subroutine write_itp_coefs_dest_file_b                          &
!!     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!!      subroutine write_itp_idx_dest_file_b                            &
!!     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!!        type(interpolate_table_dest), intent(in) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
!!      subroutine read_itp_coefs_dest_file_b                           &
!!     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!!      subroutine read_itp_idx_dest_file_b                             &
!!     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!!      subroutine read_itp_table_dest_file_b                           &
!!     &         (file_name, id_rank, IO_itp_dest, ierr)
!!      subroutine read_itp_domain_dest_file_b                          &
!!     &         (file_name, id_rank, IO_itp_dest, ierr)
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!!@endverbatim
!
      module itp_work_file_IO_b
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
      use binary_IO
!
      implicit none
!
      integer(kind = kint), parameter :: id_read_tbl =  21
      integer(kind = kint), parameter :: id_write_tbl = 22
      type(binary_IO_buffer) :: bbuf_tbl
      private :: id_read_tbl, id_write_tbl, bbuf_tbl
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_itp_coefs_dest_file_b                            &
     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!
      use itp_table_dest_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
      integer(kind = kint), intent(inout) :: ierr
!
!
      bbuf_tbl%id_binary = id_write_tbl
      call open_write_binary_file(file_name, bbuf_tbl)
      if(bbuf_tbl%ierr_bin .gt. 0) go to 99
      call write_interpolate_table_dest_b                               &
     &   (id_rank, IO_itp_dest, bbuf_tbl)
      if(bbuf_tbl%ierr_bin .gt. 0) go to 99
!
      call write_interpolate_idx_dest_b                                 &
     &   (IO_itp_dest, IO_itp_c_dest, bbuf_tbl)
      if(bbuf_tbl%ierr_bin .gt. 0) go to 99
      call write_interpolate_coefs_dest_b                               &
     &   (IO_itp_dest, IO_itp_c_dest, bbuf_tbl)
!
  99  continue
      call close_binary_file(bbuf_tbl)
      ierr = bbuf_tbl%ierr_bin
!
      end subroutine write_itp_coefs_dest_file_b
!
!-----------------------------------------------------------------------
!
      subroutine write_itp_idx_dest_file_b                              &
     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!
      use itp_table_dest_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
      integer(kind = kint), intent(inout) :: ierr
!
!
      bbuf_tbl%id_binary = id_write_tbl
      call open_write_binary_file(file_name, bbuf_tbl)
      if(bbuf_tbl%ierr_bin .gt. 0) go to 99
      call write_interpolate_table_dest_b                               &
     &   (id_rank, IO_itp_dest, bbuf_tbl)
      if(bbuf_tbl%ierr_bin .gt. 0) go to 99
!
      call write_interpolate_idx_dest_b                                 &
     &   (IO_itp_dest, IO_itp_c_dest, bbuf_tbl)
!
  99  continue
      call close_binary_file(bbuf_tbl)
      ierr = bbuf_tbl%ierr_bin
!
      end subroutine write_itp_idx_dest_file_b
!
!-----------------------------------------------------------------------
!
      subroutine read_itp_coefs_dest_file_b                             &
     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!
      use itp_table_dest_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
      integer(kind = kint) :: n_rank_file
!
!
      bbuf_tbl%id_binary = id_read_tbl
      call open_read_binary_file(file_name, id_rank, bbuf_tbl)
      if(bbuf_tbl%ierr_bin .ne. 0) goto 99
      call read_interpolate_domain_dest_b                               &
     &   (bbuf_tbl, n_rank_file, IO_itp_dest)
      if(bbuf_tbl%ierr_bin .gt. 0) goto 99
!
      call read_interpolate_table_dest_b(bbuf_tbl, IO_itp_dest)
      if(bbuf_tbl%ierr_bin .ne. 0) goto 99
!
      call read_interpolate_idx_dest_b                                  &
     &   (bbuf_tbl, IO_itp_dest, IO_itp_c_dest)
      if(bbuf_tbl%ierr_bin .ne. 0) goto 99
!
      call read_interpolate_coefs_dest_b                                &
     &   (bbuf_tbl, IO_itp_dest, IO_itp_c_dest)
!
  99  continue
      call close_binary_file(bbuf_tbl)
      ierr = bbuf_tbl%ierr_bin
!
      if (n_rank_file .ne. id_rank) ierr = ierr_file
!
      end subroutine read_itp_coefs_dest_file_b
!
!-----------------------------------------------------------------------
!
      subroutine read_itp_idx_dest_file_b                               &
     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!
      use itp_table_dest_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
      integer(kind = kint) :: n_rank_file
!
!
      bbuf_tbl%id_binary = id_read_tbl
      call open_read_binary_file(file_name, id_rank, bbuf_tbl)
      if(bbuf_tbl%ierr_bin .ne. 0) goto 99
      call read_interpolate_domain_dest_b                               &
     &   (bbuf_tbl, n_rank_file, IO_itp_dest)
      if(bbuf_tbl%ierr_bin .gt. 0) goto 99
!
      call read_interpolate_table_dest_b(bbuf_tbl, IO_itp_dest)
      if(bbuf_tbl%ierr_bin .ne. 0) goto 99
!
      call read_interpolate_idx_dest_b                                  &
     &   (bbuf_tbl, IO_itp_dest, IO_itp_c_dest)
!
  99  continue
      call close_binary_file(bbuf_tbl)
      ierr = bbuf_tbl%ierr_bin
!
      if (n_rank_file .ne. id_rank) ierr = ierr_file
!
      end subroutine read_itp_idx_dest_file_b
!
!-----------------------------------------------------------------------
!
      subroutine read_itp_table_dest_file_b                             &
     &         (file_name, id_rank, IO_itp_dest, ierr)
!
      use itp_table_dest_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
      integer(kind = kint) :: n_rank_file
!
!
      bbuf_tbl%id_binary = id_read_tbl
      call open_read_binary_file(file_name, id_rank, bbuf_tbl)
      if(bbuf_tbl%ierr_bin .ne. 0) goto 99
      call read_interpolate_domain_dest_b                               &
     &   (bbuf_tbl, n_rank_file, IO_itp_dest)
      if(bbuf_tbl%ierr_bin .gt. 0) goto 99
!
      call read_interpolate_table_dest_b(bbuf_tbl, IO_itp_dest)
!
  99  continue
      call close_binary_file(bbuf_tbl)
      ierr = bbuf_tbl%ierr_bin
!
      if (n_rank_file .ne. id_rank) ierr = ierr_file
!
      end subroutine read_itp_table_dest_file_b
!
!-----------------------------------------------------------------------
!
      subroutine read_itp_domain_dest_file_b                            &
     &         (file_name, id_rank, IO_itp_dest, ierr)
!
      use itp_table_dest_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
      integer(kind = kint) :: n_rank_file
!
!
      bbuf_tbl%id_binary = id_read_tbl
      call open_read_binary_file(file_name, id_rank, bbuf_tbl)
      if(bbuf_tbl%ierr_bin .ne. 0) goto 99
      call read_interpolate_domain_dest_b                               &
     &   (bbuf_tbl, n_rank_file, IO_itp_dest)
!
  99  continue
      call close_binary_file(bbuf_tbl)
      ierr = bbuf_tbl%ierr_bin
!
      if (n_rank_file .ne. id_rank) ierr = ierr_file
!
      end subroutine read_itp_domain_dest_file_b
!
!-----------------------------------------------------------------------
!
      end module itp_work_file_IO_b

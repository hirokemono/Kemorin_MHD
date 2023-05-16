!>@file   itrplte_tbl_file_IO_b.f90
!!@brief  module itrplte_tbl_file_IO_b
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!!
!>@brief  Binary interpolation file IO
!!
!!@verbatim
!!      subroutine write_itp_table_coef_file_b                          &
!!     &         (file_name, id_rank, itp_tbl_IO, ierr)
!!      subroutine write_itp_table_idx_file_b                           &
!!     &         (file_name, id_rank, itp_tbl_IO, ierr)
!!        type(interpolate_table), intent(in) :: itp_tbl_IO
!!      subroutine write_dbl_itp_tbl_coef_file_b                        &
!!     &         (file_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
!!      subroutine write_dbl_itp_tbl_idx_file_b                         &
!!     &         (file_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
!!        type(interpolate_table), intent(in) :: itp_tbl1_IO
!!        type(interpolate_table), intent(in) :: itp_tbl2_IO
!!      subroutine read_itp_table_coef_file_b                           &
!!     &          (file_name, id_rank, itp_tbl_IO, ierr)
!!      subroutine read_itp_table_idx_file_b                            &
!!     &          (file_name, id_rank, itp_tbl_IO, ierr)
!!        type(interpolate_table), intent(inout) :: itp_tbl_IO
!!      subroutine read_dbl_itp_tbl_coef_file_b                         &
!!     &          (file_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
!!      subroutine read_dbl_itp_tbl_idx_file_b                          &
!!     &          (file_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
!!        type(interpolate_table), intent(inout) :: itp_tbl1_IO
!!       type(interpolate_table), intent(inout) :: itp_tbl2_IO
!!@endverbatim
!
      module itrplte_tbl_file_IO_b
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
      type(binary_IO_buffer) :: bbuf_tbl1
      private :: id_read_tbl, id_write_tbl, bbuf_tbl1
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_itp_table_coef_file_b                            &
     &         (file_name, id_rank, itp_tbl_IO, ierr)
!
      use itrplte_table_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(interpolate_table), intent(in) :: itp_tbl_IO
!
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Write Binary interpolation table file: ', trim(file_name)
      bbuf_tbl1%id_binary = id_write_tbl
      call open_write_binary_file(file_name, bbuf_tbl1)
      if(bbuf_tbl1%ierr_bin .gt. 0) go to 99
      call write_each_itp_coef_table_b(id_rank, itp_tbl_IO, bbuf_tbl1)
!
  99  continue
      call close_binary_file(bbuf_tbl1)
      ierr = bbuf_tbl1%ierr_bin
!
      end subroutine write_itp_table_coef_file_b
!
!-----------------------------------------------------------------------
!
      subroutine write_itp_table_idx_file_b                             &
     &         (file_name, id_rank, itp_tbl_IO, ierr)
!
      use itrplte_table_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(interpolate_table), intent(in) :: itp_tbl_IO
!
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Write Binary interpolation table file: ', trim(file_name)
      bbuf_tbl1%id_binary = id_write_tbl
      call open_write_binary_file(file_name, bbuf_tbl1)
      if(bbuf_tbl1%ierr_bin .gt. 0) go to 99
      call write_each_itp_idx_table_b(id_rank, itp_tbl_IO, bbuf_tbl1)
!
  99  continue
      call close_binary_file(bbuf_tbl1)
      ierr = bbuf_tbl1%ierr_bin
!
      end subroutine write_itp_table_idx_file_b
!
!-----------------------------------------------------------------------
!
      subroutine write_dbl_itp_tbl_coef_file_b                          &
     &         (file_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
!
      use itrplte_table_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(interpolate_table), intent(in) :: itp_tbl1_IO
      type(interpolate_table), intent(in) :: itp_tbl2_IO
!
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Write Binary interpolation table file: ', trim(file_name)
      bbuf_tbl1%id_binary = id_write_tbl
      call open_write_binary_file(file_name, bbuf_tbl1)
      if(bbuf_tbl1%ierr_bin .gt. 0) go to 99
!
      call write_each_itp_coef_table_b(id_rank, itp_tbl1_IO, bbuf_tbl1)
      if(bbuf_tbl1%ierr_bin .gt. 0) go to 99
      call write_each_itp_coef_table_b(id_rank, itp_tbl2_IO, bbuf_tbl1)
!
  99  continue
      call close_binary_file(bbuf_tbl1)
      ierr = bbuf_tbl1%ierr_bin
!
      end subroutine write_dbl_itp_tbl_coef_file_b
!
!-----------------------------------------------------------------------
!
      subroutine write_dbl_itp_tbl_idx_file_b                           &
     &         (file_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
!
      use itrplte_table_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
      type(interpolate_table), intent(in) :: itp_tbl1_IO
      type(interpolate_table), intent(in) :: itp_tbl2_IO
!
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Write Binary interpolation table file: ', trim(file_name)
      bbuf_tbl1%id_binary = id_write_tbl
      call open_write_binary_file(file_name, bbuf_tbl1)
      if(bbuf_tbl1%ierr_bin .gt. 0) go to 99
!
      call write_each_itp_idx_table_b(id_rank, itp_tbl1_IO, bbuf_tbl1)
      if(bbuf_tbl1%ierr_bin .gt. 0) go to 99
      call write_each_itp_idx_table_b(id_rank, itp_tbl2_IO, bbuf_tbl1)
!
  99  continue
      call close_binary_file(bbuf_tbl1)
      ierr = bbuf_tbl1%ierr_bin
!
      end subroutine write_dbl_itp_tbl_idx_file_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_itp_table_coef_file_b                             &
     &          (file_name, id_rank, itp_tbl_IO, ierr)
!
      use itrplte_table_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Read Binary interpolation table file: ', trim(file_name)
      bbuf_tbl1%id_binary = id_read_tbl
      call open_read_binary_file(file_name, id_rank, bbuf_tbl1)
      if(bbuf_tbl1%ierr_bin .ne. 0) goto 99
!
      call read_each_itp_coef_table_b(id_rank, itp_tbl_IO, bbuf_tbl1)
!
  99  continue
      call close_binary_file(bbuf_tbl1)
      ierr = bbuf_tbl1%ierr_bin
!
      end subroutine read_itp_table_coef_file_b
!
!-----------------------------------------------------------------------
!
      subroutine read_itp_table_idx_file_b                              &
     &          (file_name, id_rank, itp_tbl_IO, ierr)
!
      use itrplte_table_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Read Binary interpolation table file: ', trim(file_name)
      bbuf_tbl1%id_binary = id_read_tbl
      call open_read_binary_file(file_name, id_rank, bbuf_tbl1)
      if(bbuf_tbl1%ierr_bin .ne. 0) goto 99
!
      call read_each_itp_idx_table_b(id_rank, itp_tbl_IO, bbuf_tbl1)
!
  99  continue
      call close_binary_file(bbuf_tbl1)
      ierr = bbuf_tbl1%ierr_bin
!
      end subroutine read_itp_table_idx_file_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_dbl_itp_tbl_coef_file_b                           &
     &          (file_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
!
      use itrplte_table_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table), intent(inout) :: itp_tbl1_IO
      type(interpolate_table), intent(inout) :: itp_tbl2_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Read Binary interpolation table file: ', trim(file_name)
      bbuf_tbl1%id_binary = id_read_tbl
      call open_read_binary_file(file_name, id_rank, bbuf_tbl1)
      if(bbuf_tbl1%ierr_bin .ne. 0) goto 99
!
      call read_each_itp_coef_table_b(id_rank, itp_tbl1_IO, bbuf_tbl1)
      if(bbuf_tbl1%ierr_bin .gt. 0) goto 99
!
      call read_each_itp_coef_table_b(id_rank, itp_tbl2_IO, bbuf_tbl1)
!
  99  continue
      call close_binary_file(bbuf_tbl1)
      ierr = bbuf_tbl1%ierr_bin
!
      end subroutine read_dbl_itp_tbl_coef_file_b
!
!-----------------------------------------------------------------------
!
      subroutine read_dbl_itp_tbl_idx_file_b                            &
     &          (file_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
!
      use itrplte_table_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table), intent(inout) :: itp_tbl1_IO
      type(interpolate_table), intent(inout) :: itp_tbl2_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(id_rank .eq. 0) write(*,*)                                     &
     &  'Read Binary interpolation table file: ', trim(file_name)
      bbuf_tbl1%id_binary = id_read_tbl
      call open_read_binary_file(file_name, id_rank, bbuf_tbl1)
      if(bbuf_tbl1%ierr_bin .ne. 0) goto 99
!
      call read_each_itp_idx_table_b(id_rank, itp_tbl1_IO, bbuf_tbl1)
      if(bbuf_tbl1%ierr_bin .gt. 0) goto 99
!
      call read_each_itp_idx_table_b(id_rank, itp_tbl2_IO, bbuf_tbl1)
!
  99  continue
      call close_binary_file(bbuf_tbl1)
      ierr = bbuf_tbl1%ierr_bin
!
      end subroutine read_dbl_itp_tbl_idx_file_b
!
!-----------------------------------------------------------------------
!
      end module itrplte_tbl_file_IO_b

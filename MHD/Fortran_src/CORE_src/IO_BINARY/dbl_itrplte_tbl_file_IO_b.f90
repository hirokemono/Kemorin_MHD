!>@file   dbl_itrplte_tbl_file_IO_b.f90
!!@brief  module dbl_itrplte_tbl_file_IO_b
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!!
!>@brief  Binary double interpolation table file IO
!!
!!@verbatim
!!      subroutine write_dbl_itp_tbl_coef_file_b(file_name, id_rank,    &
!!     &          itp_tbl1_org_IO, itp_tbl1_dest_IO,                    &
!!     &          itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
!!      subroutine write_dbl_itp_tbl_idx_file_b(file_name, id_rank,     &
!!     &          itp_tbl1_org_IO, itp_tbl1_dest_IO,                    &
!!     &          itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
!!        character(len=kchara), intent(in) :: file_name
!!        integer, intent(in) :: id_rank
!!        type(interpolate_table_org), intent(in) :: itp_tbl1_org_IO
!!        type(interpolate_table_dest), intent(in) :: itp_tbl1_dest_IO
!!        type(interpolate_table_org), intent(in) :: itp_tbl2_org_IO
!!        type(interpolate_table_dest), intent(in) :: itp_tbl2_dest_IO
!!        integer(kind = kint), intent(inout) :: ierr
!!
!!      subroutine read_dbl_itp_tbl_coef_file_b(file_name, id_rank,     &
!!     &          itp_tbl1_org_IO, itp_tbl1_dest_IO,                    &
!!     &          itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
!!      subroutine read_dbl_itp_tbl_idx_file_b(file_name, id_rank,      &
!!     &          itp_tbl1_org_IO, itp_tbl1_dest_IO,                    &
!!     &          itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
!!        character(len=kchara), intent(in) :: file_name
!!        integer, intent(in) :: id_rank
!!        type(interpolate_table_org), intent(inout) :: itp_tbl1_org_IO
!!        type(interpolate_table_dest), intent(inout) :: itp_tbl1_dest_IO
!!        type(interpolate_table_org), intent(inout) :: itp_tbl2_org_IO
!!        type(interpolate_table_dest), intent(inout) :: itp_tbl2_dest_IO
!!        integer(kind = kint), intent(inout) :: ierr
!!@endverbatim
!
      module dbl_itrplte_tbl_file_IO_b
!
      use m_precision
      use m_error_IDs
!
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
      subroutine write_dbl_itp_tbl_coef_file_b(file_name, id_rank,      &
     &          itp_tbl1_org_IO, itp_tbl1_dest_IO,                      &
     &          itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
!
      use itrplte_table_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
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
     &  'Write Binary interpolation table file: ', trim(file_name)
      bbuf_tbl1%id_binary = id_write_tbl
      call open_write_binary_file(file_name, bbuf_tbl1)
      if(bbuf_tbl1%ierr_bin .gt. 0) go to 99
!
      call write_each_itp_coef_table_b(id_rank,                         &
     &    itp_tbl1_org_IO, itp_tbl1_dest_IO, bbuf_tbl1)
      if(bbuf_tbl1%ierr_bin .gt. 0) go to 99
      call write_each_itp_coef_table_b(id_rank,                         &
     &    itp_tbl2_org_IO, itp_tbl2_dest_IO, bbuf_tbl1)
!
  99  continue
      call close_binary_file(bbuf_tbl1)
      ierr = bbuf_tbl1%ierr_bin
!
      end subroutine write_dbl_itp_tbl_coef_file_b
!
!-----------------------------------------------------------------------
!
      subroutine write_dbl_itp_tbl_idx_file_b(file_name, id_rank,       &
     &          itp_tbl1_org_IO, itp_tbl1_dest_IO,                      &
     &          itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
!
      use itrplte_table_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
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
     &  'Write Binary interpolation table file: ', trim(file_name)
      bbuf_tbl1%id_binary = id_write_tbl
      call open_write_binary_file(file_name, bbuf_tbl1)
      if(bbuf_tbl1%ierr_bin .gt. 0) go to 99
!
      call write_each_itp_idx_table_b(id_rank,                          &
     &    itp_tbl1_org_IO, itp_tbl1_dest_IO, bbuf_tbl1)
      if(bbuf_tbl1%ierr_bin .gt. 0) go to 99
      call write_each_itp_idx_table_b(id_rank,                          &
     &    itp_tbl2_org_IO, itp_tbl2_dest_IO, bbuf_tbl1)
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
      subroutine read_dbl_itp_tbl_coef_file_b(file_name, id_rank,       &
     &          itp_tbl1_org_IO, itp_tbl1_dest_IO,                      &
     &          itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
!
      use itrplte_table_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
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
     &  'Read Binary interpolation table file: ', trim(file_name)
      bbuf_tbl1%id_binary = id_read_tbl
      call open_read_binary_file(file_name, id_rank, bbuf_tbl1)
      if(bbuf_tbl1%ierr_bin .ne. 0) goto 99
!
      call read_each_itp_coef_table_b(id_rank,                          &
     &    itp_tbl1_org_IO, itp_tbl1_dest_IO, bbuf_tbl1)
      if(bbuf_tbl1%ierr_bin .gt. 0) goto 99
!
      call read_each_itp_coef_table_b(id_rank,                          &
     &    itp_tbl2_org_IO, itp_tbl2_dest_IO, bbuf_tbl1)
!
  99  continue
      call close_binary_file(bbuf_tbl1)
      ierr = bbuf_tbl1%ierr_bin
!
      end subroutine read_dbl_itp_tbl_coef_file_b
!
!-----------------------------------------------------------------------
!
      subroutine read_dbl_itp_tbl_idx_file_b(file_name, id_rank,        &
     &          itp_tbl1_org_IO, itp_tbl1_dest_IO,                      &
     &          itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
!
      use itrplte_table_data_IO_b
!
      character(len=kchara), intent(in) :: file_name
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
     &  'Read Binary interpolation table file: ', trim(file_name)
      bbuf_tbl1%id_binary = id_read_tbl
      call open_read_binary_file(file_name, id_rank, bbuf_tbl1)
      if(bbuf_tbl1%ierr_bin .ne. 0) goto 99
!
      call read_each_itp_idx_table_b(id_rank,                           &
     &    itp_tbl1_org_IO, itp_tbl1_dest_IO, bbuf_tbl1)
      if(bbuf_tbl1%ierr_bin .gt. 0) goto 99
!
      call read_each_itp_idx_table_b(id_rank,                           &
     &    itp_tbl2_org_IO, itp_tbl2_dest_IO, bbuf_tbl1)
!
  99  continue
      call close_binary_file(bbuf_tbl1)
      ierr = bbuf_tbl1%ierr_bin
!
      end subroutine read_dbl_itp_tbl_idx_file_b
!
!-----------------------------------------------------------------------
!
      end module dbl_itrplte_tbl_file_IO_b

!>@file   itp_table_file_IO_b.f90
!!@brief  module itp_table_file_IO_b
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!!
!>@brief  Binary interpolation file IO
!!
!!@verbatim
!!      subroutine write_itp_table_file_b                               &
!!     &         (file_name, id_rank, itp_tbl_IO, ierr)
!!        type(interpolate_table), intent(in) :: itp_tbl_IO
!!      subroutine write_dbl_itp_table_file_b                           &
!!     &         (file_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
!!        type(interpolate_table), intent(in) :: itp_tbl1_IO
!!        type(interpolate_table), intent(in) :: itp_tbl2_IO
!!      subroutine read_itp_table_file_b                                &
!!     &          (file_name, id_rank, itp_tbl_IO, ierr)
!!        type(interpolate_table), intent(inout) :: itp_tbl_IO
!!      subroutine read_dbl_itp_table_file_b                            &
!!     &          (file_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
!!        type(interpolate_table), intent(inout) :: itp_tbl1_IO
!!       type(interpolate_table), intent(inout) :: itp_tbl2_IO
!!@endverbatim
!
      module itp_table_file_IO_b
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
      use itp_table_data_IO_b
      use binary_IO
!
      implicit none
!
      integer(kind = kint), parameter :: id_read_tbl =  21
      integer(kind = kint), parameter :: id_write_tbl = 22
      type(binary_IO_buffer) :: bbuf_tbl1
      private :: id_read_tbl, id_write_tbl, bbuf_tbl1
!
      private :: read_each_itp_table_b, write_each_itp_table_b
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_itp_table_file_b                                 &
     &         (file_name, id_rank, itp_tbl_IO, ierr)
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
      call write_each_itp_table_b(id_rank, itp_tbl_IO, bbuf_tbl1)
!
  99  continue
      call close_binary_file(bbuf_tbl1)
      ierr = bbuf_tbl1%ierr_bin
!
      end subroutine write_itp_table_file_b
!
!-----------------------------------------------------------------------
!
      subroutine write_dbl_itp_table_file_b                             &
     &         (file_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
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
      call write_each_itp_table_b(id_rank, itp_tbl1_IO, bbuf_tbl1)
      if(bbuf_tbl1%ierr_bin .gt. 0) go to 99
      call write_each_itp_table_b(id_rank, itp_tbl2_IO, bbuf_tbl1)
!
  99  continue
      call close_binary_file(bbuf_tbl1)
      ierr = bbuf_tbl1%ierr_bin
!
      end subroutine write_dbl_itp_table_file_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_itp_table_file_b                                  &
     &          (file_name, id_rank, itp_tbl_IO, ierr)
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
      call read_each_itp_table_b(id_rank, itp_tbl_IO, bbuf_tbl1)
!
  99  continue
      call close_binary_file(bbuf_tbl1)
      ierr = bbuf_tbl1%ierr_bin
!
      end subroutine read_itp_table_file_b
!
!-----------------------------------------------------------------------
!
      subroutine read_dbl_itp_table_file_b                              &
     &          (file_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
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
      call read_each_itp_table_b(id_rank, itp_tbl1_IO, bbuf_tbl1)
      if(bbuf_tbl1%ierr_bin .gt. 0) goto 99
!
      call read_each_itp_table_b(id_rank, itp_tbl2_IO, bbuf_tbl1)
!
  99  continue
      call close_binary_file(bbuf_tbl1)
      ierr = bbuf_tbl1%ierr_bin
!
      end subroutine read_dbl_itp_table_file_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_each_itp_table_b(id_rank, itp_tbl_IO, bbuf_tbl)
!
      integer, intent(in) :: id_rank
      type(interpolate_table), intent(in) :: itp_tbl_IO
      type(binary_IO_buffer), intent(inout) :: bbuf_tbl
!
!
      call write_interpolate_table_dest_b                               &
     &   (id_rank, itp_tbl_IO%tbl_dest, bbuf_tbl)
      if(bbuf_tbl%ierr_bin .gt. 0) return
!
      call write_interpolate_table_org_b                                &
     &   (id_rank, itp_tbl_IO%tbl_org, bbuf_tbl)
      if(bbuf_tbl%ierr_bin .gt. 0) return
      call write_interpolate_coefs_org_b(itp_tbl_IO%tbl_org, bbuf_tbl)
      if(bbuf_tbl%ierr_bin .gt. 0) return
!
      end subroutine write_each_itp_table_b
!
!-----------------------------------------------------------------------
!
      subroutine read_each_itp_table_b(id_rank, itp_tbl_IO, bbuf_tbl)
!
      integer, intent(in) :: id_rank
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
      type(binary_IO_buffer), intent(inout) :: bbuf_tbl
!
      integer(kind = kint) :: n_rank_file
!
!
      call read_interpolate_domain_dest_b                               &
     &   (bbuf_tbl, n_rank_file, itp_tbl_IO%tbl_dest)
      if(bbuf_tbl%ierr_bin .gt. 0) go to 99
!
      call read_interpolate_table_dest_b(bbuf_tbl, itp_tbl_IO%tbl_dest)
      if(bbuf_tbl%ierr_bin .ne. 0) go to 99
!
      call read_interpolate_domain_org_b                                &
     &   (bbuf_tbl, n_rank_file, itp_tbl_IO%tbl_org)
      if(bbuf_tbl%ierr_bin .gt. 0) go to 99
!
      call read_interpolate_table_org_b(bbuf_tbl, itp_tbl_IO%tbl_org)
      if(bbuf_tbl%ierr_bin .ne. 0) go to 99
!
      call read_interpolate_coefs_org_b(bbuf_tbl, itp_tbl_IO%tbl_org)
  99  continue
      if (n_rank_file .ne. id_rank) bbuf_tbl%ierr_bin = n_rank_file
!
      end subroutine read_each_itp_table_b
!
!-----------------------------------------------------------------------
!
      end module itp_table_file_IO_b

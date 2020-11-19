!itp_table_file_IO_b.f90
!      module itp_table_file_IO_b
!
!        programmed by H.Matsui on Sep. 2006 (ver 1.2)
!
!!      subroutine write_itp_table_file_b                               &
!!     &         (file_name, id_rank, itp_tbl_IO, ierr)
!!      subroutine read_itp_table_file_b                                &
!!     &          (file_name, id_rank, itp_tbl_IO, ierr)
!!        type(interpolate_table_org), intent(inout) :: IO_itp_org
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!
!!      subroutine write_itp_coefs_dest_file_b                          &
!!     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!!      subroutine read_itp_coefs_dest_file_b                           &
!!     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!!      subroutine read_itp_table_dest_file_b                           &
!!     &         (file_name, id_rank, IO_itp_dest, ierr)
!!      subroutine read_itp_domain_dest_file_b                          &
!!     &         (file_name, id_rank, IO_itp_dest, ierr)
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
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
      type(binary_IO_buffer) :: bbuf_tbl
      private :: id_read_tbl, id_write_tbl, bbuf_tbl
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
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      bbuf_tbl%id_binary = id_write_tbl
      call open_write_binary_file(file_name, bbuf_tbl)
      if(bbuf_tbl%ierr_bin .gt. 0) go to 99
      call write_interpolate_table_dest_b                               &
     &   (id_rank, itp_tbl_IO%tbl_dest, bbuf_tbl)
      if(bbuf_tbl%ierr_bin .gt. 0) go to 99
!
      call write_interpolate_table_org_b                                &
     &   (id_rank, itp_tbl_IO%tbl_org, bbuf_tbl)
      if(bbuf_tbl%ierr_bin .gt. 0) go to 99
      call write_interpolate_coefs_org_b(itp_tbl_IO%tbl_org, bbuf_tbl)
      if(bbuf_tbl%ierr_bin .gt. 0) go to 99
!
  99  continue
      call close_binary_file(bbuf_tbl)
      ierr = bbuf_tbl%ierr_bin
!
      if (itp_tbl_IO%tbl_org%num_dest_domain .gt. 0) then
        call dealloc_itp_table_org(itp_tbl_IO%tbl_org)
        call dealloc_itp_num_org(itp_tbl_IO%tbl_org)
      end if
!
      if (itp_tbl_IO%tbl_dest%num_org_domain .gt. 0) then
        call dealloc_itp_table_dest(itp_tbl_IO%tbl_dest)
        call dealloc_itp_num_dest(itp_tbl_IO%tbl_dest)
      end if
!
      end subroutine write_itp_table_file_b
!
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
      integer(kind = kint) :: n_rank_file
!
!
      bbuf_tbl%id_binary = id_read_tbl
      call open_read_binary_file(file_name, id_rank, bbuf_tbl)
      if(bbuf_tbl%ierr_bin .ne. 0) goto 99
      call read_interpolate_domain_dest_b                               &
     &   (bbuf_tbl, n_rank_file, itp_tbl_IO%tbl_dest)
      if(bbuf_tbl%ierr_bin .gt. 0) goto 99
!
      call read_interpolate_table_dest_b(bbuf_tbl, itp_tbl_IO%tbl_dest)
      if(bbuf_tbl%ierr_bin .ne. 0) goto 99
!
      call read_interpolate_domain_org_b                                &
     &   (bbuf_tbl, n_rank_file, itp_tbl_IO%tbl_org)
      if(bbuf_tbl%ierr_bin .gt. 0) goto 99
!
      call read_interpolate_table_org_b(bbuf_tbl, itp_tbl_IO%tbl_org)
      if(bbuf_tbl%ierr_bin .ne. 0) goto 99
!
      call read_interpolate_coefs_org_b(bbuf_tbl, itp_tbl_IO%tbl_org)
!
  99  continue
      call close_binary_file(bbuf_tbl)
      ierr = bbuf_tbl%ierr_bin
!
      if (n_rank_file .ne. id_rank) ierr = n_rank_file
!
      end subroutine read_itp_table_file_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_itp_coefs_dest_file_b                            &
     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
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
      call write_interpolate_coefs_dest_b                               &
     &   (IO_itp_dest, IO_itp_c_dest, bbuf_tbl)
!
  99  continue
      call close_binary_file(bbuf_tbl)
      ierr = bbuf_tbl%ierr_bin
!
      if (IO_itp_dest%num_org_domain .gt. 0) then
        call dealloc_itp_coef_dest(IO_itp_c_dest)
        call dealloc_itp_table_dest(IO_itp_dest)
        call dealloc_itp_num_dest(IO_itp_dest)
        call dealloc_itp_coef_stack(IO_itp_c_dest)
      end if
!
      end subroutine write_itp_coefs_dest_file_b
!
!-----------------------------------------------------------------------
!
      subroutine read_itp_coefs_dest_file_b                             &
     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
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
      subroutine read_itp_table_dest_file_b                             &
     &         (file_name, id_rank, IO_itp_dest, ierr)
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
      end module itp_table_file_IO_b

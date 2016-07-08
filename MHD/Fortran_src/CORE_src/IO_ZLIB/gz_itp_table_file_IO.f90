!gz_itp_table_file_IO.f90
!      module gz_itp_table_file_IO
!
!        programmed by H.Matsui on Sep. 2006 (ver 1.2)
!
!      subroutine gz_write_itp_table_file(file_name, my_rank)
!      subroutine gz_read_itp_table_file(file_name, my_rank, ierr)
!
!      subroutine gz_write_itp_coefs_dest_file(file_name, my_rank)
!      subroutine gz_read_itp_coefs_dest_file(file_name, my_rank, ierr)
!      subroutine gz_read_itp_table_dest_file(file_name, my_rank, ierr)
!      subroutine gz_read_itp_domain_dest_file(file_name, my_rank, ierr)
!
      module gz_itp_table_file_IO
!
      use m_precision
      use m_error_IDs
!
      use m_interpolate_table_dest_IO
      use m_interpolate_table_org_IO
!
      use set_parallel_file_name
      use gz_itp_table_data_IO
!
      implicit none
!
      character(len=kchara), private  :: gzip_name
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine gz_write_itp_table_file(file_name, my_rank)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind= kint), intent(in) :: my_rank
!
!
      call add_gzip_extension(file_name, gzip_name)
      call open_wt_gzfile(gzip_name)
!
      call write_gz_itp_table_dest(my_rank)
!
      call write_gz_itp_table_org(my_rank)
      call write_gz_itp_coefs_org
!
      call close_gzfile
!
      if (num_dest_domain_IO .gt. 0) then
        call dealloc_itp_table_org(IO_itp_org)
        call deallocate_itp_num_org_IO
      end if
!
      if (num_org_domain_IO .gt. 0) then
        call deallocate_itp_nod_dst_IO
        call deallocate_itp_num_dst_IO
      end if
!
      end subroutine gz_write_itp_table_file
!
!-----------------------------------------------------------------------
!
      subroutine gz_read_itp_table_file(file_name, my_rank, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: n_rank_file
!
!
      call add_gzip_extension(file_name, gzip_name)
      call open_rd_gzfile(gzip_name)
!
!        write(*,*) 'read_gz_itp_domain_dest', trim(file_name)
      call read_gz_itp_domain_dest(n_rank_file)
!        write(*,*) 'read_gz_itp_table_dest'
      call read_gz_itp_table_dest
!
!        write(*,*) 'read_gz_itp_domain_org'
      call read_gz_itp_domain_org(n_rank_file)
!        write(*,*) 'read_gz_itp_coefs_org'
      call read_gz_itp_table_org
      call read_gz_itp_coefs_org
!
      call close_gzfile
!
      ierr = 0
      if (n_rank_file .ne. my_rank) ierr = n_rank_file
!
      end subroutine gz_read_itp_table_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine gz_write_itp_coefs_dest_file(file_name, my_rank)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind= kint), intent(in) :: my_rank
!
!
      call add_gzip_extension(file_name, gzip_name)
      call open_wt_gzfile(gzip_name)
!
      call write_gz_itp_table_dest(my_rank)
      call write_gz_itp_coefs_dest
      call close_gzfile
!
      if (num_org_domain_IO .gt. 0) then
        call deallocate_itp_coefs_dst_IO
        call deallocate_itp_nod_dst_IO
        call deallocate_itp_num_dst_IO
      end if
!
      end subroutine gz_write_itp_coefs_dest_file
!
!-----------------------------------------------------------------------
!
      subroutine gz_read_itp_coefs_dest_file(file_name, my_rank, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind= kint), intent(in) :: my_rank
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: n_rank_file
!
!
      call add_gzip_extension(file_name, gzip_name)
      call open_rd_gzfile(gzip_name)
!
      call read_gz_itp_domain_dest(n_rank_file)
      call read_gz_itp_table_dest
      call read_gz_itp_coefs_dest
      call close_gzfile
!
      ierr = 0
      if (n_rank_file .ne. my_rank) ierr = ierr_file
!
      end subroutine gz_read_itp_coefs_dest_file
!
!-----------------------------------------------------------------------
!
      subroutine gz_read_itp_table_dest_file(file_name, my_rank, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind= kint), intent(in) :: my_rank
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: n_rank_file
!
!
      call add_gzip_extension(file_name, gzip_name)
      call open_rd_gzfile(gzip_name)
!
      call read_gz_itp_domain_dest(n_rank_file)
      call read_gz_itp_table_dest
      call close_gzfile
!
      ierr = 0
      if (n_rank_file .ne. my_rank) ierr = ierr_file
!
      end subroutine gz_read_itp_table_dest_file
!
!-----------------------------------------------------------------------
!
      subroutine gz_read_itp_domain_dest_file(file_name, my_rank, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: n_rank_file
!
!
      call add_gzip_extension(file_name, gzip_name)
      call open_rd_gzfile(gzip_name)
!
      call read_gz_itp_domain_dest(n_rank_file)
      call close_gzfile
!
      ierr = 0
      if (n_rank_file .ne. my_rank) ierr = ierr_file
!
      end subroutine gz_read_itp_domain_dest_file
!
!-----------------------------------------------------------------------
!
      end module gz_itp_table_file_IO

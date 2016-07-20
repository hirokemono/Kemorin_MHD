!gz_itp_table_file_IO.f90
!      module gz_itp_table_file_IO
!
!        programmed by H.Matsui on Sep. 2006 (ver 1.2)
!
!!      subroutine gz_write_itp_table_file                              &
!!     &         (file_name, my_rank, IO_itp_org, IO_itp_dest)
!!      subroutine gz_read_itp_table_file                               &
!!     &         (file_name, my_rank, IO_itp_org, IO_itp_dest, ierr)
!!        type(interpolate_table_org), intent(inout) :: IO_itp_org
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!
!!      subroutine gz_write_itp_coefs_dest_file                         &
!!     &         (file_name, my_rank, IO_itp_dest, IO_itp_c_dest)
!!      subroutine gz_read_itp_coefs_dest_file                          &
!!     &         (file_name, my_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!!      subroutine gz_read_itp_table_dest_file                          &
!!     &         (file_name, my_rank, IO_itp_dest, ierr)
!!      subroutine gz_read_itp_domain_dest_file                         &
!!     &         (file_name, my_rank, IO_itp_dest, ierr)
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
      module gz_itp_table_file_IO
!
      use m_precision
      use m_error_IDs
!
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
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
      subroutine gz_write_itp_table_file                                &
     &         (file_name, my_rank, IO_itp_org, IO_itp_dest)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind= kint), intent(in) :: my_rank
!
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
!
      call add_gzip_extension(file_name, gzip_name)
      call open_wt_gzfile(gzip_name)
!
      call write_gz_itp_table_dest(my_rank, IO_itp_dest)
!
      call write_gz_itp_table_org(my_rank, IO_itp_org)
      call write_gz_itp_coefs_org(IO_itp_org)
!
      call close_gzfile
!
      if (IO_itp_org%num_dest_domain .gt. 0) then
        call dealloc_itp_table_org(IO_itp_org)
        call dealloc_itp_num_org(IO_itp_org)
      end if
!
      if (IO_itp_dest%num_org_domain .gt. 0) then
        call dealloc_itp_table_dest(IO_itp_dest)
        call dealloc_itp_num_dest(IO_itp_dest)
      end if
!
      end subroutine gz_write_itp_table_file
!
!-----------------------------------------------------------------------
!
      subroutine gz_read_itp_table_file                                 &
     &         (file_name, my_rank, IO_itp_org, IO_itp_dest, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: n_rank_file
!
!
      call add_gzip_extension(file_name, gzip_name)
      call open_rd_gzfile(gzip_name)
!
!        write(*,*) 'read_gz_itp_domain_dest', trim(file_name)
      call read_gz_itp_domain_dest(n_rank_file, IO_itp_dest)
!        write(*,*) 'read_gz_itp_table_dest'
      call read_gz_itp_table_dest(IO_itp_dest)
!
!        write(*,*) 'read_gz_itp_domain_org'
      call read_gz_itp_domain_org(n_rank_file, IO_itp_org)
!        write(*,*) 'read_gz_itp_coefs_org'
      call read_gz_itp_table_org(IO_itp_org)
      call read_gz_itp_coefs_org(IO_itp_org)
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
      subroutine gz_write_itp_coefs_dest_file                           &
     &         (file_name, my_rank, IO_itp_dest, IO_itp_c_dest)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind= kint), intent(in) :: my_rank
!
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
!
      call add_gzip_extension(file_name, gzip_name)
      call open_wt_gzfile(gzip_name)
!
      call write_gz_itp_table_dest(my_rank, IO_itp_dest)
      call write_gz_itp_coefs_dest(IO_itp_dest, IO_itp_c_dest)
      call close_gzfile
!
      if (IO_itp_dest%num_org_domain .gt. 0) then
        call dealloc_itp_coef_dest(IO_itp_c_dest)
        call dealloc_itp_coef_stack(IO_itp_c_dest)
        call dealloc_itp_table_dest(IO_itp_dest)
        call dealloc_itp_num_dest(IO_itp_dest)
      end if
!
      end subroutine gz_write_itp_coefs_dest_file
!
!-----------------------------------------------------------------------
!
      subroutine gz_read_itp_coefs_dest_file                            &
     &         (file_name, my_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind= kint), intent(in) :: my_rank
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
      integer(kind = kint) :: n_rank_file
!
!
      call add_gzip_extension(file_name, gzip_name)
      call open_rd_gzfile(gzip_name)
!
      call read_gz_itp_domain_dest(n_rank_file, IO_itp_dest)
      call read_gz_itp_table_dest(IO_itp_dest)
      call read_gz_itp_coefs_dest(IO_itp_dest, IO_itp_c_dest)
      call close_gzfile
!
      ierr = 0
      if (n_rank_file .ne. my_rank) ierr = ierr_file
!
      end subroutine gz_read_itp_coefs_dest_file
!
!-----------------------------------------------------------------------
!
      subroutine gz_read_itp_table_dest_file                            &
     &         (file_name, my_rank, IO_itp_dest, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind= kint), intent(in) :: my_rank
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
      integer(kind = kint) :: n_rank_file
!
!
      call add_gzip_extension(file_name, gzip_name)
      call open_rd_gzfile(gzip_name)
!
      call read_gz_itp_domain_dest(n_rank_file, IO_itp_dest)
      call read_gz_itp_table_dest(IO_itp_dest)
      call close_gzfile
!
      ierr = 0
      if (n_rank_file .ne. my_rank) ierr = ierr_file
!
      end subroutine gz_read_itp_table_dest_file
!
!-----------------------------------------------------------------------
!
      subroutine gz_read_itp_domain_dest_file                           &
     &         (file_name, my_rank, IO_itp_dest, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: my_rank
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
      integer(kind = kint) :: n_rank_file
!
!
      call add_gzip_extension(file_name, gzip_name)
      call open_rd_gzfile(gzip_name)
!
      call read_gz_itp_domain_dest(n_rank_file, IO_itp_dest)
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

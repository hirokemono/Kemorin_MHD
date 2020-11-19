!>@file  gz_MPI_itp_table_file_IO.f90
!!       module gz_MPI_itp_table_file_IO
!!
!!@author H. Matsui
!!@date   Programmed in Sep. 2012
!!
!> @brief gzipped file IO for interpolation
!!
!!@verbatim
!!      subroutine gz_mpi_write_itp_table_file                          &
!!     &         (file_name, id_rank, itp_tbl_IO)
!!      subroutine gz_mpi_read_itp_table_file                           &
!!     &         (file_name, id_rank, itp_tbl_IO, ierr)
!!        type(interpolate_table), intent(inout) :: itp_tbl_IO
!!
!!      subroutine gz_mpi_wrt_itp_coefs_dest_file                       &
!!     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest)
!!      subroutine gz_mpi_read_itp_coefs_dest_file                      &
!!     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!!      subroutine gz_mpi_read_itp_tbl_dest_file                        &
!!     &         (file_name, id_rank, IO_itp_dest, ierr)
!!      subroutine gz_mpi_read_itp_dmn_dest_file                        &
!!     &         (file_name, id_rank, IO_itp_dest, ierr)
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!!@endverbatim
!
      module gz_MPI_itp_table_file_IO
!
      use m_precision
      use m_error_IDs
!
      use t_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
      use t_calypso_mpi_IO_param
!
      use set_parallel_file_name
      use gz_itp_table_data_IO
      use gz_binary_IO
!
      implicit none
!
      character(len=kchara), private  :: gzip_name
      type(buffer_4_gzip), private :: zbuf_itp
      type(calypso_MPI_IO_params), save, private :: IO_param
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_write_itp_table_file                            &
     &         (file_name, id_rank, itp_tbl_IO)
!
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
!
!
      gzip_name = add_gzip_extension(file_name)
      call open_wt_gzfile_a(gzip_name, zbuf_itp)
!
!      call write_gz_itp_table_dest                                     &
!     &   (id_rank, itp_tbl_IO%tbl_dest, zbuf_itp)
!
!      call write_gz_itp_table_org                                      &
!     &   (id_rank, itp_tbl_IO%tbl_org, zbuf_itp)
!      call write_gz_itp_coefs_org(itp_tbl_IO%tbl_org, zbuf_itp)
!
      call close_gzfile_a(zbuf_itp)
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
      end subroutine gz_mpi_write_itp_table_file
!
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_read_itp_table_file                             &
     &         (file_name, id_rank, itp_tbl_IO, ierr)
!
      use skip_gz_comment
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
      gzip_name = add_gzip_extension(file_name)
      call open_rd_gzfile_a(gzip_name, zbuf_itp)
!
!        write(*,*) 'read_gz_itp_domain_dest', trim(file_name)
!      call read_gz_itp_domain_dest                                     &
!     &   (n_rank_file, itp_tbl_IO%tbl_dest, zbuf_itp)
!        write(*,*) 'read_gz_itp_table_dest'
!      call read_gz_itp_table_dest(itp_tbl_IO%tbl_dest, zbuf_itp)
!
!        write(*,*) 'read_gz_itp_domain_org'
!      call read_gz_itp_domain_org                                      &
!     &   (n_rank_file, itp_tbl_IO%tbl_org, zbuf_itp)
!        write(*,*) 'read_gz_itp_coefs_org'
!      call read_gz_itp_table_org(itp_tbl_IO%tbl_org, zbuf_itp)
!      call read_gz_itp_coefs_org(itp_tbl_IO%tbl_org, zbuf_itp)
!
      call close_gzfile_a(zbuf_itp)
!
      ierr = 0
      if (n_rank_file .ne. id_rank) ierr = n_rank_file
!
      end subroutine gz_mpi_read_itp_table_file
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_wrt_itp_coefs_dest_file                         &
     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest)
!
      use skip_gz_comment
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
!
      gzip_name = add_gzip_extension(file_name)
      call open_wt_gzfile_a(gzip_name, zbuf_itp)
!
!      call write_gz_itp_table_dest(id_rank, IO_itp_dest, zbuf_itp)
!      call write_gz_itp_coefs_dest                                     &
!     &   (IO_itp_dest, IO_itp_c_dest, zbuf_itp)
      call close_gzfile_a(zbuf_itp)
!
      if (IO_itp_dest%num_org_domain .gt. 0) then
        call dealloc_itp_coef_dest(IO_itp_c_dest)
        call dealloc_itp_coef_stack(IO_itp_c_dest)
        call dealloc_itp_table_dest(IO_itp_dest)
        call dealloc_itp_num_dest(IO_itp_dest)
      end if
!
      end subroutine gz_mpi_wrt_itp_coefs_dest_file
!
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_read_itp_coefs_dest_file                        &
     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!
      use skip_gz_comment
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
      gzip_name = add_gzip_extension(file_name)
      call open_rd_gzfile_a(gzip_name, zbuf_itp)
!
!      call read_gz_itp_domain_dest(n_rank_file, IO_itp_dest, zbuf_itp)
!      call read_gz_itp_table_dest(IO_itp_dest, zbuf_itp)
!      call read_gz_itp_coefs_dest(IO_itp_dest, IO_itp_c_dest, zbuf_itp)
      call close_gzfile_a(zbuf_itp)
!
      ierr = 0
      if (n_rank_file .ne. id_rank) ierr = ierr_file
!
      end subroutine gz_mpi_read_itp_coefs_dest_file
!
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_read_itp_tbl_dest_file                          &
     &         (file_name, id_rank, IO_itp_dest, ierr)
!
      use skip_gz_comment
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
      gzip_name = add_gzip_extension(file_name)
      call open_rd_gzfile_a(gzip_name, zbuf_itp)
!
!      call read_gz_itp_domain_dest(n_rank_file, IO_itp_dest, zbuf_itp)
!      call read_gz_itp_table_dest(IO_itp_dest, zbuf_itp)
      call close_gzfile_a(zbuf_itp)
!
      ierr = 0
      if (n_rank_file .ne. id_rank) ierr = ierr_file
!
      end subroutine gz_mpi_read_itp_tbl_dest_file
!
!-----------------------------------------------------------------------
!
      subroutine gz_mpi_read_itp_dmn_dest_file                          &
     &         (file_name, id_rank, IO_itp_dest, ierr)
!
      use skip_gz_comment
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
      gzip_name = add_gzip_extension(file_name)
      call open_rd_gzfile_a(gzip_name, zbuf_itp)
!
!      call read_gz_itp_domain_dest(n_rank_file, IO_itp_dest, zbuf_itp)
      call close_gzfile_a(zbuf_itp)
!
      ierr = 0
      if (n_rank_file .ne. id_rank) ierr = ierr_file
!
      end subroutine gz_mpi_read_itp_dmn_dest_file
!
!-----------------------------------------------------------------------
!
      end module gz_MPI_itp_table_file_IO

!>@file  gz_MPI_itp_table_file_IO_b.f90
!!       module gz_MPI_itp_table_file_IO_b
!!
!!@author H. Matsui
!!@date   Programmed in Sep. 2012
!!
!> @brief gzipped binary file IO for interpolation
!!
!!@verbatim
!!      subroutine write_gz_mpi_itp_table_file_b                        &
!!     &         (file_name, id_rank, itp_tbl_IO, ierr)
!!      subroutine read_gz_mpi_itp_table_file_b                         &
!!     &          (file_name, id_rank, num_pe, itp_tbl_IO, ierr)
!!        type(interpolate_table), intent(inout) :: itp_tbl_IO
!!
!!      subroutine wrt_gz_mpi_itp_coef_dest_file_b                      &
!!     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!!      subroutine read_gz_mpi_itp_coef_dst_file_b                      &
!!     &         (file_name, id_rank, num_pe,                           &
!!     &          IO_itp_dest, IO_itp_c_dest, ierr)
!!      subroutine read_gz_mpi_itp_tbl_dest_file_b                      &
!!     &         (file_name, id_rank, num_pe, IO_itp_dest, ierr)
!!      subroutine read_gz_mpi_itp_dmn_dest_file_b                      &
!!     &         (file_name, id_rank, num_pe, IO_itp_dest, ierr)
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!!@endverbatim
!
      module gz_MPI_itp_table_file_IO_b
!
      use m_precision
      use m_error_IDs
!
      use calypso_mpi
      use t_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
      use t_binary_IO_buffer
      use t_calypso_mpi_IO_param
!
      use t_buffer_4_gzip
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
      subroutine write_gz_itp_table_file_b                              &
     &         (file_name, id_rank, itp_tbl_IO, ierr)
!
      use set_parallel_file_name
      use gz_MPI_itp_table_data_IO_b
      use gz_MPI_binary_datum_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      gzip_name = add_gzip_extension(file_name)
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write gzipped binary interpolation file: ', trim(gzip_name)
!
      call open_write_gz_mpi_file_b(gzip_name, IO_param)
!      call write_gz_mpi_itp_table_dest_b                               &
!     &   (id_rank, itp_tbl_IO%tbl_dest, zbuf_itp)
!      if(zbuf_itp%ierr_zlib .gt. 0) go to 99
!
!      call write_gz_mpi_itp_table_org_b                                &
!     &    (id_rank, itp_tbl_IO%tbl_org, zbuf_itp)
!      if(zbuf_itp%ierr_zlib .gt. 0) go to 99
!      call write_gz_mpi_itp_coefs_org_b(itp_tbl_IO%tbl_org, zbuf_itp)
!      if(zbuf_itp%ierr_zlib .gt. 0) go to 99
!
  99  continue
      call close_mpi_file(IO_param)
      ierr = zbuf_itp%ierr_zlib
!
      if (itp_tbl_IO%tbl_org%num_dest_domain .gt. 0) then
        call dealloc_itp_table_org(itp_tbl_IO%tbl_org)
        call dealloc_itp_num_org(itp_tbl_IO%tbl_org)
      end if
!
      if (itp_tbl_IO%tbl_dest%num_org_domain .gt. 0) then
        call dealloc_itp_table_dest(itp_tbl_IO%tbl_dest)
      end if
      call dealloc_itp_num_dest(itp_tbl_IO%tbl_dest)
!
      end subroutine write_gz_itp_table_file_b
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_mpi_itp_table_file_b                           &
     &          (file_name, id_rank, num_pe, itp_tbl_IO, ierr)
!
      use set_parallel_file_name
      use gz_MPI_itp_table_data_IO_b
      use gz_MPI_binary_datum_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank, num_pe
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: n_rank_file
!
!
      gzip_name = add_gzip_extension(file_name)
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary interpolation file: ', trim(gzip_name)
!
      call open_read_gz_mpi_file_b                                      &
     &   (gzip_name, num_pe, id_rank, IO_param)
!      call read_gz_mpi_itp_domain_dest_b                               &
!     &   (zbuf_itp, n_rank_file, itp_tbl_IO%tbl_dest)
!      if(zbuf_itp%ierr_zlib .gt. 0) goto 99
!
!      call read_gz_mpi_itp_table_dest_b(zbuf_itp, itp_tbl_IO%tbl_dest)
!      if(zbuf_itp%ierr_zlib .ne. 0) goto 99
!
!      call read_gz_mpi_itp_domain_org_b                                &
!     &    (zbuf_itp, n_rank_file, itp_tbl_IO%tbl_org)
!      if(zbuf_itp%ierr_zlib .gt. 0) goto 99
!
!      call read_gz_mpi_itp_table_org_b(zbuf_itp, itp_tbl_IO%tbl_org)
!      if(zbuf_itp%ierr_zlib .ne. 0) goto 99
!
!      call read_gz_mpi_itp_coefs_org_b(zbuf_itp, itp_tbl_IO%tbl_org)
!
  99  continue
      call close_mpi_file(IO_param)
      ierr = zbuf_itp%ierr_zlib
!
      if (n_rank_file .ne. id_rank) ierr = n_rank_file
!
      end subroutine read_gz_mpi_itp_table_file_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine wrt_gz_mpi_itp_coef_dest_file_b                        &
     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!
      use set_parallel_file_name
      use gz_MPI_itp_table_data_IO_b
      use gz_MPI_binary_datum_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
      integer(kind = kint), intent(inout) :: ierr
!
!
      gzip_name = add_gzip_extension(file_name)
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write gzipped binary export coefs file: ', trim(gzip_name)
!
      call open_write_gz_mpi_file_b(gzip_name, IO_param)
!      if(zbuf_itp%ierr_zlib .gt. 0) go to 99
!      call write_gz_mpi_itp_table_dest_b                               &
!     &   (id_rank, IO_itp_dest, zbuf_itp)
!      if(zbuf_itp%ierr_zlib .gt. 0) go to 99
!
!      call write_gz_mpi_itp_coefs_dest_b                               &
!     &   (IO_itp_dest, IO_itp_c_dest, zbuf_itp)
!
  99  continue
      call close_mpi_file(IO_param)
      ierr = zbuf_itp%ierr_zlib
!
      if (IO_itp_dest%num_org_domain .gt. 0) then
        call dealloc_itp_coef_dest(IO_itp_c_dest)
        call dealloc_itp_table_dest(IO_itp_dest)
        call dealloc_itp_coef_stack(IO_itp_c_dest)
      end if
      call dealloc_itp_num_dest(IO_itp_dest)
!
      end subroutine wrt_gz_mpi_itp_coef_dest_file_b
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_mpi_itp_coef_dst_file_b                        &
     &         (file_name, id_rank, num_pe,                             &
     &          IO_itp_dest, IO_itp_c_dest, ierr)
!
      use set_parallel_file_name
      use gz_MPI_itp_table_data_IO_b
      use gz_MPI_binary_datum_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank, num_pe
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
      integer(kind = kint) :: n_rank_file
! 
!
      gzip_name = add_gzip_extension(file_name)
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary export coefs file: ', trim(gzip_name)
!
      call open_read_gz_mpi_file_b                                      &
     &   (gzip_name, num_pe, id_rank, IO_param)
!      call read_gz_mpi_itp_domain_dest_b                               &
!     &   (zbuf_itp, n_rank_file, IO_itp_dest)
!      if(zbuf_itp%ierr_zlib .gt. 0) goto 99
!
!      call read_gz_mpi_itp_table_dest_b(zbuf_itp, IO_itp_dest)
!      if(zbuf_itp%ierr_zlib .ne. 0) goto 99
!
!      call read_gz_mpi_itp_coefs_dest_b                                &
!     &   (zbuf_itp, IO_itp_dest, IO_itp_c_dest)
!
  99  continue
      call close_mpi_file(IO_param)
      ierr = zbuf_itp%ierr_zlib
!
      if (n_rank_file .ne. id_rank) ierr = ierr_file
!
      end subroutine read_gz_mpi_itp_coef_dst_file_b
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_mpi_itp_tbl_dest_file_b                        &
     &         (file_name, id_rank, num_pe, IO_itp_dest, ierr)
!
      use set_parallel_file_name
      use gz_MPI_itp_table_data_IO_b
      use gz_MPI_binary_datum_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank, num_pe
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
      integer(kind = kint) :: n_rank_file
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary interapolate export file: ',              &
     &    trim(gzip_name)
!
      gzip_name = add_gzip_extension(file_name)
      call open_read_gz_mpi_file_b                                      &
     &   (gzip_name, num_pe, id_rank, IO_param)
!      call read_gz_mpi_itp_domain_dest_b                               &
!     &   (zbuf_itp, n_rank_file, IO_itp_dest)
!      if(zbuf_itp%ierr_zlib .gt. 0) goto 99
!
!      call read_gz_mpi_itp_table_dest_b(zbuf_itp, IO_itp_dest)
!
  99  continue
      call close_mpi_file(IO_param)
      ierr = zbuf_itp%ierr_zlib
!
      if (n_rank_file .ne. id_rank) ierr = ierr_file
!
      end subroutine read_gz_mpi_itp_tbl_dest_file_b
!
!-----------------------------------------------------------------------
!
      subroutine read_gz_mpi_itp_dmn_dest_file_b                        &
     &         (file_name, id_rank, num_pe, IO_itp_dest, ierr)
!
      use set_parallel_file_name
      use gz_MPI_itp_table_data_IO_b
      use gz_MPI_binary_datum_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank, num_pe
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
      integer(kind = kint) :: n_rank_file
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read gzipped binary export domain file: ', trim(gzip_name)
!
      gzip_name = add_gzip_extension(file_name)
      call open_read_gz_mpi_file_b                                      &
     &   (gzip_name, num_pe, id_rank, IO_param)
!      call read_gz_mpi_itp_domain_dest_b                               &
!     &   (zbuf_itp, n_rank_file, IO_itp_dest)
!
  99  continue
      call close_mpi_file(IO_param)
      ierr = zbuf_itp%ierr_zlib
!
      if (n_rank_file .ne. id_rank) ierr = ierr_file
!
      end subroutine read_gz_mpi_itp_dmn_dest_file_b
!
!-----------------------------------------------------------------------
!
      end module gz_MPI_itp_table_file_IO_b

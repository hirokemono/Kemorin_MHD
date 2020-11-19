!>@file   MPI_itp_table_file_IO_b.f90
!!@brief  module MPI_itp_table_file_IO_b
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!!
!>@brief  Binary interpolation file IO using MPi-IO
!!
!!@verbatim
!!      subroutine mpi_write_itp_table_file_b                           &
!!     &         (file_name, id_rank, itp_tbl_IO, ierr)
!!      subroutine mpi_read_itp_table_file_b                            &
!!     &          (file_name, id_rank, num_pe, itp_tbl_IO, ierr)
!!        type(interpolate_table_org), intent(inout) :: IO_itp_org
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!
!!      subroutine mpi_wrt_itp_coefs_dest_file_b                        &
!!     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!!      subroutine mpi_read_itp_coefs_dest_file_b                       &
!!     &         (file_name, id_rank, num_pe,                           &
!!     &          IO_itp_dest, IO_itp_c_dest, ierr)
!!      subroutine mpi_read_itp_table_dest_file_b                       &
!!     &         (file_name, id_rank, num_pe, IO_itp_dest, ierr)
!!      subroutine mpi_read_itp_domain_dest_file_b                      &
!!     &         (file_name, id_rank, num_pe, IO_itp_dest, ierr)
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!!@endverbatim
!
      module MPI_itp_table_file_IO_b
!
      use m_precision
      use m_error_IDs
!
      use t_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
      use t_binary_IO_buffer
      use t_calypso_mpi_IO_param
!
      use binary_IO
!
      implicit none
!
      integer(kind = kint), parameter :: id_read_tbl =  21
      integer(kind = kint), parameter :: id_write_tbl = 22
      type(binary_IO_buffer) :: bbuf_tbl
      type(calypso_MPI_IO_params), save, private :: IO_param
      private :: id_read_tbl, id_write_tbl, bbuf_tbl
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine mpi_write_itp_table_file_b                             &
     &         (file_name, id_rank, itp_tbl_IO, ierr)
!
      use MPI_itp_table_data_IO_b
      use MPI_ascii_data_IO
      use MPI_binary_head_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write binary interpolation file: ', trim(file_name)
!
      bbuf_tbl%id_binary = id_write_tbl
      call open_write_mpi_file_b(file_name, IO_param)
!      if(bbuf_tbl%ierr_bin .gt. 0) go to 99
!      call write_interpolate_table_dest_b                              &
!     &   (id_rank, itp_tbl_IO%tbl_dest, bbuf_tbl)
!      if(bbuf_tbl%ierr_bin .gt. 0) go to 99
!
!      call mpi_write_itp_table_org_b                                   &
!     &   (id_rank, itp_tbl_IO%tbl_org, bbuf_tbl)
!      if(bbuf_tbl%ierr_bin .gt. 0) go to 99
!      call mpi_write_itp_coefs_org_b(itp_tbl_IO%tbl_org, bbuf_tbl)
!      if(bbuf_tbl%ierr_bin .gt. 0) go to 99
!
  99  continue
      call close_mpi_file(IO_param)
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
      end subroutine mpi_write_itp_table_file_b
!
!-----------------------------------------------------------------------
!
      subroutine mpi_read_itp_table_file_b                              &
     &          (file_name, id_rank, num_pe, itp_tbl_IO, ierr)
!
      use MPI_itp_table_data_IO_b
      use MPI_ascii_data_IO
      use MPI_binary_head_IO
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
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read binary interpolation file: ', trim(file_name)
!
      bbuf_tbl%id_binary = id_read_tbl
      call open_read_mpi_file_b(file_name, num_pe, id_rank, IO_param)
!      if(bbuf_tbl%ierr_bin .ne. 0) goto 99
!      call mpi_read_itp_domain_dest_b                                  &
!     &   (bbuf_tbl, n_rank_file, itp_tbl_IO%tbl_dest)
!      if(bbuf_tbl%ierr_bin .gt. 0) goto 99
!
!      call mpi_read_itp_table_dest_b(bbuf_tbl, itp_tbl_IO%tbl_dest)
!      if(bbuf_tbl%ierr_bin .ne. 0) goto 99
!
!      call mpi_read_itp_domain_org_b                                   &
!     &   (bbuf_tbl, n_rank_file, itp_tbl_IO%tbl_org)
!      if(bbuf_tbl%ierr_bin .gt. 0) goto 99
!
!      call mpi_read_itp_table_org_b(bbuf_tbl, itp_tbl_IO%tbl_org)
!      if(bbuf_tbl%ierr_bin .ne. 0) goto 99
!
!      call mpi_read_itp_coefs_org_b(bbuf_tbl, itp_tbl_IO%tbl_org)
!
  99  continue
      call close_mpi_file(IO_param)
      ierr = bbuf_tbl%ierr_bin
!
      if (n_rank_file .ne. id_rank) ierr = n_rank_file
!
      end subroutine mpi_read_itp_table_file_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine mpi_wrt_itp_coefs_dest_file_b                          &
     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!
      use MPI_itp_table_data_IO_b
      use MPI_ascii_data_IO
      use MPI_binary_head_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
      integer(kind = kint), intent(inout) :: ierr
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write binary export coefs file: ', trim(file_name)
!
      bbuf_tbl%id_binary = id_write_tbl
      call open_write_mpi_file_b(file_name, IO_param)
!      if(bbuf_tbl%ierr_bin .gt. 0) go to 99
!      call mpi_write_itp_table_dest_b                                  &
!     &   (id_rank, IO_itp_dest, bbuf_tbl)
!      if(bbuf_tbl%ierr_bin .gt. 0) go to 99
!
!      call mpi_write_itp_coefs_dest_b                                  &
!     &   (IO_itp_dest, IO_itp_c_dest, bbuf_tbl)
!
  99  continue
      call close_mpi_file(IO_param)
      ierr = bbuf_tbl%ierr_bin
!
      if (IO_itp_dest%num_org_domain .gt. 0) then
        call dealloc_itp_coef_dest(IO_itp_c_dest)
        call dealloc_itp_table_dest(IO_itp_dest)
        call dealloc_itp_num_dest(IO_itp_dest)
        call dealloc_itp_coef_stack(IO_itp_c_dest)
      end if
!
      end subroutine mpi_wrt_itp_coefs_dest_file_b
!
!-----------------------------------------------------------------------
!
      subroutine mpi_read_itp_coefs_dest_file_b                         &
     &         (file_name, id_rank, num_pe,                             &
     &          IO_itp_dest, IO_itp_c_dest, ierr)
!
      use MPI_itp_table_data_IO_b
      use MPI_ascii_data_IO
      use MPI_binary_head_IO
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
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read binary export coefs file: ', trim(file_name)
!
      bbuf_tbl%id_binary = id_read_tbl
      call open_read_mpi_file_b(file_name, num_pe, id_rank, IO_param)
!      if(bbuf_tbl%ierr_bin .ne. 0) goto 99
!      call mpi_read_itp_domain_dest_b                                   &
!     &   (bbuf_tbl, n_rank_file, IO_itp_dest)
!      if(bbuf_tbl%ierr_bin .gt. 0) goto 99
!
!      call mpi_read_itp_table_dest_b(bbuf_tbl, IO_itp_dest)
!      if(bbuf_tbl%ierr_bin .ne. 0) goto 99
!
!      call mpi_read_itp_coefs_dest_b                               &
!     &   (bbuf_tbl, IO_itp_dest, IO_itp_c_dest)
!
  99  continue
      call close_mpi_file(IO_param)
      ierr = bbuf_tbl%ierr_bin
!
      if (n_rank_file .ne. id_rank) ierr = ierr_file
!
      end subroutine mpi_read_itp_coefs_dest_file_b
!
!-----------------------------------------------------------------------
!
      subroutine mpi_read_itp_table_dest_file_b                         &
     &         (file_name, id_rank, num_pe, IO_itp_dest, ierr)
!
      use MPI_itp_table_data_IO_b
      use MPI_ascii_data_IO
      use MPI_binary_head_IO
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
     &   'Read binary interapolate export file: ', trim(file_name)
!
      bbuf_tbl%id_binary = id_read_tbl
      call open_read_mpi_file_b(file_name, num_pe, id_rank, IO_param)
!      if(bbuf_tbl%ierr_bin .ne. 0) goto 99
!      call mpi_read_itp_domain_dest_b                                  &
!     &   (bbuf_tbl, n_rank_file, IO_itp_dest)
!      if(bbuf_tbl%ierr_bin .gt. 0) goto 99
!
!      call mpi_read_itp_table_dest_b(bbuf_tbl, IO_itp_dest)
!
  99  continue
      call close_mpi_file(IO_param)
      ierr = bbuf_tbl%ierr_bin
!
      if (n_rank_file .ne. id_rank) ierr = ierr_file
!
      end subroutine mpi_read_itp_table_dest_file_b
!
!-----------------------------------------------------------------------
!
      subroutine mpi_read_itp_domain_dest_file_b                        &
     &         (file_name, id_rank, num_pe, IO_itp_dest, ierr)
!
      use MPI_itp_table_data_IO_b
      use MPI_ascii_data_IO
      use MPI_binary_head_IO
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
     &   'Read binary export domain file: ', trim(file_name)
!
      bbuf_tbl%id_binary = id_read_tbl
      call open_read_mpi_file_b(file_name, num_pe, id_rank, IO_param)
!      if(bbuf_tbl%ierr_bin .ne. 0) goto 99
!      call mpi_read_itp_domain_dest_b                                  &
!     &   (bbuf_tbl, n_rank_file, IO_itp_dest)
!
  99  continue
      call close_mpi_file(IO_param)
      ierr = bbuf_tbl%ierr_bin
!
      if (n_rank_file .ne. id_rank) ierr = ierr_file
!
      end subroutine mpi_read_itp_domain_dest_file_b
!
!-----------------------------------------------------------------------
!
      end module MPI_itp_table_file_IO_b

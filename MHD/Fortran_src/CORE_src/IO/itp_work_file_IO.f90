!>@file   itp_work_file_IO.f90
!!@brief  module itp_work_file_IO
!!
!!@author H. Matsui
!!@date Programmed in Sep. 2006
!
!>@brief ASCII Interpolation table file IO
!!
!!@verbatim
!!      subroutine write_itp_coefs_dest_file_a                          &
!!     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest)
!!      subroutine write_itp_idx_dest_file_a                            &
!!     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest)
!!        type(interpolate_table_dest), intent(in) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
!!      subroutine read_itp_coefs_dest_file_a                           &
!!     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!!      subroutine read_itp_idx_dest_file_a                             &
!!     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!!      subroutine read_itp_table_dest_file_a                           &
!!     &         (file_name, id_rank, IO_itp_dest, ierr)
!!      subroutine read_itp_domain_dest_file_a                          &
!!     &         (file_name, id_rank, IO_itp_dest, ierr)
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!!@endverbatim
!
      module itp_work_file_IO
!
      use m_precision
      use m_error_IDs
!
      use t_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
!
      implicit none
!
      integer(kind = kint), parameter :: id_tbl_file = 19
      private :: id_tbl_file
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_itp_coefs_dest_file_a                            &
     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest)
!
      use itp_table_dest_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
!
!
      open (id_tbl_file, file = file_name, form = 'formatted')
      call write_interpolate_table_dest                                 &
     &   (id_tbl_file, id_rank, IO_itp_dest)
      call write_interpolate_coefs_dest                                 &
     &   (id_tbl_file, IO_itp_dest, IO_itp_c_dest)
      close(id_tbl_file)
!
      end subroutine write_itp_coefs_dest_file_a
!
!-----------------------------------------------------------------------
!
      subroutine write_itp_idx_dest_file_a                              &
     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest)
!
      use itp_table_dest_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
!
!
      open (id_tbl_file, file = file_name, form = 'formatted')
      call write_interpolate_table_dest                                 &
     &   (id_tbl_file, id_rank, IO_itp_dest)
      call write_interpolate_idx_dest                                   &
     &   (id_tbl_file, IO_itp_dest, IO_itp_c_dest)
      close(id_tbl_file)
!
      end subroutine write_itp_idx_dest_file_a
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine read_itp_coefs_dest_file_a                             &
     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!
      use itp_table_dest_data_IO
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
      open (id_tbl_file, file = file_name, form = 'formatted')
      call read_interpolate_domain_dest                                 &
     &   (id_tbl_file, n_rank_file, IO_itp_dest, ierr)
      if(ierr .gt. 0) go to 99
      call read_interpolate_table_dest                                  &
     &   (id_tbl_file, IO_itp_dest, ierr)
      if(ierr .gt. 0) go to 99
      call read_interpolate_coefs_dest                                  &
     &   (id_tbl_file, IO_itp_dest, IO_itp_c_dest, ierr)
!
  99  continue
      close(id_tbl_file)
      if(ierr .gt. 0) write(*,*) 'Read file error'
!
      ierr = 0
      if (n_rank_file .ne. id_rank) ierr = ierr_file
!
      end subroutine read_itp_coefs_dest_file_a
!
!-----------------------------------------------------------------------
!
      subroutine read_itp_idx_dest_file_a                               &
     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!
      use itp_table_dest_data_IO
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
      open (id_tbl_file, file = file_name, form = 'formatted')
      call read_interpolate_domain_dest                                 &
     &   (id_tbl_file, n_rank_file, IO_itp_dest, ierr)
      if(ierr .gt. 0) go to 99
      call read_interpolate_table_dest                                  &
     &   (id_tbl_file, IO_itp_dest, ierr)
      if(ierr .gt. 0) go to 99
      call read_interpolate_idx_dest                                    &
     &   (id_tbl_file, IO_itp_dest, IO_itp_c_dest, ierr)
!
  99  continue
      close(id_tbl_file)
      if(ierr .gt. 0) write(*,*) 'Read file error'
!
      ierr = 0
      if (n_rank_file .ne. id_rank) ierr = ierr_file
!
      end subroutine read_itp_idx_dest_file_a
!
!-----------------------------------------------------------------------
!
      subroutine read_itp_table_dest_file_a                             &
     &         (file_name, id_rank, IO_itp_dest, ierr)
!
      use itp_table_dest_data_IO
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
      open (id_tbl_file, file = file_name, form = 'formatted')
      call read_interpolate_domain_dest                                 &
     &   (id_tbl_file, n_rank_file, IO_itp_dest, ierr)
      if(ierr .gt. 0) return
      call read_interpolate_table_dest                                  &
     &    (id_tbl_file, IO_itp_dest, ierr)
      if(ierr .gt. 0) return
      close(id_tbl_file)
!
      ierr = 0
      if (n_rank_file .ne. id_rank) ierr = ierr_file
!
      end subroutine read_itp_table_dest_file_a
!
!-----------------------------------------------------------------------
!
      subroutine read_itp_domain_dest_file_a                            &
     &         (file_name, id_rank, IO_itp_dest, ierr)
!
      use itp_table_dest_data_IO
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
      open (id_tbl_file, file = file_name, form = 'formatted')
      call read_interpolate_domain_dest                                 &
     &   (id_tbl_file, n_rank_file, IO_itp_dest, ierr)
      if(ierr .gt. 0) return
      close(id_tbl_file)
!
      ierr = 0
      if (n_rank_file .ne. id_rank) ierr = ierr_file
!
      end subroutine read_itp_domain_dest_file_a
!
!-----------------------------------------------------------------------
!
      end module itp_work_file_IO

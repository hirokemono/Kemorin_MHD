!itp_table_file_IO_b.f90
!      module itp_table_file_IO_b
!
!        programmed by H.Matsui on Sep. 2006 (ver 1.2)
!
!!      subroutine write_itp_table_file_b                               &
!!     &         (file_name, my_rank, IO_itp_org, IO_itp_dest)
!!      subroutine read_itp_table_file_b                                &
!!     &         (file_name, my_rank, IO_itp_org, IO_itp_dest, ierr)
!!        type(interpolate_table_org), intent(inout) :: IO_itp_org
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!
!!      subroutine write_itp_coefs_dest_file_b                          &
!!     &         (file_name, my_rank, IO_itp_dest, IO_itp_c_dest)
!!      subroutine read_itp_coefs_dest_file_b                           &
!!     &         (file_name, my_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!!      subroutine read_itp_table_dest_file_b                           &
!!     &         (file_name, my_rank, IO_itp_dest, ierr)
!!      subroutine read_itp_domain_dest_file_b                          &
!!     &         (file_name, my_rank, IO_itp_dest, ierr)
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
      module itp_table_file_IO_b
!
      use m_precision
      use m_error_IDs
!
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
!
      use itp_table_data_IO_b
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine write_itp_table_file_b                                 &
     &         (file_name, my_rank, IO_itp_org, IO_itp_dest)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind= kint), intent(in) :: my_rank
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
      integer (kind = kint) :: ierr
!
!
      call open_wt_rawfile(file_name, ierr)
      call write_interpolate_table_dest_b(my_rank, IO_itp_dest)
!
      call write_interpolate_table_org_b(my_rank, IO_itp_org)
      call write_interpolate_coefs_org_b(IO_itp_org)
!
      call close_rawfile
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
      end subroutine write_itp_table_file_b
!
!-----------------------------------------------------------------------
!
      subroutine read_itp_table_file_b                                  &
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
      call open_rd_rawfile(file_name, ierr)
      call read_interpolate_domain_dest_b(n_rank_file, IO_itp_dest)
      call read_interpolate_table_dest_b(IO_itp_dest)
!
      call read_interpolate_domain_org_b(n_rank_file, IO_itp_org)
      call read_interpolate_table_org_b(IO_itp_org)
      call read_interpolate_coefs_org_b(IO_itp_org)
!
      call close_rawfile
!
      ierr = 0
      if (n_rank_file .ne. my_rank) ierr = n_rank_file
!
      end subroutine read_itp_table_file_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_itp_coefs_dest_file_b                            &
     &         (file_name, my_rank, IO_itp_dest, IO_itp_c_dest)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind= kint), intent(in) :: my_rank
!
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
      integer (kind = kint) :: ierr
!
!
      call open_wt_rawfile(file_name, ierr)
      call write_interpolate_table_dest_b(my_rank, IO_itp_dest)
      call write_interpolate_coefs_dest_b(IO_itp_dest, IO_itp_c_dest)
      call close_rawfile
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
      call open_rd_rawfile(file_name, ierr)
      call read_interpolate_domain_dest_b(n_rank_file, IO_itp_dest)
      call read_interpolate_table_dest_b(IO_itp_dest)
      call read_interpolate_coefs_dest_b(IO_itp_dest, IO_itp_c_dest)
      call close_rawfile
!
      ierr = 0
      if (n_rank_file .ne. my_rank) ierr = ierr_file
!
      end subroutine read_itp_coefs_dest_file_b
!
!-----------------------------------------------------------------------
!
      subroutine read_itp_table_dest_file_b                             &
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
      call open_rd_rawfile(file_name, ierr)
      call read_interpolate_domain_dest_b(n_rank_file, IO_itp_dest)
      call read_interpolate_table_dest_b(IO_itp_dest)
      call close_rawfile
!
      ierr = 0
      if (n_rank_file .ne. my_rank) ierr = ierr_file
!
      end subroutine read_itp_table_dest_file_b
!
!-----------------------------------------------------------------------
!
      subroutine read_itp_domain_dest_file_b                            &
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
      call open_rd_rawfile(file_name, ierr)
      call read_interpolate_domain_dest_b(n_rank_file, IO_itp_dest)
      call close_rawfile
!
      ierr = 0
      if (n_rank_file .ne. my_rank) ierr = ierr_file
!
      end subroutine read_itp_domain_dest_file_b
!
!-----------------------------------------------------------------------
!
      end module itp_table_file_IO_b

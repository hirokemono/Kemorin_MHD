!>@file   itp_table_file_IO.f90
!!@brief  module itp_table_file_IO
!!
!!@author H. Matsui
!!@date Programmed in Sep. 2006
!
!>@brief ASCII Interpolation table file IO
!!
!!@verbatim
!!      subroutine write_itp_table_file_a                               &
!!     &         (file_name, my_rankt, IO_itp_org, IO_itp_des)
!!      subroutine read_itp_table_file_a                                &
!!     &         (file_name, id_rank, IO_itp_org, IO_itp_dest, ierr)
!!        type(interpolate_table_org), intent(inout) :: IO_itp_org
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!
!!      subroutine write_itp_coefs_dest_file_a                          &
!!     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest)
!!      subroutine read_itp_coefs_dest_file_a                           &
!!     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!!      subroutine read_itp_table_dest_file_a                           &
!!     &         (file_name, id_rank, IO_itp_dest, ierr)
!!      subroutine read_itp_domain_dest_file_a                          &
!!     &         (file_name, id_rank, IO_itp_dest, ierr)
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!!@endverbatim
!
      module itp_table_file_IO
!
      use m_precision
      use m_error_IDs
!
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
!
      use itp_table_data_IO
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
      subroutine write_itp_table_file_a                                 &
     &         (file_name, id_rank, IO_itp_org, IO_itp_dest)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind= kint), intent(in) :: id_rank
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
!
      open (id_tbl_file, file = file_name, form = 'formatted')
      call write_interpolate_table_dest                                 &
     &   (id_tbl_file, id_rank, IO_itp_dest)
!
      call write_interpolate_table_org                                  &
     &   (id_tbl_file, id_rank, IO_itp_org)
      call write_interpolate_coefs_org(id_tbl_file, IO_itp_org)
!
      close(id_tbl_file)
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
      end subroutine write_itp_table_file_a
!
!-----------------------------------------------------------------------
!
      subroutine read_itp_table_file_a                                  &
     &         (file_name, id_rank, IO_itp_org, IO_itp_dest, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank
!
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: n_rank_file
!
!
      open (id_tbl_file, file = file_name, form = 'formatted')
!        write(*,*) 'read_interpolate_domain_dest', trim(file_name)
      call read_interpolate_domain_dest                                 &
     &   (id_tbl_file, n_rank_file, IO_itp_dest)
!        write(*,*) 'read_interpolate_table_dest'
      call read_interpolate_table_dest(id_tbl_file, IO_itp_dest)
!
!        write(*,*) 'read_interpolate_domain_org'
      call read_interpolate_domain_org                                  &
     &   (id_tbl_file, n_rank_file, IO_itp_org)
!        write(*,*) 'read_interpolate_table_org'
      call read_interpolate_table_org(id_tbl_file, IO_itp_org)
!        write(*,*) 'read_interpolate_coefs_org'
      call read_interpolate_coefs_org(id_tbl_file, IO_itp_org)
!
      close(id_tbl_file)
!
      ierr = 0
      if (n_rank_file .ne. id_rank) ierr = n_rank_file
!
      end subroutine read_itp_table_file_a
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine write_itp_coefs_dest_file_a                            &
     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind= kint), intent(in) :: id_rank
!
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
!
      open (id_tbl_file, file = file_name, form = 'formatted')
      call write_interpolate_table_dest                                 &
     &   (id_tbl_file, id_rank, IO_itp_dest)
      call write_interpolate_coefs_dest                                 &
     &   (id_tbl_file, IO_itp_dest, IO_itp_c_dest)
      close(id_tbl_file)
!
      if (IO_itp_dest%num_org_domain .gt. 0) then
        call dealloc_itp_coef_dest(IO_itp_c_dest)
        call dealloc_itp_coef_stack(IO_itp_c_dest)
        call dealloc_itp_table_dest(IO_itp_dest)
        call dealloc_itp_num_dest(IO_itp_dest)
      end if
!
      end subroutine write_itp_coefs_dest_file_a
!
!-----------------------------------------------------------------------
!
      subroutine read_itp_coefs_dest_file_a                             &
     &         (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind= kint), intent(in) :: id_rank
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
     &   (id_tbl_file, n_rank_file, IO_itp_dest)
      call read_interpolate_table_dest(id_tbl_file, IO_itp_dest)
      call read_interpolate_coefs_dest                                  &
     &   (id_tbl_file, IO_itp_dest, IO_itp_c_dest)
      close(id_tbl_file)
!
      ierr = 0
      if (n_rank_file .ne. id_rank) ierr = ierr_file
!
      end subroutine read_itp_coefs_dest_file_a
!
!-----------------------------------------------------------------------
!
      subroutine read_itp_table_dest_file_a                             &
     &         (file_name, id_rank, IO_itp_dest, ierr)
!
      character(len=kchara), intent(in) :: file_name
      integer(kind= kint), intent(in) :: id_rank
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
      integer(kind = kint) :: n_rank_file
!
!
      open (id_tbl_file, file = file_name, form = 'formatted')
      call read_interpolate_domain_dest                                 &
     &   (id_tbl_file, n_rank_file, IO_itp_dest)
      call read_interpolate_table_dest(id_tbl_file, IO_itp_dest)
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
     &   (id_tbl_file, n_rank_file, IO_itp_dest)
      close(id_tbl_file)
!
      ierr = 0
      if (n_rank_file .ne. id_rank) ierr = ierr_file
!
      end subroutine read_itp_domain_dest_file_a
!
!-----------------------------------------------------------------------
!
      end module itp_table_file_IO

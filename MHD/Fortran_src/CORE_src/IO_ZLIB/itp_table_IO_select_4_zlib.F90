!itp_table_IO_select_4_zlib.F90
!      module itp_table_IO_select_4_zlib
!
!        programmed by H.Matsui on Sep. 2006 (ver 1.2)
!
!!      subroutine sel_write_interpolate_table                          &
!!     &         (my_rank, IO_itp_org, IO_itp_dest)
!!      subroutine sel_read_interpolate_table                           &
!!     &         (my_rank, IO_itp_org, IO_itp_dest, ierr)
!!
!!      subroutine sel_write_itp_coefs_dest                             &
!!     &         (my_rank, IO_itp_dest, IO_itp_c_dest)
!!      subroutine sel_read_itp_coefs_dest                              &
!!     &         (my_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!!      subroutine sel_read_itp_table_dest(my_rank, IO_itp_dest, ierr)
!!      subroutine sel_read_itp_domain_dest(my_rank, IO_itp_dest, ierr)
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
      module itp_table_IO_select_4_zlib
!
      use m_precision
!
      use m_file_format_switch
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
!
      use itp_table_file_IO
      use itp_table_file_IO_b
      use gz_itp_table_file_IO
!
      use set_parallel_file_name
!
      implicit none
!
      character(len=kchara), parameter :: work_header = 'work'
      character(len=kchara) :: table_file_header
      character(len=kchara) :: tbl_file_name
      integer(kind = kint) :: ifmt_itp_table_file
!
      private :: tbl_file_name
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_write_interpolate_table                            &
     &         (my_rank, IO_itp_org, IO_itp_dest)
!
      integer(kind= kint), intent(in) :: my_rank
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
!
      tbl_file_name = add_int_suffix(my_rank, table_file_header)
!
#ifdef ZLIB_IO
      if(ifmt_itp_table_file .eq. id_gzip_txt_file_fmt) then
        call gz_write_itp_table_file                                    &
     &     (tbl_file_name, my_rank, IO_itp_org, IO_itp_dest)
        return
      end if
#endif
!
      if (ifmt_itp_table_file .eq. id_binary_file_fmt) then
        call write_itp_table_file_b                                     &
     &     (tbl_file_name, my_rank, IO_itp_org, IO_itp_dest)
      else if(ifmt_itp_table_file .eq. id_ascii_file_fmt) then
        call write_itp_table_file_a                                     &
     &     (tbl_file_name, my_rank, IO_itp_org, IO_itp_dest)
      end if
!
      end subroutine sel_write_interpolate_table
!
!-----------------------------------------------------------------------
!
      subroutine sel_read_interpolate_table                             &
     &         (my_rank, IO_itp_org, IO_itp_dest, ierr)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_org), intent(inout) :: IO_itp_org
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
!
      tbl_file_name = add_int_suffix(my_rank, table_file_header)
!
#ifdef ZLIB_IO
      if(ifmt_itp_table_file .eq. id_gzip_txt_file_fmt) then
        call gz_read_itp_table_file                                     &
     &     (tbl_file_name, my_rank, IO_itp_org, IO_itp_dest, ierr)
        return
      end if
#endif
!
      if (ifmt_itp_table_file .eq. id_binary_file_fmt) then
        call read_itp_table_file_b                                      &
     &     (tbl_file_name, my_rank, IO_itp_org, IO_itp_dest, ierr)
      else if(ifmt_itp_table_file .eq. id_ascii_file_fmt) then
        call read_itp_table_file_a                                      &
     &     (tbl_file_name, my_rank, IO_itp_org, IO_itp_dest, ierr)
      end if
!
      end subroutine sel_read_interpolate_table
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_write_itp_coefs_dest                               &
     &         (my_rank, IO_itp_dest, IO_itp_c_dest)
!
      integer(kind= kint), intent(in) :: my_rank
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
!
      tbl_file_name = add_int_suffix(my_rank, work_header)
!
#ifdef ZLIB_IO
      if(ifmt_itp_table_file .eq. id_gzip_txt_file_fmt) then
        call  gz_write_itp_coefs_dest_file                              &
     &     (tbl_file_name, my_rank, IO_itp_dest, IO_itp_c_dest)
        return
      end if
#endif
!
      if (ifmt_itp_table_file .eq. id_binary_file_fmt) then
        call  write_itp_coefs_dest_file_b                               &
     &     (tbl_file_name, my_rank, IO_itp_dest, IO_itp_c_dest)
      else if(ifmt_itp_table_file .eq. id_ascii_file_fmt) then
        call  write_itp_coefs_dest_file_a                               &
     &     (tbl_file_name, my_rank, IO_itp_dest, IO_itp_c_dest)
      end if
!
      end subroutine sel_write_itp_coefs_dest
!
!-----------------------------------------------------------------------
!
      subroutine sel_read_itp_coefs_dest                                &
     &         (my_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!
      integer(kind= kint), intent(in) :: my_rank
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
!
      tbl_file_name = add_int_suffix(my_rank, work_header)
!
#ifdef ZLIB_IO
      if(ifmt_itp_table_file .eq. id_gzip_txt_file_fmt) then
        call gz_read_itp_coefs_dest_file                                &
     &     (tbl_file_name, my_rank, IO_itp_dest, IO_itp_c_dest, ierr)
        return
      end if
#endif
!
      if (ifmt_itp_table_file .eq. id_binary_file_fmt) then
        call read_itp_coefs_dest_file_b                                 &
     &     (tbl_file_name, my_rank, IO_itp_dest, IO_itp_c_dest, ierr)
      else if(ifmt_itp_table_file .eq. id_ascii_file_fmt) then
        call read_itp_coefs_dest_file_a                                 &
     &     (tbl_file_name, my_rank, IO_itp_dest, IO_itp_c_dest, ierr)
      end if
!
      end subroutine sel_read_itp_coefs_dest
!
!-----------------------------------------------------------------------
!
      subroutine sel_read_itp_table_dest(my_rank, IO_itp_dest, ierr)
!
      integer(kind= kint), intent(in) :: my_rank
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
!
      tbl_file_name = add_int_suffix(my_rank, work_header)
!
#ifdef ZLIB_IO
      if(ifmt_itp_table_file .eq. id_gzip_txt_file_fmt) then
        call gz_read_itp_table_dest_file                                &
     &     (tbl_file_name, my_rank, IO_itp_dest, ierr)
        return
      end if
#endif
!
      if (ifmt_itp_table_file .eq. id_binary_file_fmt) then
        call read_itp_table_dest_file_b                                 &
     &     (tbl_file_name, my_rank, IO_itp_dest, ierr)
      else if(ifmt_itp_table_file .eq. id_ascii_file_fmt) then
        call read_itp_table_dest_file_a                                 &
     &     (tbl_file_name, my_rank, IO_itp_dest, ierr)
      end if
!
      end subroutine sel_read_itp_table_dest
!
!-----------------------------------------------------------------------
!
      subroutine sel_read_itp_domain_dest(my_rank, IO_itp_dest, ierr)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
!
      tbl_file_name =  add_int_suffix(my_rank, work_header)
!
#ifdef ZLIB_IO
      if(ifmt_itp_table_file .eq. id_gzip_txt_file_fmt) then
        call gz_read_itp_domain_dest_file                               &
     &     (tbl_file_name, my_rank, IO_itp_dest, ierr)
        return
      end if
#endif
!
      if (ifmt_itp_table_file .eq. id_binary_file_fmt) then
        call read_itp_domain_dest_file_b                                &
     &     (tbl_file_name, my_rank, IO_itp_dest, ierr)
      else if(ifmt_itp_table_file .eq. id_ascii_file_fmt) then
        call read_itp_domain_dest_file_a                                &
     &     (tbl_file_name, my_rank, IO_itp_dest, ierr)
      end if
!
      end subroutine sel_read_itp_domain_dest
!
!-----------------------------------------------------------------------
!
      end module itp_table_IO_select_4_zlib

!itp_table_IO_select_4_zlib.F90
!      module itp_table_IO_select_4_zlib
!
!        programmed by H.Matsui on Sep. 2006 (ver 1.2)
!
!      subroutine sel_write_interpolate_table(my_rank, ifile_type)
!      subroutine sel_read_interpolate_table(my_rank, ifile_type, ierr)
!      subroutine sel_write_itp_coefs_dest(my_rank, ifile_type)
!      subroutine sel_read_itp_coefs_dest(my_rank, ifile_type, ierr)
!      subroutine sel_read_itp_table_dest(my_rank, ifile_type, ierr)
!      subroutine sel_read_itp_domain_dest(my_rank, ifile_type, ierr)
!
      module itp_table_IO_select_4_zlib
!
      use m_precision
!
      use m_file_format_switch
      use m_interpolate_table_dest_IO
      use m_interpolate_table_org_IO
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
      private :: tbl_file_name
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_write_interpolate_table(my_rank, ifile_type)
!
      integer(kind= kint), intent(in) :: my_rank, ifile_type
!
!
      call add_int_suffix(my_rank, table_file_header, tbl_file_name)
!
#ifdef ZLIB_IO
      if(ifile_type .eq. id_gzip_txt_file_fmt) then
        call gz_write_itp_table_file(tbl_file_name, my_rank)
        return
      end if
#endif
!
      if (ifile_type .eq. id_binary_file_fmt) then
        call write_itp_table_file_b(tbl_file_name, my_rank)
      else if(ifile_type .eq. id_ascii_file_fmt) then
        call write_itp_table_file_a(tbl_file_name, my_rank)
      end if
!
      end subroutine sel_write_interpolate_table
!
!-----------------------------------------------------------------------
!
      subroutine sel_read_interpolate_table(my_rank, ifile_type, ierr)
!
      integer(kind = kint), intent(in) :: my_rank, ifile_type
      integer(kind = kint), intent(inout) :: ierr
!
!
      call add_int_suffix(my_rank, table_file_header, tbl_file_name)
!
#ifdef ZLIB_IO
      if(ifile_type .eq. id_gzip_txt_file_fmt) then
        call gz_read_itp_table_file(tbl_file_name, my_rank, ierr)
        return
      end if
#endif
!
      if (ifile_type .eq. id_binary_file_fmt) then
        call read_itp_table_file_b(tbl_file_name, my_rank, ierr)
      else if(ifile_type .eq. id_ascii_file_fmt) then
        call read_itp_table_file_a(tbl_file_name, my_rank, ierr)
      end if
!
      end subroutine sel_read_interpolate_table
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_write_itp_coefs_dest(my_rank, ifile_type)
!
      integer(kind= kint) :: my_rank, ifile_type
!
!
      call add_int_suffix(my_rank, work_header, tbl_file_name)
!
#ifdef ZLIB_IO
      if(ifile_type .eq. id_gzip_txt_file_fmt) then
        call  gz_write_itp_coefs_dest_file(tbl_file_name, my_rank)
        return
      end if
#endif
!
      if (ifile_type .eq. id_binary_file_fmt) then
        call  write_itp_coefs_dest_file_b(tbl_file_name, my_rank)
      else if(ifile_type .eq. id_ascii_file_fmt) then
        call  write_itp_coefs_dest_file_a(tbl_file_name, my_rank)
      end if
!
      end subroutine sel_write_itp_coefs_dest
!
!-----------------------------------------------------------------------
!
      subroutine sel_read_itp_coefs_dest(my_rank, ifile_type, ierr)
!
      integer(kind= kint), intent(in) :: my_rank, ifile_type
      integer(kind = kint), intent(inout) :: ierr
!
!
      call add_int_suffix(my_rank, work_header, tbl_file_name)
!
#ifdef ZLIB_IO
      if(ifile_type .eq. id_gzip_txt_file_fmt) then
        call gz_read_itp_coefs_dest_file(tbl_file_name, my_rank, ierr)
        return
      end if
#endif
!
      if (ifile_type .eq. id_binary_file_fmt) then
        call read_itp_coefs_dest_file_b(tbl_file_name, my_rank, ierr)
      else if(ifile_type .eq. id_ascii_file_fmt) then
        call read_itp_coefs_dest_file_a(tbl_file_name, my_rank, ierr)
      end if
!
      end subroutine sel_read_itp_coefs_dest
!
!-----------------------------------------------------------------------
!
      subroutine sel_read_itp_table_dest(my_rank, ifile_type, ierr)
!
      integer(kind= kint), intent(in) :: my_rank, ifile_type
      integer(kind = kint), intent(inout) :: ierr
!
!
      call add_int_suffix(my_rank, work_header, tbl_file_name)
!
#ifdef ZLIB_IO
      if(ifile_type .eq. id_gzip_txt_file_fmt) then
        call gz_read_itp_table_dest_file(tbl_file_name, my_rank, ierr)
        return
      end if
#endif
!
      if (ifile_type .eq. id_binary_file_fmt) then
        call read_itp_table_dest_file_b(tbl_file_name, my_rank, ierr)
      else if(ifile_type .eq. id_ascii_file_fmt) then
        call read_itp_table_dest_file_a(tbl_file_name, my_rank, ierr)
      end if
!
      end subroutine sel_read_itp_table_dest
!
!-----------------------------------------------------------------------
!
      subroutine sel_read_itp_domain_dest(my_rank, ifile_type, ierr)
!
      integer(kind = kint), intent(in) :: my_rank, ifile_type
      integer(kind = kint), intent(inout) :: ierr
!
!
      call add_int_suffix(my_rank, work_header, tbl_file_name)
!
#ifdef ZLIB_IO
      if(ifile_type .eq. id_gzip_txt_file_fmt) then
        call gz_read_itp_domain_dest_file(tbl_file_name, my_rank, ierr)
        return
      end if
#endif
!
      if (ifile_type .eq. id_binary_file_fmt) then
        call read_itp_domain_dest_file_b(tbl_file_name, my_rank, ierr)
      else if(ifile_type .eq. id_ascii_file_fmt) then
        call read_itp_domain_dest_file_a(tbl_file_name, my_rank, ierr)
      end if
!
      end subroutine sel_read_itp_domain_dest
!
!-----------------------------------------------------------------------
!
      end module itp_table_IO_select_4_zlib

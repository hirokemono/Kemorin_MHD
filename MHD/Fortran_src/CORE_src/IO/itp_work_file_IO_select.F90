!>@file   itp_work_file_IO_select.F90
!!@brief  module itp_work_file_IO_select
!!
!!@author H. Matsui
!!@date Programmed in Sep. 2006 (ver 1.2)
!!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine sel_read_itp_coefs_dest(id_rank, table_file_IO,      &
!!     &          IO_itp_dest, IO_itp_c_dest, ierr)
!!      subroutine sel_read_itp_index_dest(id_rank, table_file_IO,      &
!!     &          IO_itp_dest, IO_itp_c_dest, ierr)
!!        type(field_IO_params), intent(in) ::  table_file_IO
!!      subroutine sel_read_itp_table_dest                              &
!!     &         (id_rank, table_file_IO, IO_itp_dest, ierr)
!!      subroutine sel_read_itp_domain_dest                             &
!!     &         (id_rank, table_file_IO, IO_itp_dest, ierr)
!!      subroutine sel_write_itp_coefs_dest                             &
!!     &         (id_rank, table_file_IO, IO_itp_dest, IO_itp_c_dest)
!!      subroutine sel_write_itp_index_dest                             &
!!     &         (id_rank, table_file_IO, IO_itp_dest, IO_itp_c_dest)
!!        type(field_IO_params), intent(in) ::  table_file_IO
!!        type(interpolate_table_dest), intent(in) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
!!      subroutine dealloc_itp_dest_after_write                         &
!!     &         (IO_itp_dest, IO_itp_c_dest)
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!!@endverbatim
!
      module itp_work_file_IO_select
!
      use m_precision
!
      use m_file_format_switch
      use t_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
      use t_file_IO_parameter
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_read_itp_coefs_dest(id_rank, table_file_IO,        &
     &          IO_itp_dest, IO_itp_c_dest, ierr)
!
      use set_interpolate_file_name
      use itp_work_file_IO
      use itp_work_file_IO_b
      use gz_itp_work_file_IO
      use gz_itp_work_file_IO_b
!
      use itp_work_file_IO
      use itp_work_file_IO_b
      use gz_itp_work_file_IO
      use gz_itp_work_file_IO_b
!
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
      character(len=kchara) :: file_name
!
!
      file_name = set_interpolate_work_name(id_rank, table_file_IO)
!
      if (table_file_IO%iflag_format .eq. id_binary_file_fmt) then
        call read_itp_coefs_dest_file_b                                 &
     &     (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format.eq.id_gzip_txt_file_fmt) then
        call gz_read_itp_coefs_dest_file                                &
     &     (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
      else if(table_file_IO%iflag_format.eq.id_gzip_bin_file_fmt) then
        call read_gz_itp_coefs_dest_file_b                              &
     &     (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
#endif
!
      else if(table_file_IO%iflag_format .eq. id_ascii_file_fmt) then
        call read_itp_coefs_dest_file_a                                 &
     &     (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
      end if
!
      end subroutine sel_read_itp_coefs_dest
!
!-----------------------------------------------------------------------
!
      subroutine sel_read_itp_index_dest(id_rank, table_file_IO,        &
     &          IO_itp_dest, IO_itp_c_dest, ierr)
!
      use set_interpolate_file_name
      use itp_work_file_IO
      use itp_work_file_IO_b
      use gz_itp_work_file_IO
      use gz_itp_work_file_IO_b
!
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
      character(len=kchara) :: file_name
!
!
      file_name = set_interpolate_work_name(id_rank, table_file_IO)
!
      if (table_file_IO%iflag_format .eq. id_binary_file_fmt) then
        call read_itp_idx_dest_file_b                                   &
     &     (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format.eq.id_gzip_txt_file_fmt) then
        call gz_read_itp_coefs_dest_file                                &
     &     (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
      else if(table_file_IO%iflag_format.eq.id_gzip_bin_file_fmt) then
        call read_gz_itp_idx_dest_file_b                                &
     &     (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
#endif
!
      else if(table_file_IO%iflag_format .eq. id_ascii_file_fmt) then
        call read_itp_idx_dest_file_a                                   &
     &     (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
      end if
!
      end subroutine sel_read_itp_index_dest
!
!-----------------------------------------------------------------------
!
      subroutine sel_read_itp_table_dest                                &
     &         (id_rank, table_file_IO, IO_itp_dest, ierr)
!
      use set_interpolate_file_name
      use itp_work_file_IO
      use itp_work_file_IO_b
      use gz_itp_work_file_IO
      use gz_itp_work_file_IO_b
!
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
      character(len=kchara) :: file_name
!
!
      file_name = set_interpolate_work_name(id_rank, table_file_IO)
!
      if (table_file_IO%iflag_format .eq. id_binary_file_fmt) then
        call read_itp_table_dest_file_b                                 &
     &     (file_name, id_rank, IO_itp_dest, ierr)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format.eq.id_gzip_txt_file_fmt) then
        call gz_read_itp_table_dest_file                                &
     &     (file_name, id_rank, IO_itp_dest, ierr)
      else if(table_file_IO%iflag_format.eq.id_gzip_bin_file_fmt) then
        call read_gz_itp_table_dest_file_b                              &
     &     (file_name, id_rank, IO_itp_dest, ierr)
#endif
!
      else if(table_file_IO%iflag_format .eq. id_ascii_file_fmt) then
        call read_itp_table_dest_file_a                                 &
     &     (file_name, id_rank, IO_itp_dest, ierr)
      end if
!
      end subroutine sel_read_itp_table_dest
!
!-----------------------------------------------------------------------
!
      subroutine sel_read_itp_domain_dest                               &
     &         (id_rank, table_file_IO, IO_itp_dest, ierr)
!
      use set_interpolate_file_name
      use itp_work_file_IO
      use itp_work_file_IO_b
      use gz_itp_work_file_IO
      use gz_itp_work_file_IO_b
!
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
      character(len=kchara) :: file_name
!
!
      file_name = set_interpolate_work_name(id_rank, table_file_IO)
!
      if (table_file_IO%iflag_format .eq. id_binary_file_fmt) then
        call read_itp_domain_dest_file_b                                &
     &     (file_name, id_rank, IO_itp_dest, ierr)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format.eq.id_gzip_txt_file_fmt) then
        call gz_read_itp_domain_dest_file                               &
     &     (file_name, id_rank, IO_itp_dest, ierr)
      else if(table_file_IO%iflag_format.eq.id_gzip_bin_file_fmt) then
        call read_gz_itp_domain_dest_file_b                             &
     &     (file_name, id_rank, IO_itp_dest, ierr)
#endif
!
      else if(table_file_IO%iflag_format .eq. id_ascii_file_fmt) then
        call read_itp_domain_dest_file_a                                &
     &     (file_name, id_rank, IO_itp_dest, ierr)
      end if
!
      end subroutine sel_read_itp_domain_dest
!
!-----------------------------------------------------------------------
!
      subroutine sel_write_itp_coefs_dest                               &
     &         (id_rank, table_file_IO, IO_itp_dest, IO_itp_c_dest)
!
      use set_interpolate_file_name
      use itp_work_file_IO
      use itp_work_file_IO_b
      use gz_itp_work_file_IO
      use gz_itp_work_file_IO_b
!
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
!
      character(len=kchara) :: file_name
      integer(kind = kint) :: ierr = 0
!
!
      file_name = set_interpolate_work_name(id_rank, table_file_IO)
!
      if (table_file_IO%iflag_format .eq. id_binary_file_fmt) then
        call  write_itp_coefs_dest_file_b                               &
     &     (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format.eq.id_gzip_txt_file_fmt) then
        call  gz_write_itp_coefs_dest_file                              &
     &     (file_name, id_rank, IO_itp_dest, IO_itp_c_dest)
      else if(table_file_IO%iflag_format.eq.id_gzip_bin_file_fmt) then
        call  write_gz_itp_coefs_dest_file_b                            &
     &     (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
#endif
!
      else if(table_file_IO%iflag_format .eq. id_ascii_file_fmt) then
        call  write_itp_coefs_dest_file_a                               &
     &     (file_name, id_rank, IO_itp_dest, IO_itp_c_dest)
      end if
!
      end subroutine sel_write_itp_coefs_dest
!
!-----------------------------------------------------------------------
!
      subroutine sel_write_itp_index_dest                               &
     &         (id_rank, table_file_IO, IO_itp_dest, IO_itp_c_dest)
!
      use set_interpolate_file_name
      use itp_work_file_IO
      use itp_work_file_IO_b
      use gz_itp_work_file_IO
      use gz_itp_work_file_IO_b
!
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
      type(interpolate_table_dest), intent(in) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(in) :: IO_itp_c_dest
!
      character(len=kchara) :: file_name
      integer(kind = kint) :: ierr = 0
!
!
      file_name = set_interpolate_work_name(id_rank, table_file_IO)
!
      if (table_file_IO%iflag_format .eq. id_binary_file_fmt) then
        call  write_itp_idx_dest_file_b                                 &
     &     (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format.eq.id_gzip_txt_file_fmt) then
        call  gz_write_itp_coefs_dest_file                              &
     &     (file_name, id_rank, IO_itp_dest, IO_itp_c_dest)
      else if(table_file_IO%iflag_format.eq.id_gzip_bin_file_fmt) then
        call  write_gz_itp_idx_dest_file_b                              &
     &     (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
#endif
!
      else if(table_file_IO%iflag_format .eq. id_ascii_file_fmt) then
        call  write_itp_idx_dest_file_a                                 &
     &     (file_name, id_rank, IO_itp_dest, IO_itp_c_dest)
      end if
!
      end subroutine sel_write_itp_index_dest
!
!-----------------------------------------------------------------------
!
      subroutine dealloc_itp_dest_after_write                           &
     &         (IO_itp_dest, IO_itp_c_dest)
!
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
      if(IO_itp_dest%num_org_domain .gt. 0) then
        call dealloc_itp_coef_dest(IO_itp_c_dest)
        call dealloc_itp_coef_stack(IO_itp_c_dest)
      end if
      call dealloc_itp_table_dest(IO_itp_dest)
      call dealloc_itp_num_dest(IO_itp_dest)
!
      end subroutine dealloc_itp_dest_after_write
!
!-----------------------------------------------------------------------
!
      end module itp_work_file_IO_select

!>@file   itp_table_IO_select_4_zlib.f90
!!@brief  module itp_table_IO_select_4_zlib
!!
!!@author H. Matsui
!!@date Programmed in Sep. 2006 (ver 1.2)
!!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine sel_write_interpolate_table                          &
!!     &         (id_rank, table_file_IO, itp_tbl_IO)
!!      subroutine sel_read_interpolate_table                           &
!!     &         (id_rank, table_file_IO, itp_tbl_IO, ierr)
!!        type(field_IO_params), intent(in) ::  table_file_IO
!!        type(interpolate_table), intent(inout) :: itp_tbl_IO
!!
!!      subroutine sel_write_itp_coefs_dest                             &
!!     &         (id_rank, table_file_IO, IO_itp_dest, IO_itp_c_dest)
!!      subroutine sel_read_itp_coefs_dest(id_rank, table_file_IO,      &
!!     &          IO_itp_dest, IO_itp_c_dest, ierr)
!!        type(field_IO_params), intent(in) ::  table_file_IO
!!      subroutine sel_read_itp_table_dest                              &
!!     &         (id_rank, table_file_IO, IO_itp_dest, ierr)
!!      subroutine sel_read_itp_domain_dest                             &
!!     &         (id_rank, table_file_IO, IO_itp_dest, ierr)
!!        type(field_IO_params), intent(in) ::  table_file_IO
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!@endverbatim
!
      module itp_table_IO_select_4_zlib
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
      use itp_table_file_IO
      use itp_table_file_IO_b
      use gz_itp_table_file_IO
      use gz_itp_table_file_IO_b
!
      use set_parallel_file_name
!
      implicit none
!
      character(len=kchara), parameter :: work_header = 'work'
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_write_interpolate_table                            &
     &         (id_rank, table_file_IO, itp_tbl_IO)
!
      use set_mesh_extensions
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
      type(interpolate_table), intent(inout) :: itp_tbl_IO
!
      character(len=kchara) :: fname_tmp, file_name
      integer(kind = kint) :: ierr = 0
!
!
      fname_tmp = add_process_id(id_rank, table_file_IO%file_prefix)
      if(     (table_file_IO%iflag_format .eq. id_binary_file_fmt)      &
     &   .or. (table_file_IO%iflag_format .eq. id_gzip_bin_file_fmt)    &
     &  ) then
        file_name =  add_itb_extension(fname_tmp)
      else
        file_name =  add_itb_extension(fname_tmp)
      end if
!
      if (table_file_IO%iflag_format .eq. id_binary_file_fmt) then
        call write_itp_table_file_b                                     &
     &     (file_name, id_rank, itp_tbl_IO, ierr)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format.eq.id_gzip_txt_file_fmt) then
        call gz_write_itp_table_file(file_name, id_rank, itp_tbl_IO)
      else if(table_file_IO%iflag_format.eq.id_gzip_bin_file_fmt) then
        call write_gz_itp_table_file_b                                  &
     &     (file_name, id_rank, itp_tbl_IO, ierr)
#endif
!
      else if(table_file_IO%iflag_format .eq. id_ascii_file_fmt) then
        call write_itp_table_file_a                                     &
     &     (file_name, id_rank, itp_tbl_IO)
      end if
!
      end subroutine sel_write_interpolate_table
!
!-----------------------------------------------------------------------
!
      subroutine sel_read_interpolate_table                             &
     &         (id_rank, table_file_IO, itp_tbl_IO, ierr)
!
      use set_mesh_extensions
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table), intent(inout) :: itp_tbl_IO
!
      character(len=kchara) :: fname_tmp, file_name
!
!
      fname_tmp = add_process_id(id_rank, table_file_IO%file_prefix)
      if(     (table_file_IO%iflag_format .eq. id_binary_file_fmt)      &
     &   .or. (table_file_IO%iflag_format .eq. id_gzip_bin_file_fmt)    &
     &  ) then
        file_name =  add_itb_extension(fname_tmp)
      else
        file_name =  add_itb_extension(fname_tmp)
      end if
!
      if (table_file_IO%iflag_format .eq. id_binary_file_fmt) then
        call read_itp_table_file_b                                      &
     &     (file_name, id_rank, itp_tbl_IO, ierr)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format.eq.id_gzip_txt_file_fmt) then
        call gz_read_itp_table_file                                     &
     &     (file_name, id_rank, itp_tbl_IO, ierr)
      else if(table_file_IO%iflag_format.eq.id_gzip_bin_file_fmt) then
        call read_gz_itp_table_file_b                                   &
     &     (file_name, id_rank, itp_tbl_IO, ierr)
#endif
!
      else if(table_file_IO%iflag_format .eq. id_ascii_file_fmt) then
        call read_itp_table_file_a                                      &
     &     (file_name, id_rank, itp_tbl_IO, ierr)
      end if
!
      end subroutine sel_read_interpolate_table
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_write_itp_coefs_dest                               &
     &         (id_rank, table_file_IO, IO_itp_dest, IO_itp_c_dest)
!
      use set_mesh_extensions
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
      character(len=kchara) :: fname_tmp, file_name
      integer(kind = kint) :: ierr = 0
!
      fname_tmp = add_process_id(id_rank, work_header)
      if(     (table_file_IO%iflag_format .eq. id_binary_file_fmt)      &
     &   .or. (table_file_IO%iflag_format .eq. id_gzip_bin_file_fmt)    &
     &  ) then
        file_name =  add_itb_extension(fname_tmp)
      else
        file_name =  add_itp_extension(fname_tmp)
      end if
!
      if (table_file_IO%iflag_format .eq. id_binary_file_fmt) then
        call  write_itp_coefs_dest_file_b                               &
     &     (file_name, id_rank, IO_itp_dest, IO_itp_c_dest, ierr)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format.eq.id_gzip_txt_file_fmt) then
        call  gz_write_itp_coefs_dest_file                              &
     &     (file_name, id_rank, IO_itp_dest, IO_itp_c_dest)
      else if(table_file_IO%iflag_format.eq.id_gzip_txt_file_fmt) then
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
      subroutine sel_read_itp_coefs_dest(id_rank, table_file_IO,        &
     &          IO_itp_dest, IO_itp_c_dest, ierr)
!
      use set_mesh_extensions
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
      character(len=kchara) :: fname_tmp, file_name
!
!
      fname_tmp = add_process_id(id_rank, work_header)
      if(     (table_file_IO%iflag_format .eq. id_binary_file_fmt)      &
     &   .or. (table_file_IO%iflag_format .eq. id_gzip_bin_file_fmt)    &
     &  ) then
        file_name =  add_itb_extension(fname_tmp)
      else
        file_name =  add_itb_extension(fname_tmp)
      end if
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
      subroutine sel_read_itp_table_dest                                &
     &         (id_rank, table_file_IO, IO_itp_dest, ierr)
!
      use set_mesh_extensions
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
      character(len=kchara) :: fname_tmp, file_name
!
!
      fname_tmp = add_process_id(id_rank, work_header)
      if(     (table_file_IO%iflag_format .eq. id_binary_file_fmt)      &
     &   .or. (table_file_IO%iflag_format .eq. id_gzip_bin_file_fmt)    &
     &  ) then
        file_name =  add_itb_extension(fname_tmp)
      else
        file_name =  add_itb_extension(fname_tmp)
      end if
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
      use set_mesh_extensions
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
      character(len=kchara) :: fname_tmp, file_name
!
!
      fname_tmp =  add_process_id(id_rank, work_header)
      if(     (table_file_IO%iflag_format .eq. id_binary_file_fmt)      &
     &   .or. (table_file_IO%iflag_format .eq. id_gzip_bin_file_fmt)    &
     &  ) then
        file_name =  add_itb_extension(fname_tmp)
      else
        file_name =  add_itb_extension(fname_tmp)
      end if
!
      if (table_file_IO%iflag_format .eq. id_binary_file_fmt) then
        call read_itp_domain_dest_file_b                                &
     &     (file_name, id_rank, IO_itp_dest, ierr)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format.eq.id_gzip_txt_file_fmt) then
        call gz_read_itp_domain_dest_file                               &
     &     (file_name, id_rank, IO_itp_dest, ierr)
      else if(table_file_IO%iflag_format.eq.id_gzip_txt_file_fmt) then
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
      end module itp_table_IO_select_4_zlib

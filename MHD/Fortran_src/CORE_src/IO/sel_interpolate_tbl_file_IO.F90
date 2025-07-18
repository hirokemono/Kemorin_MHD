!>@file   sel_interpolate_tbl_file_IO.f90
!!@brief  module sel_interpolate_tbl_file_IO
!!
!!@author H. Matsui
!!@date Programmed in Sep. 2006 (ver 1.2)
!!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine sel_write_itrplte_coef_tbl(id_rank, table_file_IO,   &
!!     &          itp_tbl_org_IO, itp_tbl_dest_IO)
!!      subroutine sel_write_itrplte_idx_tbl(id_rank, table_file_IO,    &
!!     &          itp_tbl_org_IO, itp_tbl_dest_IO)
!!        integer, intent(in) :: id_rank
!!        type(field_IO_params), intent(in) ::  table_file_IO
!!        type(interpolate_table_org), intent(in) :: itp_tbl_org_IO
!!        type(interpolate_table_dest), intent(in) :: itp_tbl_dest_IO
!!
!!      subroutine sel_read_itrplte_coef_tbl(id_rank, table_file_IO,    &
!!     &          itp_tbl_org_IO, itp_tbl_dest_IO, ierr)
!!      subroutine sel_read_itrplte_idx_tbl(id_rank, table_file_IO,     &
!!     &          itp_tbl_org_IO, itp_tbl_dest_IO, ierr)
!!        integer, intent(in) :: id_rank
!!        type(field_IO_params), intent(in) ::  table_file_IO
!!        type(interpolate_table_org), intent(inout) :: itp_tbl_org_IO
!!        type(interpolate_table_dest), intent(inout) :: itp_tbl_dest_IO
!!        integer(kind = kint), intent(inout) :: ierr
!!
!!      subroutine dealloc_itp_tbl_after_write(itp_tbl_org_IO,          &
!!     &                                       itp_tbl_dest_IO)
!!        type(interpolate_table_org), intent(inout) :: itp_tbl_org_IO
!!        type(interpolate_table_dest), intent(inout) :: itp_tbl_dest_IO
!!@endverbatim
!
      module sel_interpolate_tbl_file_IO
!
      use m_precision
!
      use m_file_format_switch
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
      subroutine sel_write_itrplte_coef_tbl(id_rank, table_file_IO,     &
     &          itp_tbl_org_IO, itp_tbl_dest_IO)
!
      use set_interpolate_file_name
      use itrplte_tbl_file_IO
      use itrplte_tbl_file_IO_b
      use gz_itrplte_tbl_file_IO
      use gz_itrplte_table_file_IO_b
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
      type(interpolate_table_org), intent(in) :: itp_tbl_org_IO
      type(interpolate_table_dest), intent(in) :: itp_tbl_dest_IO
!
      character(len=kchara) :: file_name
      integer(kind = kint) :: ierr = 0
!
!
      file_name = s_set_interpolate_file_name(id_rank, table_file_IO)
!
      if (table_file_IO%iflag_format .eq. id_binary_file_fmt) then
        call write_itp_table_coef_file_b(file_name, id_rank,            &
     &      itp_tbl_org_IO, itp_tbl_dest_IO, ierr)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format.eq.id_gzip_txt_file_fmt) then
        call gz_write_itp_table_coef_file_a(file_name, id_rank,         &
     &      itp_tbl_org_IO, itp_tbl_dest_IO)
      else if(table_file_IO%iflag_format.eq.id_gzip_bin_file_fmt) then
        call gz_write_itp_table_coef_file_b(file_name, id_rank,         &
     &      itp_tbl_org_IO, itp_tbl_dest_IO, ierr)
#endif
!
      else if(table_file_IO%iflag_format .eq. id_ascii_file_fmt) then
        call write_itp_table_coef_file_a(file_name, id_rank,            &
     &      itp_tbl_org_IO, itp_tbl_dest_IO)
      end if
!
      end subroutine sel_write_itrplte_coef_tbl
!
!-----------------------------------------------------------------------
!
      subroutine sel_write_itrplte_idx_tbl(id_rank, table_file_IO,      &
     &          itp_tbl_org_IO, itp_tbl_dest_IO)
!
      use set_interpolate_file_name
      use itrplte_tbl_file_IO
      use itrplte_tbl_file_IO_b
      use gz_itrplte_tbl_file_IO
      use gz_itrplte_table_file_IO_b
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
      type(interpolate_table_org), intent(in) :: itp_tbl_org_IO
      type(interpolate_table_dest), intent(in) :: itp_tbl_dest_IO
!
      character(len=kchara) :: file_name
      integer(kind = kint) :: ierr = 0
!
!
      file_name = s_set_interpolate_file_name(id_rank, table_file_IO)
!
      if (table_file_IO%iflag_format .eq. id_binary_file_fmt) then
        call write_itp_table_idx_file_b(file_name, id_rank,             &
     &      itp_tbl_org_IO, itp_tbl_dest_IO, ierr)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format.eq.id_gzip_txt_file_fmt) then
        call gz_write_itp_table_idx_file_a(file_name, id_rank,          &
     &      itp_tbl_org_IO, itp_tbl_dest_IO)
      else if(table_file_IO%iflag_format.eq.id_gzip_bin_file_fmt) then
        call gz_write_itp_table_idx_file_b(file_name, id_rank,          &
     &      itp_tbl_org_IO, itp_tbl_dest_IO, ierr)
#endif
!
      else if(table_file_IO%iflag_format .eq. id_ascii_file_fmt) then
        call write_itp_table_idx_file_a(file_name, id_rank,             &
     &      itp_tbl_org_IO, itp_tbl_dest_IO)
      end if
!
      end subroutine sel_write_itrplte_idx_tbl
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_read_itrplte_coef_tbl(id_rank, table_file_IO,      &
     &          itp_tbl_org_IO, itp_tbl_dest_IO, ierr)
!
      use set_interpolate_file_name
      use itrplte_tbl_file_IO
      use itrplte_tbl_file_IO_b
      use gz_itrplte_tbl_file_IO
      use gz_itrplte_table_file_IO_b
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
!
      type(interpolate_table_org), intent(inout) :: itp_tbl_org_IO
      type(interpolate_table_dest), intent(inout) :: itp_tbl_dest_IO
      integer(kind = kint), intent(inout) :: ierr
!
      character(len=kchara) :: file_name
!
!
      file_name = s_set_interpolate_file_name(id_rank, table_file_IO)
!
      if (table_file_IO%iflag_format .eq. id_binary_file_fmt) then
        call read_itp_table_coef_file_b(file_name, id_rank,             &
     &      itp_tbl_org_IO, itp_tbl_dest_IO, ierr)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format.eq.id_gzip_txt_file_fmt) then
        call gz_read_itp_table_coef_file_a(file_name, id_rank,          &
     &      itp_tbl_org_IO, itp_tbl_dest_IO, ierr)
      else if(table_file_IO%iflag_format.eq.id_gzip_bin_file_fmt) then
        call gz_read_itp_table_coef_file_b(file_name, id_rank,          &
     &      itp_tbl_org_IO, itp_tbl_dest_IO, ierr)
#endif
!
      else if(table_file_IO%iflag_format .eq. id_ascii_file_fmt) then
        call read_itp_table_coef_file_a(file_name, id_rank,             &
     &      itp_tbl_org_IO, itp_tbl_dest_IO, ierr)
      end if
!
      end subroutine sel_read_itrplte_coef_tbl
!
!-----------------------------------------------------------------------
!
      subroutine sel_read_itrplte_idx_tbl(id_rank, table_file_IO,       &
     &          itp_tbl_org_IO, itp_tbl_dest_IO, ierr)
!
      use set_interpolate_file_name
      use itrplte_tbl_file_IO
      use itrplte_tbl_file_IO_b
      use gz_itrplte_tbl_file_IO
      use gz_itrplte_table_file_IO_b
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
!
      type(interpolate_table_org), intent(inout) :: itp_tbl_org_IO
      type(interpolate_table_dest), intent(inout) :: itp_tbl_dest_IO
      integer(kind = kint), intent(inout) :: ierr
!
      character(len=kchara) :: file_name
!
!
      file_name = s_set_interpolate_file_name(id_rank, table_file_IO)
!
      if (table_file_IO%iflag_format .eq. id_binary_file_fmt) then
        call read_itp_table_idx_file_b(file_name, id_rank,              &
     &      itp_tbl_org_IO, itp_tbl_dest_IO, ierr)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format.eq.id_gzip_txt_file_fmt) then
        call gz_read_itp_table_idx_file_a(file_name, id_rank,           &
     &      itp_tbl_org_IO, itp_tbl_dest_IO, ierr)
      else if(table_file_IO%iflag_format.eq.id_gzip_bin_file_fmt) then
        call gz_read_itp_table_idx_file_b(file_name, id_rank,           &
     &      itp_tbl_org_IO, itp_tbl_dest_IO, ierr)
#endif
!
      else if(table_file_IO%iflag_format .eq. id_ascii_file_fmt) then
        call read_itp_table_idx_file_a(file_name, id_rank,              &
     &      itp_tbl_org_IO, itp_tbl_dest_IO, ierr)
      end if
!
      end subroutine sel_read_itrplte_idx_tbl
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine dealloc_itp_tbl_after_write(itp_tbl_org_IO,            &
     &                                       itp_tbl_dest_IO)
!
      type(interpolate_table_org), intent(inout) :: itp_tbl_org_IO
      type(interpolate_table_dest), intent(inout) :: itp_tbl_dest_IO
!
!
      if (itp_tbl_org_IO%num_dest_domain .gt. 0) then
        call dealloc_itp_table_org(itp_tbl_org_IO)
      end if
      call dealloc_itp_num_org(itp_tbl_org_IO)
!
      call dealloc_itp_table_dest(itp_tbl_dest_IO)
      call dealloc_itp_num_dest(itp_tbl_dest_IO)
!
      end subroutine dealloc_itp_tbl_after_write
!
!-----------------------------------------------------------------------
!
      end module sel_interpolate_tbl_file_IO

!>@file   itrplte_tbl_idx_IO_select.f90
!!@brief  module itrplte_tbl_idx_IO_select
!!
!!@author H. Matsui
!!@date Programmed in Sep. 2006 (ver 1.2)
!!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine sel_read_itrplte_idx_tbl(id_rank, table_file_IO,     &
!!     &          itp_tbl_IO, ierr)
!!        type(field_IO_params), intent(in) ::  table_file_IO
!!        type(interpolate_table), intent(inout) :: itp_tbl_IO
!!      subroutine sel_read_dbl_itrplte_idx_tbl(id_rank, table_file_IO, &
!!     &          itp_tbl1_IO, itp_tbl2_IO, ierr)
!!        type(field_IO_params), intent(in) ::  table_file_IO
!!        type(interpolate_table), intent(inout) :: itp_tbl1_IO
!!        type(interpolate_table), intent(inout) :: itp_tbl2_IO
!!
!!      subroutine sel_write_itrplte_idx_tbl                            &
!!     &         (id_rank, table_file_IO, itp_tbl_IO)
!!        type(field_IO_params), intent(in) ::  table_file_IO
!!        type(interpolate_table), intent(in) :: itp_tbl_IO
!!      subroutine sel_write_dbl_itrplte_idx_tbl                        &
!!     &         (id_rank, table_file_IO, itp_tbl1_IO, itp_tbl2_IO)
!!        type(field_IO_params), intent(in) ::  table_file_IO
!!        type(interpolate_table), intent(in) :: itp_tbl1_IO
!!        type(interpolate_table), intent(in) :: itp_tbl2_IO
!!@endverbatim
!
      module itrplte_tbl_idx_IO_select
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
      subroutine sel_read_itrplte_idx_tbl(id_rank, table_file_IO,       &
     &          itp_tbl_IO, ierr)
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
      type(interpolate_table), intent(inout) :: itp_tbl_IO
      integer(kind = kint), intent(inout) :: ierr
!
      character(len=kchara) :: file_name
!
!
      file_name = s_set_interpolate_file_name(id_rank, table_file_IO)
!
      if (table_file_IO%iflag_format .eq. id_binary_file_fmt) then
        call read_itp_table_idx_file_b                                  &
     &     (file_name, id_rank, itp_tbl_IO, ierr)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format.eq.id_gzip_txt_file_fmt) then
        call gz_read_itp_table_idx_file_a                               &
     &     (file_name, id_rank, itp_tbl_IO, ierr)
      else if(table_file_IO%iflag_format.eq.id_gzip_bin_file_fmt) then
        call gz_read_itp_table_idx_file_b                               &
     &     (file_name, id_rank, itp_tbl_IO, ierr)
#endif
!
      else if(table_file_IO%iflag_format .eq. id_ascii_file_fmt) then
        call read_itp_table_idx_file_a                                  &
     &     (file_name, id_rank, itp_tbl_IO, ierr)
      end if
!
      end subroutine sel_read_itrplte_idx_tbl
!
!-----------------------------------------------------------------------
!
      subroutine sel_read_dbl_itrplte_idx_tbl(id_rank, table_file_IO,   &
     &          itp_tbl1_IO, itp_tbl2_IO, ierr)
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
      type(interpolate_table), intent(inout) :: itp_tbl1_IO
      type(interpolate_table), intent(inout) :: itp_tbl2_IO
      integer(kind = kint), intent(inout) :: ierr
!
      character(len=kchara) :: file_name
!
!
      file_name = s_set_interpolate_file_name(id_rank, table_file_IO)
!
      if (table_file_IO%iflag_format .eq. id_binary_file_fmt) then
        call read_dbl_itp_tbl_idx_file_b                                &
     &     (file_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format.eq.id_gzip_txt_file_fmt) then
        call gz_rd_dbl_itp_tbl_idx_file_a                               &
     &     (file_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
      else if(table_file_IO%iflag_format.eq.id_gzip_bin_file_fmt) then
        call gz_rd_dbl_itp_tbl_idx_file_b                               &
     &     (file_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
#endif
!
      else if(table_file_IO%iflag_format .eq. id_ascii_file_fmt) then
        call read_dbl_itp_tbl_idx_file_a                                &
     &     (file_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
      end if
!
      end subroutine sel_read_dbl_itrplte_idx_tbl
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_write_itrplte_idx_tbl                              &
     &         (id_rank, table_file_IO, itp_tbl_IO)
!
      use set_interpolate_file_name
      use itrplte_tbl_file_IO
      use itrplte_tbl_file_IO_b
      use gz_itrplte_tbl_file_IO
      use gz_itrplte_table_file_IO_b
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
      type(interpolate_table), intent(in) :: itp_tbl_IO
!
      character(len=kchara) :: file_name
      integer(kind = kint) :: ierr = 0
!
!
      file_name = s_set_interpolate_file_name(id_rank, table_file_IO)
!
      if (table_file_IO%iflag_format .eq. id_binary_file_fmt) then
        call write_itp_table_idx_file_b                                 &
     &     (file_name, id_rank, itp_tbl_IO, ierr)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format.eq.id_gzip_txt_file_fmt) then
        call gz_write_itp_table_file(file_name, id_rank, itp_tbl_IO)
      else if(table_file_IO%iflag_format.eq.id_gzip_bin_file_fmt) then
        call gz_write_itp_table_idx_file_b                              &
     &     (file_name, id_rank, itp_tbl_IO, ierr)
#endif
!
      else if(table_file_IO%iflag_format .eq. id_ascii_file_fmt) then
        call write_itp_table_idx_file_a                                 &
     &     (file_name, id_rank, itp_tbl_IO)
      end if
!
      end subroutine sel_write_itrplte_idx_tbl
!
!-----------------------------------------------------------------------
!
      subroutine sel_write_dbl_itrplte_idx_tbl                          &
     &         (id_rank, table_file_IO, itp_tbl1_IO, itp_tbl2_IO)
!
      use set_interpolate_file_name
      use itrplte_tbl_file_IO
      use itrplte_tbl_file_IO_b
      use gz_itrplte_tbl_file_IO
      use gz_itrplte_table_file_IO_b
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
      type(interpolate_table), intent(in) :: itp_tbl1_IO
      type(interpolate_table), intent(in) :: itp_tbl2_IO
!
      character(len=kchara) :: file_name
      integer(kind = kint) :: ierr = 0
!
!
      file_name = s_set_interpolate_file_name(id_rank, table_file_IO)
!
      if (table_file_IO%iflag_format .eq. id_binary_file_fmt) then
        call write_dbl_itp_tbl_idx_file_b                               &
     &     (file_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format.eq.id_gzip_txt_file_fmt) then
        call gz_wt_dbl_itp_tbl_idx_file_a                               &
     &     (file_name, id_rank, itp_tbl1_IO, itp_tbl2_IO)
      else if(table_file_IO%iflag_format.eq.id_gzip_bin_file_fmt) then
        call gz_wt_dbl_itp_tbl_idx_file_b                               &
     &     (file_name, id_rank, itp_tbl1_IO, itp_tbl2_IO, ierr)
#endif
!
      else if(table_file_IO%iflag_format .eq. id_ascii_file_fmt) then
        call write_dbl_itp_tbl_idx_file_a                               &
     &     (file_name, id_rank, itp_tbl1_IO, itp_tbl2_IO)
      end if
!
      end subroutine sel_write_dbl_itrplte_idx_tbl
!
!-----------------------------------------------------------------------
!
      end module itrplte_tbl_idx_IO_select

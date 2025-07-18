!>@file   sel_dbl_itrplt_tbl_file_IO.f90
!!@brief  module sel_dbl_itrplt_tbl_file_IO
!!
!!@author H. Matsui
!!@date Programmed in Sep. 2006 (ver 1.2)
!!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine sel_write_dbl_itrplte_coef_tbl                       &
!!     &         (id_rank, table_file_IO,                               &
!!     &          itp_tbl1_org_IO, itp_tbl1_dest_IO,                    &
!!     &          itp_tbl2_org_IO, itp_tbl2_dest_IO)
!!      subroutine sel_write_dbl_itrplte_idx_tbl(id_rank, table_file_IO,&
!!     &          itp_tbl1_org_IO, itp_tbl1_dest_IO,                    &
!!     &          itp_tbl2_org_IO, itp_tbl2_dest_IO)
!!        integer, intent(in) :: id_rank
!!        type(field_IO_params), intent(in) ::  table_file_IO
!!        type(interpolate_table_org), intent(in) :: itp_tbl1_org_IO
!!        type(interpolate_table_dest), intent(in) :: itp_tbl1_dest_IO
!!        type(interpolate_table_org), intent(in) :: itp_tbl2_org_IO
!!        type(interpolate_table_dest), intent(in) :: itp_tbl2_dest_IO
!!
!!      subroutine sel_read_dbl_itrplte_coef_tbl(id_rank, table_file_IO,&
!!     &          itp_tbl1_org_IO, itp_tbl1_dest_IO,                    &
!!     &          itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
!!      subroutine sel_read_dbl_itrplte_idx_tbl(id_rank, table_file_IO, &
!!     &          itp_tbl1_org_IO, itp_tbl1_dest_IO,                    &
!!     &          itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
!!        integer, intent(in) :: id_rank
!!        type(field_IO_params), intent(in) ::  table_file_IO
!!        type(interpolate_table_org), intent(inout) :: itp_tbl1_org_IO
!!        type(interpolate_table_dest), intent(inout) :: itp_tbl1_dest_IO
!!        type(interpolate_table_org), intent(inout) :: itp_tbl2_org_IO
!!        type(interpolate_table_dest), intent(inout) :: itp_tbl2_dest_IO
!!        integer(kind = kint), intent(inout) :: ierr
!!@endverbatim
!
      module sel_dbl_itrplt_tbl_file_IO
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
      subroutine sel_write_dbl_itrplte_coef_tbl                         &
     &         (id_rank, table_file_IO,                                 &
     &          itp_tbl1_org_IO, itp_tbl1_dest_IO,                      &
     &          itp_tbl2_org_IO, itp_tbl2_dest_IO)
!
      use set_interpolate_file_name
      use dbl_itrplte_tbl_file_IO
      use dbl_itrplte_tbl_file_IO_b
      use gz_dbl_itrplt_tbl_file_IO
      use gz_dbl_itrplt_tbl_file_IO_b
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
      type(interpolate_table_org), intent(in) :: itp_tbl1_org_IO
      type(interpolate_table_dest), intent(in) :: itp_tbl1_dest_IO
      type(interpolate_table_org), intent(in) :: itp_tbl2_org_IO
      type(interpolate_table_dest), intent(in) :: itp_tbl2_dest_IO
!
      character(len=kchara) :: file_name
      integer(kind = kint) :: ierr = 0
!
!
      file_name = s_set_interpolate_file_name(id_rank, table_file_IO)
!
      if (table_file_IO%iflag_format .eq. id_binary_file_fmt) then
        call write_dbl_itp_tbl_coef_file_b(file_name, id_rank,          &
     &      itp_tbl1_org_IO, itp_tbl1_dest_IO,                          &
     &      itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format.eq.id_gzip_txt_file_fmt) then
        call gz_wt_dbl_itp_tbl_coef_file_a(file_name, id_rank,          &
     &      itp_tbl1_org_IO, itp_tbl1_dest_IO,                          &
     &      itp_tbl2_org_IO, itp_tbl2_dest_IO)
      else if(table_file_IO%iflag_format.eq.id_gzip_bin_file_fmt) then
        call gz_wt_dbl_itp_tbl_coef_file_b(file_name, id_rank,          &
     &      itp_tbl1_org_IO, itp_tbl1_dest_IO,                          &
     &      itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
#endif
!
      else if(table_file_IO%iflag_format .eq. id_ascii_file_fmt) then
        call write_dbl_itp_tbl_coef_file_a(file_name, id_rank,          &
     &      itp_tbl1_org_IO, itp_tbl1_dest_IO,                          &
     &      itp_tbl2_org_IO, itp_tbl2_dest_IO)
      end if
!
      end subroutine sel_write_dbl_itrplte_coef_tbl
!
!-----------------------------------------------------------------------
!
      subroutine sel_write_dbl_itrplte_idx_tbl(id_rank, table_file_IO,  &
     &          itp_tbl1_org_IO, itp_tbl1_dest_IO,                      &
     &          itp_tbl2_org_IO, itp_tbl2_dest_IO)
!
      use set_interpolate_file_name
      use dbl_itrplte_tbl_file_IO
      use dbl_itrplte_tbl_file_IO_b
      use gz_dbl_itrplt_tbl_file_IO
      use gz_dbl_itrplt_tbl_file_IO_b
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
      type(interpolate_table_org), intent(in) :: itp_tbl1_org_IO
      type(interpolate_table_dest), intent(in) :: itp_tbl1_dest_IO
      type(interpolate_table_org), intent(in) :: itp_tbl2_org_IO
      type(interpolate_table_dest), intent(in) :: itp_tbl2_dest_IO
!
      character(len=kchara) :: file_name
      integer(kind = kint) :: ierr = 0
!
!
      file_name = s_set_interpolate_file_name(id_rank, table_file_IO)
!
      if (table_file_IO%iflag_format .eq. id_binary_file_fmt) then
        call write_dbl_itp_tbl_idx_file_b(file_name, id_rank,           &
     &      itp_tbl1_org_IO, itp_tbl1_dest_IO,                          &
     &      itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format.eq.id_gzip_txt_file_fmt) then
        call gz_wt_dbl_itp_tbl_idx_file_a(file_name, id_rank,           &
     &      itp_tbl1_org_IO, itp_tbl1_dest_IO,                          &
     &      itp_tbl2_org_IO, itp_tbl2_dest_IO)
      else if(table_file_IO%iflag_format.eq.id_gzip_bin_file_fmt) then
        call gz_wt_dbl_itp_tbl_idx_file_b(file_name, id_rank,           &
     &      itp_tbl1_org_IO, itp_tbl1_dest_IO,                          &
     &      itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
#endif
!
      else if(table_file_IO%iflag_format .eq. id_ascii_file_fmt) then
        call write_dbl_itp_tbl_idx_file_a(file_name, id_rank,           &
     &      itp_tbl1_org_IO, itp_tbl1_dest_IO,                          &
     &      itp_tbl2_org_IO, itp_tbl2_dest_IO)
      end if
!
      end subroutine sel_write_dbl_itrplte_idx_tbl
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_read_dbl_itrplte_coef_tbl(id_rank, table_file_IO,  &
     &          itp_tbl1_org_IO, itp_tbl1_dest_IO,                      &
     &          itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
!
      use set_interpolate_file_name
      use dbl_itrplte_tbl_file_IO
      use dbl_itrplte_tbl_file_IO_b
      use gz_dbl_itrplt_tbl_file_IO
      use gz_dbl_itrplt_tbl_file_IO_b
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
!
      type(interpolate_table_org), intent(inout) :: itp_tbl1_org_IO
      type(interpolate_table_dest), intent(inout) :: itp_tbl1_dest_IO
      type(interpolate_table_org), intent(inout) :: itp_tbl2_org_IO
      type(interpolate_table_dest), intent(inout) :: itp_tbl2_dest_IO
      integer(kind = kint), intent(inout) :: ierr
!
      character(len=kchara) :: file_name
!
!
      file_name = s_set_interpolate_file_name(id_rank, table_file_IO)
!
      if (table_file_IO%iflag_format .eq. id_binary_file_fmt) then
        call read_dbl_itp_tbl_coef_file_b(file_name, id_rank,           &
     &      itp_tbl1_org_IO, itp_tbl1_dest_IO,                          &
     &      itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format.eq.id_gzip_txt_file_fmt) then
        call gz_rd_dbl_itp_tbl_coef_file_a(file_name, id_rank,          &
     &      itp_tbl1_org_IO, itp_tbl1_dest_IO,                          &
     &      itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
      else if(table_file_IO%iflag_format.eq.id_gzip_bin_file_fmt) then
        call gz_rd_dbl_itp_tbl_coef_file_b(file_name, id_rank,          &
     &      itp_tbl1_org_IO, itp_tbl1_dest_IO,                          &
     &      itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
#endif
!
      else if(table_file_IO%iflag_format .eq. id_ascii_file_fmt) then
        call read_dbl_itp_tbl_coef_file_a(file_name, id_rank,           &
     &      itp_tbl1_org_IO, itp_tbl1_dest_IO,                          &
     &      itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
      end if
!
      end subroutine sel_read_dbl_itrplte_coef_tbl
!
!-----------------------------------------------------------------------
!
      subroutine sel_read_dbl_itrplte_idx_tbl(id_rank, table_file_IO,   &
     &          itp_tbl1_org_IO, itp_tbl1_dest_IO,                      &
     &          itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
!
      use set_interpolate_file_name
      use dbl_itrplte_tbl_file_IO
      use dbl_itrplte_tbl_file_IO_b
      use gz_dbl_itrplt_tbl_file_IO
      use gz_dbl_itrplt_tbl_file_IO_b
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
!
      type(interpolate_table_org), intent(inout) :: itp_tbl1_org_IO
      type(interpolate_table_dest), intent(inout) :: itp_tbl1_dest_IO
      type(interpolate_table_org), intent(inout) :: itp_tbl2_org_IO
      type(interpolate_table_dest), intent(inout) :: itp_tbl2_dest_IO
      integer(kind = kint), intent(inout) :: ierr
!
      character(len=kchara) :: file_name
!
!
      file_name = s_set_interpolate_file_name(id_rank, table_file_IO)
!
      if (table_file_IO%iflag_format .eq. id_binary_file_fmt) then
        call read_dbl_itp_tbl_idx_file_b(file_name, id_rank,            &
     &      itp_tbl1_org_IO, itp_tbl1_dest_IO,                          &
     &      itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format.eq.id_gzip_txt_file_fmt) then
        call gz_rd_dbl_itp_tbl_idx_file_a(file_name, id_rank,           &
     &      itp_tbl1_org_IO, itp_tbl1_dest_IO,                          &
     &      itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
      else if(table_file_IO%iflag_format.eq.id_gzip_bin_file_fmt) then
        call gz_rd_dbl_itp_tbl_idx_file_b(file_name, id_rank,           &
     &      itp_tbl1_org_IO, itp_tbl1_dest_IO,                          &
     &      itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
#endif
!
      else if(table_file_IO%iflag_format .eq. id_ascii_file_fmt) then
        call read_dbl_itp_tbl_idx_file_a(file_name, id_rank,            &
     &      itp_tbl1_org_IO, itp_tbl1_dest_IO,                          &
     &      itp_tbl2_org_IO, itp_tbl2_dest_IO, ierr)
      end if
!
      end subroutine sel_read_dbl_itrplte_idx_tbl
!
!-----------------------------------------------------------------------
!
      end module sel_dbl_itrplt_tbl_file_IO

!>@file   para_itrplte_table_IO_sel.F90
!!@brief  module para_itrplte_table_IO_sel
!!
!!@author H. Matsui
!!@date Programmed in Sep. 2006 (ver 1.2)
!!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!       subroutine sel_mpi_read_interpolate_table                      &
!!     &         (id_rank, num_pe, table_file_IO, itp_tbl_IO, ierr)
!!        type(field_IO_params), intent(in) ::  table_file_IO
!!        type(interpolate_table), intent(inout) :: itp_tbl_IO
!!      subroutine sel_mpi_read_dbl_itp_table(id_rank, num_pe,          &
!!     &          table_file_IO, itp_tbl1_IO, itp_tbl2_IO, ierr)
!!        type(field_IO_params), intent(in) ::  table_file_IO
!!        type(interpolate_table), intent(inout) :: itp_tbl1_IO
!!        type(interpolate_table), intent(inout) :: itp_tbl2_IO
!!
!!      subroutine sel_mpi_write_interpolate_table                      &
!!     &         (id_rank, table_file_IO, itp_tbl_IO)
!!        type(field_IO_params), intent(in) ::  table_file_IO
!!        type(interpolate_table), intent(inout) :: itp_tbl_IO
!!      subroutine sel_mpi_write_dbl_itp_table                          &
!!     &         (id_rank, table_file_IO, itp_tbl1_IO, itp_tbl2_IO)
!!        type(field_IO_params), intent(in) ::  table_file_IO
!!        type(interpolate_table), intent(inout) :: itp_tbl1_IO
!!        type(interpolate_table), intent(inout) :: itp_tbl2_IO
!!@endverbatim
!
      module para_itrplte_table_IO_sel
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
      subroutine sel_mpi_read_interpolate_table                         &
     &         (id_rank, num_pe, table_file_IO, itp_tbl_IO, ierr)
!
      use set_interpolate_file_name
      use MPI_itrplte_tbl_file_IO
      use MPI_itrplte_tbl_file_IO_b
      use gz_MPI_itp_table_file_IO
      use gz_MPI_itrplte_tbl_file_IO_b
      use itrplte_tbl_coef_IO_select
!
      integer, intent(in) :: id_rank, num_pe
      type(field_IO_params), intent(in) ::  table_file_IO
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table), intent(inout) :: itp_tbl_IO
!
      character(len=kchara) :: file_name
!
!
      file_name = set_mpi_interpolate_file_name(id_rank, table_file_IO)
!
      if (table_file_IO%iflag_format                                    &
     &         .eq. iflag_single+id_binary_file_fmt) then
        call mpi_read_itp_table_coef_file_b                             &
     &     (file_name, id_rank, num_pe, itp_tbl_IO)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format                                &
     &         .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_read_itp_table_file                                 &
     &     (file_name, id_rank, num_pe, itp_tbl_IO)
      else if(table_file_IO%iflag_format                                &
     &         .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_rd_itp_tbl_coef_file_b                              &
     &     (file_name, id_rank, num_pe, itp_tbl_IO)
#endif
!
      else if(table_file_IO%iflag_format                                &
     &         .eq. iflag_single+id_ascii_file_fmt) then
        call mpi_read_itp_table_coef_file_a                             &
     &     (file_name, id_rank, num_pe, itp_tbl_IO)
!
      else
        call sel_read_itrplte_coef_tbl                                  &
     &     (id_rank, table_file_IO, itp_tbl_IO, ierr)
      end if
!
      end subroutine sel_mpi_read_interpolate_table
!
!-----------------------------------------------------------------------
!
      subroutine sel_mpi_read_dbl_itp_table(id_rank, num_pe,            &
     &          table_file_IO, itp_tbl1_IO, itp_tbl2_IO, ierr)
!
      use set_interpolate_file_name
      use MPI_itrplte_tbl_file_IO
      use MPI_itrplte_tbl_file_IO_b
      use gz_MPI_itp_table_file_IO
      use gz_MPI_itrplte_tbl_file_IO_b
      use itrplte_tbl_coef_IO_select
!
      integer, intent(in) :: id_rank, num_pe
      type(field_IO_params), intent(in) ::  table_file_IO
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table), intent(inout) :: itp_tbl1_IO
      type(interpolate_table), intent(inout) :: itp_tbl2_IO
!
      character(len=kchara) :: file_name
!
!
      file_name = set_mpi_interpolate_file_name(id_rank, table_file_IO)
!
      if (table_file_IO%iflag_format                                    &
     &         .eq. iflag_single+id_binary_file_fmt) then
        call mpi_rd_dbl_itp_tbl_coef_file_b                             &
     &     (file_name, id_rank, num_pe, itp_tbl1_IO, itp_tbl2_IO)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format                                &
     &         .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_read_dbl_itp_tbl_file                               &
     &     (file_name, id_rank, num_pe, itp_tbl1_IO, itp_tbl2_IO)
      else if(table_file_IO%iflag_format                                &
     &         .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_rd_dbl_itbl_coef_file_b                             &
     &     (file_name, id_rank, num_pe, itp_tbl1_IO, itp_tbl2_IO)
#endif
!
      else if(table_file_IO%iflag_format                                &
     &         .eq. iflag_single+id_ascii_file_fmt) then
        call mpi_rd_dbl_itp_tbl_coef_file_a                             &
     &     (file_name, id_rank, num_pe, itp_tbl1_IO, itp_tbl2_IO)
!
      else
        call sel_read_dbl_itrplte_coef_tbl                              &
     &     (id_rank, table_file_IO, itp_tbl1_IO, itp_tbl2_IO, ierr)
      end if
!
      end subroutine sel_mpi_read_dbl_itp_table
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine sel_mpi_write_interpolate_table                        &
     &         (id_rank, table_file_IO, itp_tbl_IO)
!
      use set_interpolate_file_name
      use MPI_itrplte_tbl_file_IO
      use MPI_itrplte_tbl_file_IO_b
      use gz_MPI_itp_table_file_IO
      use gz_MPI_itrplte_tbl_file_IO_b
      use itrplte_tbl_coef_IO_select
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
      type(interpolate_table), intent(inout) :: itp_tbl_IO
!
      character(len=kchara) :: file_name
!
!
      file_name = set_mpi_interpolate_file_name(id_rank, table_file_IO)
!
      if (table_file_IO%iflag_format                                    &
     &         .eq. iflag_single+id_binary_file_fmt) then
        call mpi_write_itp_table_coef_file_b(file_name, itp_tbl_IO)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format                                &
     &         .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_write_itp_table_file(file_name, itp_tbl_IO)
      else if(table_file_IO%iflag_format                                &
     &         .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_wt_itp_tbl_coef_file_b(file_name, itp_tbl_IO)
#endif
!
      else if(table_file_IO%iflag_format                                &
     &         .eq. iflag_single+id_ascii_file_fmt) then
        call mpi_write_itp_table_coef_file_a(file_name, itp_tbl_IO)
!
      else
        call sel_write_itrplte_coef_tbl                                 &
     &     (id_rank, table_file_IO, itp_tbl_IO)
      end if
!
      end subroutine sel_mpi_write_interpolate_table
!
!-----------------------------------------------------------------------
!
      subroutine sel_mpi_write_dbl_itp_table                            &
     &         (id_rank, table_file_IO, itp_tbl1_IO, itp_tbl2_IO)
!
      use set_interpolate_file_name
      use MPI_itrplte_tbl_file_IO
      use MPI_itrplte_tbl_file_IO_b
      use gz_MPI_itp_table_file_IO
      use gz_MPI_itrplte_tbl_file_IO_b
      use itrplte_tbl_coef_IO_select
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
      type(interpolate_table), intent(inout) :: itp_tbl1_IO
      type(interpolate_table), intent(inout) :: itp_tbl2_IO
!
      character(len=kchara) :: file_name
!
!
      file_name = set_mpi_interpolate_file_name(id_rank, table_file_IO)
!
      if (table_file_IO%iflag_format                                    &
     &         .eq. iflag_single+id_binary_file_fmt) then
        call mpi_wt_dbl_itp_tbl_coef_file_b                             &
     &     (file_name, itp_tbl1_IO, itp_tbl2_IO)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format                                &
     &         .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_write_dbl_itp_tbl_file                              &
     &     (file_name, itp_tbl1_IO, itp_tbl2_IO)
      else if(table_file_IO%iflag_format                                &
     &         .eq. iflag_single+id_gzip_bin_file_fmt) then
        call gz_mpi_wt_dbl_itbl_coef_file_b                             &
     &     (file_name, itp_tbl1_IO, itp_tbl2_IO)
#endif
!
      else if(table_file_IO%iflag_format                                &
     &         .eq. iflag_single+id_ascii_file_fmt) then
        call mpi_wt_dbl_itp_tbl_coef_file_a                             &
     &     (file_name, itp_tbl1_IO, itp_tbl2_IO)
!
      else
        call sel_write_dbl_itrplte_coef_tbl                             &
     &     (id_rank, table_file_IO, itp_tbl1_IO, itp_tbl2_IO)
      end if
!
      end subroutine sel_mpi_write_dbl_itp_table
!
!-----------------------------------------------------------------------
!
      end module para_itrplte_table_IO_sel

!>@file   parallel_itp_work_IO_select.F90
!!@brief  module parallel_itp_work_IO_select
!!
!!@author H. Matsui
!!@date Programmed in Sep. 2006 (ver 1.2)
!!
!>@brief  Make grouping with respect to volume
!!
!!@verbatim
!!      subroutine sel_mpi_write_itp_coefs_dest                         &
!!     &         (id_rank, table_file_IO, IO_itp_dest, IO_itp_c_dest)
!!      subroutine sel_mpi_read_itp_coefs_dest(id_rank, num_pe,         &
!!     &          table_file_IO, IO_itp_dest, IO_itp_c_dest, ierr)
!!        type(field_IO_params), intent(in) ::  table_file_IO
!!      subroutine sel_mpi_read_itp_table_dest                          &
!!     &         (id_rank, num_pe, table_file_IO, IO_itp_dest, ierr)
!!      subroutine sel_mpi_read_itp_domain_dest                         &
!!     &         (id_rank, num_pe, table_file_IO, IO_itp_dest, ierr)
!!        type(field_IO_params), intent(in) ::  table_file_IO
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!@endverbatim
!
      module parallel_itp_work_IO_select
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
      use MPI_itp_work_file_IO
      use MPI_itp_work_file_IO_b
      use gz_MPI_itp_work_file_IO
      use gz_MPI_itp_work_file_IO_b
      use itp_work_file_IO_select
!
      implicit none
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine sel_mpi_write_itp_coefs_dest                           &
     &         (id_rank, table_file_IO, IO_itp_dest, IO_itp_c_dest)
!
      use set_interpolate_file_name
!
      integer, intent(in) :: id_rank
      type(field_IO_params), intent(in) ::  table_file_IO
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
      character(len=kchara) :: file_name
!
!
      file_name = set_mpi_interpolate_work_name(id_rank, table_file_IO)
!
      if (table_file_IO%iflag_format                                    &
     &         .eq. iflag_single+id_binary_file_fmt) then
        call  mpi_wrt_itp_coefs_dest_file_b                             &
     &     (file_name, IO_itp_dest, IO_itp_c_dest)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format                                &
     &         .eq. iflag_single+id_gzip_txt_file_fmt) then
        call  gz_mpi_wrt_itp_coefs_dest_file                            &
     &     (file_name, IO_itp_dest, IO_itp_c_dest)
      else if(table_file_IO%iflag_format                                &
     &         .eq. iflag_single+id_gzip_bin_file_fmt) then
        call  wrt_gz_mpi_itp_coef_dest_file_b                           &
     &     (file_name, id_rank, IO_itp_dest, IO_itp_c_dest)
#endif
!
      else if(table_file_IO%iflag_format                                &
     &         .eq. iflag_single+id_ascii_file_fmt) then
        call  mpi_wrt_itp_coefs_dest_file_a                             &
     &     (file_name, IO_itp_dest, IO_itp_c_dest)
!
      else
        call sel_write_itp_coefs_dest                                   &
     &     (id_rank, table_file_IO, IO_itp_dest, IO_itp_c_dest)
      end if
!
      call dealloc_itp_dest_after_write(IO_itp_dest, IO_itp_c_dest)
!
      end subroutine sel_mpi_write_itp_coefs_dest
!
!-----------------------------------------------------------------------
!
      subroutine sel_mpi_read_itp_coefs_dest(id_rank, num_pe,           &
     &          table_file_IO, IO_itp_dest, IO_itp_c_dest, ierr)
!
      use set_interpolate_file_name
!
      integer, intent(in) :: id_rank, num_pe
      type(field_IO_params), intent(in) ::  table_file_IO
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
      character(len=kchara) :: file_name
!
!
      file_name = set_mpi_interpolate_work_name(id_rank, table_file_IO)
!
      if (table_file_IO%iflag_format                                    &
     &         .eq. iflag_single+id_binary_file_fmt) then
        call mpi_read_itp_coefs_dest_file_b                             &
     &     (file_name, id_rank, num_pe, IO_itp_dest, IO_itp_c_dest)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format                                &
     &         .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_read_itp_coefs_dest_file                            &
     &     (file_name, id_rank, num_pe, IO_itp_dest, IO_itp_c_dest)
      else if(table_file_IO%iflag_format                                &
     &         .eq. iflag_single+id_gzip_bin_file_fmt) then
        call read_gz_mpi_itp_coef_dst_file_b                            &
     &     (file_name, id_rank, num_pe, IO_itp_dest, IO_itp_c_dest)
#endif
!
      else if(table_file_IO%iflag_format                                &
     &         .eq. iflag_single+id_ascii_file_fmt) then
        call mpi_read_itp_coefs_dest_file_a                             &
     &     (file_name, id_rank, num_pe, IO_itp_dest, IO_itp_c_dest)
!
      else
        call sel_read_itp_coefs_dest(id_rank, table_file_IO,            &
     &          IO_itp_dest, IO_itp_c_dest, ierr)
      end if
!
      end subroutine sel_mpi_read_itp_coefs_dest
!
!-----------------------------------------------------------------------
!
      subroutine sel_mpi_read_itp_table_dest                            &
     &         (id_rank, num_pe, table_file_IO, IO_itp_dest, ierr)
!
      use set_interpolate_file_name
!
      integer, intent(in) :: id_rank, num_pe
      type(field_IO_params), intent(in) ::  table_file_IO
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
      character(len=kchara) :: file_name
!
!
      file_name = set_mpi_interpolate_work_name(id_rank, table_file_IO)
!
      if (table_file_IO%iflag_format                                    &
     &         .eq. iflag_single+id_binary_file_fmt) then
        call mpi_read_itp_table_dest_file_b                             &
     &     (file_name, id_rank, num_pe, IO_itp_dest)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format                                &
     &         .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_read_itp_tbl_dest_file                              &
     &     (file_name, id_rank, num_pe, IO_itp_dest)
      else if(table_file_IO%iflag_format                                &
     &         .eq. iflag_single+id_gzip_bin_file_fmt) then
        call read_gz_mpi_itp_tbl_dest_file_b                            &
     &     (file_name, id_rank, num_pe, IO_itp_dest)
#endif
!
      else if(table_file_IO%iflag_format                                &
     &         .eq. iflag_single+id_ascii_file_fmt) then
        call mpi_read_itp_table_dest_file_a                             &
     &     (file_name, id_rank, num_pe, IO_itp_dest)
!
      else
        call sel_read_itp_table_dest                                    &
     &     (id_rank, table_file_IO, IO_itp_dest, ierr)
      end if
!
      end subroutine sel_mpi_read_itp_table_dest
!
!-----------------------------------------------------------------------
!
      subroutine sel_mpi_read_itp_domain_dest                           &
     &         (id_rank, num_pe, table_file_IO, IO_itp_dest, ierr)
!
      use set_interpolate_file_name
!
      integer, intent(in) :: id_rank, num_pe
      type(field_IO_params), intent(in) ::  table_file_IO
!
      integer(kind = kint), intent(inout) :: ierr
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
      character(len=kchara) :: file_name
!
!
      file_name = set_mpi_interpolate_work_name(id_rank, table_file_IO)
!
      if (table_file_IO%iflag_format                                    &
     &         .eq. iflag_single+id_binary_file_fmt) then
        call mpi_read_itp_domain_dest_file_b                            &
     &     (file_name, id_rank, num_pe, IO_itp_dest)
!
#ifdef ZLIB_IO
      else if(table_file_IO%iflag_format                                &
     &         .eq. iflag_single+id_gzip_txt_file_fmt) then
        call gz_mpi_read_itp_dmn_dest_file                              &
     &     (file_name, id_rank, num_pe, IO_itp_dest)
      else if(table_file_IO%iflag_format                                &
     &         .eq. iflag_single+id_gzip_bin_file_fmt) then
        call read_gz_mpi_itp_dmn_dest_file_b                            &
     &     (file_name, id_rank, num_pe, IO_itp_dest)
#endif
!
      else if(table_file_IO%iflag_format                                &
     &         .eq. iflag_single+id_ascii_file_fmt) then
        call mpi_read_itp_domain_dest_file_a                            &
     &     (file_name, id_rank, num_pe, IO_itp_dest)
      else
        call sel_read_itp_domain_dest                                   &
     &         (id_rank, table_file_IO, IO_itp_dest, ierr)
      end if
!
      end subroutine sel_mpi_read_itp_domain_dest
!
!-----------------------------------------------------------------------
!
      end module parallel_itp_work_IO_select

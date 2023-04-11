!>@file   MPI_itrplte_tbl_file_IO.f90
!!@brief  module MPI_itrplte_tbl_file_IO
!!
!!@author H. Matsui
!!@date Programmed in Sep. 2006
!
!>@brief ASCII Interpolation table file IO
!!
!!@verbatim
!!      subroutine mpi_write_itp_table_coef_file_a(file_name, itp_tbl_IO)
!!      subroutine mpi_write_itp_table_idx_file_a(file_name, itp_tbl_IO)
!!        type(interpolate_table), intent(in) :: itp_tbl_IO
!!      subroutine mpi_wt_dbl_itp_tbl_coef_file_a                       &
!!     &         (file_name, itp_tbl1_IO, itp_tbl2_IO)
!!      subroutine mpi_wt_dbl_itp_tbl_idx_file_a                        &
!!     &         (file_name, itp_tbl1_IO, itp_tbl2_IO)
!!        type(interpolate_table), intent(in) :: itp_tbl1_IO
!!        type(interpolate_table), intent(in) :: itp_tbl2_IO
!!
!!      subroutine mpi_read_itp_table_coef_file_a                       &
!!     &         (file_name, id_rank, num_pe, itp_tbl_IO)
!!      subroutine mpi_read_itp_table_idx_file_a                        &
!!     &         (file_name, id_rank, num_pe, itp_tbl_IO)
!!        type(interpolate_table), intent(inout) :: itp_tbl_IO
!!      subroutine mpi_rd_dbl_itp_tbl_coef_file_a                       &
!!     &         (file_name, id_rank, num_pe, itp_tbl1_IO, itp_tbl2_IO)
!!      subroutine mpi_rd_dbl_itp_tbl_idx_file_a                        &
!!     &         (file_name, id_rank, num_pe, itp_tbl1_IO, itp_tbl2_IO)
!!        type(interpolate_table), intent(inout) :: itp_tbl1_IO
!!        type(interpolate_table), intent(inout) :: itp_tbl2_IO
!!@endverbatim
!
      module MPI_itrplte_tbl_file_IO
!
      use m_precision
      use m_error_IDs
!
      use t_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
      use t_calypso_mpi_IO_param
      use m_interpolation_data_labels
!
      implicit none
!
      type(calypso_MPI_IO_params), save, private :: IO_param1
!
      private :: mpi_write_each_itp_idx_tbl_a
      private :: mpi_read_each_itp_idx_tbl_a
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine mpi_write_itp_table_coef_file_a(file_name, itp_tbl_IO)
!
      use MPI_ascii_data_IO
      use MPI_itp_table_org_data_IO
!
      character(len=kchara), intent(in) :: file_name
      type(interpolate_table), intent(in) :: itp_tbl_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write merged ascii interpolation file: ', trim(file_name)
!
      call open_write_mpi_file(file_name, IO_param1)
      call mpi_write_each_itp_idx_tbl_a(IO_param1, itp_tbl_IO)
      call mpi_write_itp_coefs_org(IO_param1, itp_tbl_IO%tbl_org)
      call close_mpi_file(IO_param1)
!
      end subroutine mpi_write_itp_table_coef_file_a
!
!-----------------------------------------------------------------------
!
      subroutine mpi_write_itp_table_idx_file_a(file_name, itp_tbl_IO)
!
      use MPI_ascii_data_IO
!
      character(len=kchara), intent(in) :: file_name
      type(interpolate_table), intent(in) :: itp_tbl_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write merged ascii interpolation file: ', trim(file_name)
!
      call open_write_mpi_file(file_name, IO_param1)
      call mpi_write_each_itp_idx_tbl_a(IO_param1, itp_tbl_IO)
      call close_mpi_file(IO_param1)
!
      end subroutine mpi_write_itp_table_idx_file_a
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine mpi_wt_dbl_itp_tbl_coef_file_a                         &
     &         (file_name, itp_tbl1_IO, itp_tbl2_IO)
!
      use MPI_ascii_data_IO
      use MPI_itp_table_org_data_IO
!
      character(len=kchara), intent(in) :: file_name
      type(interpolate_table), intent(in) :: itp_tbl1_IO
      type(interpolate_table), intent(in) :: itp_tbl2_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write merged ascii interpolation file: ', trim(file_name)
!
      call open_write_mpi_file(file_name, IO_param1)
      call mpi_write_each_itp_idx_tbl_a(IO_param1, itp_tbl1_IO)
      call mpi_write_itp_coefs_org(IO_param1, itp_tbl1_IO%tbl_org)
!
      call mpi_write_each_itp_idx_tbl_a(IO_param1, itp_tbl2_IO)
      call mpi_write_itp_coefs_org(IO_param1, itp_tbl2_IO%tbl_org)
      call close_mpi_file(IO_param1)
!
      end subroutine mpi_wt_dbl_itp_tbl_coef_file_a
!
!-----------------------------------------------------------------------
!
      subroutine mpi_wt_dbl_itp_tbl_idx_file_a                          &
     &         (file_name, itp_tbl1_IO, itp_tbl2_IO)
!
      use MPI_ascii_data_IO
!
      character(len=kchara), intent(in) :: file_name
      type(interpolate_table), intent(in) :: itp_tbl1_IO
      type(interpolate_table), intent(in) :: itp_tbl2_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write merged ascii interpolation file: ', trim(file_name)
!
      call open_write_mpi_file(file_name, IO_param1)
      call mpi_write_each_itp_idx_tbl_a(IO_param1, itp_tbl1_IO)
      call mpi_write_each_itp_idx_tbl_a(IO_param1, itp_tbl2_IO)
      call close_mpi_file(IO_param1)
!
      end subroutine mpi_wt_dbl_itp_tbl_idx_file_a
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine mpi_read_itp_table_coef_file_a                         &
     &         (file_name, id_rank, num_pe, itp_tbl_IO)
!
      use MPI_ascii_data_IO
      use MPI_itp_table_org_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank, num_pe
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read merged ascii interpolation file: ', trim(file_name)
!
      call open_read_mpi_file(file_name, num_pe, id_rank, IO_param1)
      call mpi_read_each_itp_idx_tbl_a(IO_param1, itp_tbl_IO)
      call mpi_read_itp_coefs_org(IO_param1, itp_tbl_IO%tbl_org)
      call close_mpi_file(IO_param1)
!
      end subroutine mpi_read_itp_table_coef_file_a
!
!-----------------------------------------------------------------------
!
      subroutine mpi_read_itp_table_idx_file_a                          &
     &         (file_name, id_rank, num_pe, itp_tbl_IO)
!
      use MPI_ascii_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank, num_pe
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read merged ascii interpolation file: ', trim(file_name)
!
      call open_read_mpi_file(file_name, num_pe, id_rank, IO_param1)
      call mpi_read_each_itp_idx_tbl_a(IO_param1, itp_tbl_IO)
      call close_mpi_file(IO_param1)
!
      end subroutine mpi_read_itp_table_idx_file_a
!
!-----------------------------------------------------------------------
!
      subroutine mpi_rd_dbl_itp_tbl_coef_file_a                         &
     &         (file_name, id_rank, num_pe, itp_tbl1_IO, itp_tbl2_IO)
!
      use MPI_ascii_data_IO
      use MPI_itp_table_org_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank, num_pe
!
      type(interpolate_table), intent(inout) :: itp_tbl1_IO
      type(interpolate_table), intent(inout) :: itp_tbl2_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read merged ascii interpolation file: ', trim(file_name)
!
      call open_read_mpi_file(file_name, num_pe, id_rank, IO_param1)
      call mpi_read_each_itp_idx_tbl_a(IO_param1, itp_tbl1_IO)
      call mpi_read_itp_coefs_org(IO_param1, itp_tbl1_IO%tbl_org)
      call mpi_read_each_itp_idx_tbl_a(IO_param1, itp_tbl2_IO)
      call mpi_read_itp_coefs_org(IO_param1, itp_tbl2_IO%tbl_org)
      call close_mpi_file(IO_param1)
!
      end subroutine mpi_rd_dbl_itp_tbl_coef_file_a
!
!-----------------------------------------------------------------------
!
      subroutine mpi_rd_dbl_itp_tbl_idx_file_a                          &
     &         (file_name, id_rank, num_pe, itp_tbl1_IO, itp_tbl2_IO)
!
      use MPI_ascii_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank, num_pe
!
      type(interpolate_table), intent(inout) :: itp_tbl1_IO
      type(interpolate_table), intent(inout) :: itp_tbl2_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read merged ascii interpolation file: ', trim(file_name)
!
      call open_read_mpi_file(file_name, num_pe, id_rank, IO_param1)
      call mpi_read_each_itp_idx_tbl_a(IO_param1, itp_tbl1_IO)
      call mpi_read_each_itp_idx_tbl_a(IO_param1, itp_tbl2_IO)
      call close_mpi_file(IO_param1)
!
      end subroutine mpi_rd_dbl_itp_tbl_idx_file_a
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine mpi_write_each_itp_idx_tbl_a(IO_param, itp_tbl_IO)
!
      use MPI_itp_table_dest_data_IO
      use MPI_itp_table_org_data_IO
      use MPI_ascii_data_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table), intent(in) :: itp_tbl_IO
!
!
      call mpi_write_itp_domain_dest(IO_param, itp_tbl_IO%tbl_dest)
      call mpi_write_itp_table_dest(IO_param, itp_tbl_IO%tbl_dest)
!
      call mpi_write_itp_domain_org(IO_param, itp_tbl_IO%tbl_org)
      call mpi_write_itp_table_org(IO_param, itp_tbl_IO%tbl_org)
      call mpi_write_itp_index_org(IO_param, itp_tbl_IO%tbl_org)
!
      end subroutine mpi_write_each_itp_idx_tbl_a
!
!-----------------------------------------------------------------------
!
      subroutine mpi_read_each_itp_idx_tbl_a(IO_param, itp_tbl_IO)
!
      use MPI_itp_table_org_data_IO
      use MPI_itp_table_dest_data_IO
      use MPI_ascii_data_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table), intent(inout) :: itp_tbl_IO
!
!
      call mpi_read_itp_domain_dest(IO_param, itp_tbl_IO%tbl_dest)
      call mpi_read_itp_table_dest(IO_param, itp_tbl_IO%tbl_dest)
!
      call mpi_read_itp_domain_org(IO_param, itp_tbl_IO%tbl_org)
      call mpi_read_itp_table_org(IO_param, itp_tbl_IO%tbl_org)
      call mpi_read_itp_index_org(IO_param, itp_tbl_IO%tbl_org)
!
      end subroutine mpi_read_each_itp_idx_tbl_a
!
!-----------------------------------------------------------------------
!
      end module MPI_itrplte_tbl_file_IO

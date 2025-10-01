!>@file   MPI_itrplte_tbl_file_IO_b.f90
!!@brief  module MPI_itrplte_tbl_file_IO_b
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!!
!>@brief  Binary interpolation file IO using MPi-IO
!!
!!@verbatim
!!      subroutine mpi_write_itp_table_coef_file_b                      &
!!     &         (file_name, itp_tbl_IO)
!!      subroutine mpi_write_itp_table_idx_file_b                       &
!!     &         (file_name, itp_tbl_IO)
!!        type(interpolate_table), intent(in) :: itp_tbl_IO
!!      subroutine mpi_wt_dbl_itp_tbl_coef_file_b                       &
!!     &         (file_name, itp_tbl1_IO, itp_tbl2_IO)
!!      subroutine mpi_wt_dbl_itp_tbl_idx_file_b                        &
!!     &         (file_name, itp_tbl1_IO, itp_tbl2_IO)
!!        type(interpolate_table), intent(in) :: itp_tbl1_IO
!!        type(interpolate_table), intent(in) :: itp_tbl2_IO
!!
!!      subroutine mpi_read_itp_table_coef_file_b                       &
!!     &          (file_name, id_rank, num_pe, itp_tbl_IO)
!!      subroutine mpi_read_itp_table_idx_file_b                        &
!!     &          (file_name, id_rank, num_pe, itp_tbl_IO)
!!        type(interpolate_table), intent(inout) :: itp_tbl_IO
!!      subroutine mpi_rd_dbl_itp_tbl_coef_file_b                       &
!!     &         (file_name, id_rank, num_pe, itp_tbl1_IO, itp_tbl2_IO)
!!      subroutine mpi_rd_dbl_itp_tbl_idx_file_b                        &
!!     &         (file_name, id_rank, num_pe, itp_tbl1_IO, itp_tbl2_IO)
!!        type(interpolate_table), intent(inout) :: itp_tbl1_IO
!!        type(interpolate_table), intent(inout) :: itp_tbl2_IO
!!@endverbatim
!
      module MPI_itrplte_tbl_file_IO_b
!
      use m_precision
      use m_error_IDs
!
      use t_interpolate_table
      use t_interpolate_tbl_org
      use t_interpolate_tbl_dest
      use t_interpolate_coefs_dest
      use t_binary_IO_buffer
      use t_calypso_mpi_IO_param
!
      implicit none
!
      type(calypso_MPI_IO_params), save, private :: IO_param1
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine mpi_write_itp_table_coef_file_b                        &
     &         (file_name, itp_tbl_IO)
!
      use MPI_itrplte_table_data_IO_b
      use MPI_ascii_data_IO
      use MPI_binary_head_IO
!
      character(len=kchara), intent(in) :: file_name
      type(interpolate_table), intent(in) :: itp_tbl_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write merged binary interpolation file: ', trim(file_name)
!
      call open_write_mpi_file_b(file_name, IO_param1)
      call mpi_write_each_itp_coef_table_b(IO_param1, itp_tbl_IO)
      call close_mpi_file(IO_param1)
!
      end subroutine mpi_write_itp_table_coef_file_b
!
!-----------------------------------------------------------------------
!
      subroutine mpi_write_itp_table_idx_file_b                         &
     &         (file_name, itp_tbl_IO)
!
      use MPI_itrplte_table_data_IO_b
      use MPI_ascii_data_IO
      use MPI_binary_head_IO
!
      character(len=kchara), intent(in) :: file_name
      type(interpolate_table), intent(in) :: itp_tbl_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write merged binary interpolation file: ', trim(file_name)
!
      call open_write_mpi_file_b(file_name, IO_param1)
      call mpi_write_each_itp_idx_table_b(IO_param1, itp_tbl_IO)
      call close_mpi_file(IO_param1)
!
      end subroutine mpi_write_itp_table_idx_file_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine mpi_wt_dbl_itp_tbl_coef_file_b                         &
     &         (file_name, itp_tbl1_IO, itp_tbl2_IO)
!
      use MPI_itrplte_table_data_IO_b
      use MPI_ascii_data_IO
      use MPI_binary_head_IO
!
      character(len=kchara), intent(in) :: file_name
      type(interpolate_table), intent(in) :: itp_tbl1_IO
      type(interpolate_table), intent(in) :: itp_tbl2_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write merged binary interpolation file: ', trim(file_name)
!
      call open_write_mpi_file_b(file_name, IO_param1)
      call mpi_write_each_itp_coef_table_b(IO_param1, itp_tbl1_IO)
      call mpi_write_each_itp_coef_table_b(IO_param1, itp_tbl2_IO)
      call close_mpi_file(IO_param1)
!
      end subroutine mpi_wt_dbl_itp_tbl_coef_file_b
!
!-----------------------------------------------------------------------
!
      subroutine mpi_wt_dbl_itp_tbl_idx_file_b                          &
     &         (file_name, itp_tbl1_IO, itp_tbl2_IO)
!
      use MPI_itrplte_table_data_IO_b
      use MPI_ascii_data_IO
      use MPI_binary_head_IO
!
      character(len=kchara), intent(in) :: file_name
      type(interpolate_table), intent(in) :: itp_tbl1_IO
      type(interpolate_table), intent(in) :: itp_tbl2_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write merged binary interpolation file: ', trim(file_name)
!
      call open_write_mpi_file_b(file_name, IO_param1)
      call mpi_write_each_itp_idx_table_b(IO_param1, itp_tbl1_IO)
      call mpi_write_each_itp_idx_table_b(IO_param1, itp_tbl2_IO)
      call close_mpi_file(IO_param1)
!
      end subroutine mpi_wt_dbl_itp_tbl_idx_file_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine mpi_read_itp_table_coef_file_b                         &
     &          (file_name, id_rank, num_pe, itp_tbl_IO)
!
      use MPI_itrplte_table_data_IO_b
      use MPI_ascii_data_IO
      use MPI_binary_head_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank, num_pe
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read merged  binary interpolation file: ', trim(file_name)
!
      call open_read_mpi_file_b(file_name, num_pe, id_rank, IO_param1)
      call mpi_read_each_itp_coef_table_b(IO_param1, itp_tbl_IO)
      call close_mpi_file(IO_param1)
!
      end subroutine mpi_read_itp_table_coef_file_b
!
!-----------------------------------------------------------------------
!
      subroutine mpi_read_itp_table_idx_file_b                          &
     &          (file_name, id_rank, num_pe, itp_tbl_IO)
!
      use MPI_itrplte_table_data_IO_b
      use MPI_ascii_data_IO
      use MPI_binary_head_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank, num_pe
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read merged  binary interpolation file: ', trim(file_name)
!
      call open_read_mpi_file_b(file_name, num_pe, id_rank, IO_param1)
      call mpi_read_each_itp_idx_table_b(IO_param1, itp_tbl_IO)
      call close_mpi_file(IO_param1)
!
      end subroutine mpi_read_itp_table_idx_file_b
!
!-----------------------------------------------------------------------
!
      subroutine mpi_rd_dbl_itp_tbl_coef_file_b                         &
     &         (file_name, id_rank, num_pe, itp_tbl1_IO, itp_tbl2_IO)
!
      use MPI_itrplte_table_data_IO_b
      use MPI_ascii_data_IO
      use MPI_binary_head_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank, num_pe
!
      type(interpolate_table), intent(inout) :: itp_tbl1_IO
      type(interpolate_table), intent(inout) :: itp_tbl2_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read merged  binary interpolation file: ', trim(file_name)
!
      call open_read_mpi_file_b(file_name, num_pe, id_rank, IO_param1)
      call mpi_read_each_itp_coef_table_b(IO_param1, itp_tbl1_IO)
      call mpi_read_each_itp_coef_table_b(IO_param1, itp_tbl2_IO)
      call close_mpi_file(IO_param1)
!
      end subroutine mpi_rd_dbl_itp_tbl_coef_file_b
!
!-----------------------------------------------------------------------
!
      subroutine mpi_rd_dbl_itp_tbl_idx_file_b                          &
     &         (file_name, id_rank, num_pe, itp_tbl1_IO, itp_tbl2_IO)
!
      use MPI_itrplte_table_data_IO_b
      use MPI_ascii_data_IO
      use MPI_binary_head_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank, num_pe
!
      type(interpolate_table), intent(inout) :: itp_tbl1_IO
      type(interpolate_table), intent(inout) :: itp_tbl2_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read merged  binary interpolation file: ', trim(file_name)
!
      call open_read_mpi_file_b(file_name, num_pe, id_rank, IO_param1)
      call mpi_read_each_itp_coef_table_b(IO_param1, itp_tbl1_IO)
      call mpi_read_each_itp_coef_table_b(IO_param1, itp_tbl2_IO)
      call close_mpi_file(IO_param1)
!
      end subroutine mpi_rd_dbl_itp_tbl_idx_file_b
!
!-----------------------------------------------------------------------
!
      end module MPI_itrplte_tbl_file_IO_b

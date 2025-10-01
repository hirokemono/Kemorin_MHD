!>@file   MPI_itrplte_table_data_IO_b.f90
!!@brief  module MPI_itrplte_table_data_IO_b
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!!
!>@brief  Binary interpolation file IO using MPi-IO
!!
!!@verbatim
!!      subroutine mpi_write_each_itp_coef_table_b(IO_param, itp_tbl_IO)
!!      subroutine mpi_write_each_itp_idx_table_b(IO_param, itp_tbl_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(interpolate_table), intent(in) :: itp_tbl_IO
!!      subroutine mpi_read_each_itp_coef_table_b(IO_param, itp_tbl_IO)
!!      subroutine mpi_read_each_itp_idx_table_b(IO_param, itp_tbl_IO)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!        type(interpolate_table), intent(inout) :: itp_tbl_IO
!!@endverbatim
!
      module MPI_itrplte_table_data_IO_b
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
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine mpi_write_each_itp_coef_table_b(IO_param, itp_tbl_IO)
!
      use MPI_itp_tbl_org_data_IO_b
      use MPI_itp_tbl_dest_data_IO_b
      use MPI_ascii_data_IO
      use MPI_binary_head_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table), intent(in) :: itp_tbl_IO
!
!
      call mpi_write_each_itp_idx_table_b(IO_param, itp_tbl_IO)
      call mpi_write_itp_coefs_org_b(IO_param, itp_tbl_IO%tbl_org)
!
      end subroutine mpi_write_each_itp_coef_table_b
!
!-----------------------------------------------------------------------
!
      subroutine mpi_write_each_itp_idx_table_b(IO_param, itp_tbl_IO)
!
      use MPI_itp_tbl_org_data_IO_b
      use MPI_itp_tbl_dest_data_IO_b
      use MPI_ascii_data_IO
      use MPI_binary_head_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table), intent(in) :: itp_tbl_IO
!
!
      call mpi_write_itp_domain_dest_b(IO_param, itp_tbl_IO%tbl_dest)
      call mpi_write_itp_table_dest_b(IO_param, itp_tbl_IO%tbl_dest)
!
      call mpi_write_itp_domain_org_b(IO_param, itp_tbl_IO%tbl_org)
      call mpi_write_itp_table_org_b(IO_param, itp_tbl_IO%tbl_org)
      call mpi_write_itp_idx_org_b(IO_param, itp_tbl_IO%tbl_org)
!
      end subroutine mpi_write_each_itp_idx_table_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine mpi_read_each_itp_coef_table_b(IO_param, itp_tbl_IO)
!
      use MPI_itp_tbl_org_data_IO_b
      use MPI_itp_tbl_dest_data_IO_b
      use MPI_ascii_data_IO
      use MPI_binary_head_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table), intent(inout) :: itp_tbl_IO
!
!
      call mpi_read_each_itp_idx_table_b(IO_param, itp_tbl_IO)
      call mpi_read_itp_coefs_org_b(IO_param, itp_tbl_IO%tbl_org)
!
      end subroutine mpi_read_each_itp_coef_table_b
!
!-----------------------------------------------------------------------
!
      subroutine mpi_read_each_itp_idx_table_b(IO_param, itp_tbl_IO)
!
      use MPI_itp_tbl_org_data_IO_b
      use MPI_itp_tbl_dest_data_IO_b
      use MPI_ascii_data_IO
      use MPI_binary_head_IO
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
      type(interpolate_table), intent(inout) :: itp_tbl_IO
!
!
      call mpi_read_itp_domain_dest_b(IO_param, itp_tbl_IO%tbl_dest)
      call mpi_read_itp_table_dest_b(IO_param, itp_tbl_IO%tbl_dest)
!
      call mpi_read_itp_domain_org_b(IO_param, itp_tbl_IO%tbl_org)
      call mpi_read_itp_table_org_b(IO_param, itp_tbl_IO%tbl_org)
      call mpi_read_itp_idx_org_b(IO_param, itp_tbl_IO%tbl_org)
!
      end subroutine mpi_read_each_itp_idx_table_b
!
!-----------------------------------------------------------------------
!
      end module MPI_itrplte_table_data_IO_b

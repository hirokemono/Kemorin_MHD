!>@file   MPI_itp_table_file_IO_b.f90
!!@brief  module MPI_itp_table_file_IO_b
!!
!!@author H. Matsui
!!@date Programmed on Oct., 2020
!!
!>@brief  Binary interpolation file IO using MPi-IO
!!
!!@verbatim
!!      subroutine mpi_write_itp_table_file_b(file_name, itp_tbl_IO)
!!        type(interpolate_table), intent(in) :: itp_tbl_IO
!!      subroutine mpi_write_dbl_itp_tbl_file_b                         &
!!     &         (file_name, itp_tbl1_IO, itp_tbl2_IO)
!!        type(interpolate_table), intent(in) :: itp_tbl1_IO
!!        type(interpolate_table), intent(in) :: itp_tbl2_IO
!!
!!      subroutine mpi_read_itp_table_file_b                            &
!!     &          (file_name, id_rank, num_pe, itp_tbl_IO)
!!        type(interpolate_table), intent(inout) :: itp_tbl_IO
!!      subroutine mpi_read_dbl_itp_tbl_file_b                          &
!!     &         (file_name, id_rank, num_pe, itp_tbl1_IO, itp_tbl2_IO)
!!        type(interpolate_table), intent(inout) :: itp_tbl1_IO
!!        type(interpolate_table), intent(inout) :: itp_tbl2_IO
!!@endverbatim
!
      module MPI_itp_table_file_IO_b
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
      private :: mpi_write_each_itp_table_b, mpi_read_each_itp_table_b
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine mpi_write_itp_table_file_b(file_name, itp_tbl_IO)
!
      use MPI_itp_table_data_IO_b
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
      call mpi_write_each_itp_table_b(IO_param1, itp_tbl_IO)
      call close_mpi_file(IO_param1)
!
      end subroutine mpi_write_itp_table_file_b
!
!-----------------------------------------------------------------------
!
      subroutine mpi_write_dbl_itp_tbl_file_b                           &
     &         (file_name, itp_tbl1_IO, itp_tbl2_IO)
!
      use MPI_itp_table_data_IO_b
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
      call mpi_write_each_itp_table_b(IO_param1, itp_tbl1_IO)
      call mpi_write_each_itp_table_b(IO_param1, itp_tbl2_IO)
      call close_mpi_file(IO_param1)
!
      end subroutine mpi_write_dbl_itp_tbl_file_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine mpi_read_itp_table_file_b                              &
     &          (file_name, id_rank, num_pe, itp_tbl_IO)
!
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
      call mpi_read_each_itp_table_b(IO_param1, itp_tbl_IO)
      call close_mpi_file(IO_param1)
!
      end subroutine mpi_read_itp_table_file_b
!
!-----------------------------------------------------------------------
!
      subroutine mpi_read_dbl_itp_tbl_file_b                            &
     &         (file_name, id_rank, num_pe, itp_tbl1_IO, itp_tbl2_IO)
!
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
      call mpi_read_each_itp_table_b(IO_param1, itp_tbl1_IO)
      call mpi_read_each_itp_table_b(IO_param1, itp_tbl2_IO)
      call close_mpi_file(IO_param1)
!
      end subroutine mpi_read_dbl_itp_tbl_file_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine mpi_write_each_itp_table_b(IO_param, itp_tbl_IO)
!
      use MPI_itp_table_data_IO_b
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
      call mpi_write_itp_coefs_org_b(IO_param, itp_tbl_IO%tbl_org)
!
      end subroutine mpi_write_each_itp_table_b
!
!-----------------------------------------------------------------------
!
      subroutine mpi_read_each_itp_table_b(IO_param, itp_tbl_IO)
!
      use MPI_itp_table_data_IO_b
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
      call mpi_read_itp_coefs_org_b(IO_param, itp_tbl_IO%tbl_org)
!
      end subroutine mpi_read_each_itp_table_b
!
!-----------------------------------------------------------------------
!
      end module MPI_itp_table_file_IO_b

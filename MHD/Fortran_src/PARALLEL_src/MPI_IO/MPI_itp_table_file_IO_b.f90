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
!!      subroutine mpi_read_itp_table_file_b                            &
!!     &          (file_name, id_rank, num_pe, itp_tbl_IO)
!!        type(interpolate_table_org), intent(inout) :: IO_itp_org
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!
!!      subroutine mpi_wrt_itp_coefs_dest_file_b                        &
!!     &         (file_name, IO_itp_dest, IO_itp_c_dest)
!!      subroutine mpi_read_itp_coefs_dest_file_b                       &
!!     &        (file_name, id_rank, num_pe, IO_itp_dest, IO_itp_c_dest)
!!      subroutine mpi_read_itp_table_dest_file_b                       &
!!     &         (file_name, id_rank, num_pe, IO_itp_dest)
!!      subroutine mpi_read_itp_domain_dest_file_b                      &
!!     &         (file_name, id_rank, num_pe, IO_itp_dest)
!!        type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!!        type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
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
      type(calypso_MPI_IO_params), save, private :: IO_param
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
!
      type(interpolate_table), intent(inout) :: itp_tbl_IO
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write merged binary interpolation file: ', trim(file_name)
!
      call open_write_mpi_file_b(file_name, IO_param)
      call mpi_write_itp_domain_dest_b(IO_param, itp_tbl_IO%tbl_dest)
      call mpi_write_itp_table_dest_b(IO_param, itp_tbl_IO%tbl_dest)
!
      call mpi_write_itp_domain_org_b(IO_param, itp_tbl_IO%tbl_org)
      call mpi_write_itp_table_org_b(IO_param, itp_tbl_IO%tbl_org)
      call mpi_write_itp_coefs_org_b(IO_param, itp_tbl_IO%tbl_org)
      call close_mpi_file(IO_param)
!
      call dealloc_itp_table_org(itp_tbl_IO%tbl_org)
      call dealloc_itp_num_org(itp_tbl_IO%tbl_org)
      call dealloc_itp_table_dest(itp_tbl_IO%tbl_dest)
      call dealloc_itp_num_dest(itp_tbl_IO%tbl_dest)
!
      end subroutine mpi_write_itp_table_file_b
!
!-----------------------------------------------------------------------
!
      subroutine mpi_read_itp_table_file_b                              &
     &          (file_name, id_rank, num_pe, itp_tbl_IO)
!
      use MPI_itp_table_data_IO_b
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
      call open_read_mpi_file_b(file_name, num_pe, id_rank, IO_param)
      call mpi_read_itp_domain_dest_b(IO_param, itp_tbl_IO%tbl_dest)
      call mpi_read_itp_table_dest_b(IO_param, itp_tbl_IO%tbl_dest)
!
      call mpi_read_itp_domain_org_b(IO_param, itp_tbl_IO%tbl_org)
      call mpi_read_itp_table_org_b(IO_param, itp_tbl_IO%tbl_org)
      call mpi_read_itp_coefs_org_b(IO_param, itp_tbl_IO%tbl_org)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_read_itp_table_file_b
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine mpi_wrt_itp_coefs_dest_file_b                          &
     &         (file_name, IO_itp_dest, IO_itp_c_dest)
!
      use MPI_itp_table_data_IO_b
      use MPI_ascii_data_IO
      use MPI_binary_head_IO
!
      character(len=kchara), intent(in) :: file_name
!
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Write binary export coefs file: ', trim(file_name)
!
      call open_write_mpi_file_b(file_name, IO_param)
      call mpi_write_itp_domain_dest_b(IO_param, IO_itp_dest)
      call mpi_write_itp_table_dest_b(IO_param, IO_itp_dest)
      call mpi_write_itp_coefs_dest_b                                   &
     &   (IO_param, IO_itp_dest, IO_itp_c_dest)
      call close_mpi_file(IO_param)
!
      call dealloc_itp_coef_dest(IO_itp_c_dest)
      call dealloc_itp_table_dest(IO_itp_dest)
      call dealloc_itp_coef_stack(IO_itp_c_dest)
      call dealloc_itp_num_dest(IO_itp_dest)
!
      end subroutine mpi_wrt_itp_coefs_dest_file_b
!
!-----------------------------------------------------------------------
!
      subroutine mpi_read_itp_coefs_dest_file_b                         &
     &        (file_name, id_rank, num_pe, IO_itp_dest, IO_itp_c_dest)
!
      use MPI_itp_table_data_IO_b
      use MPI_ascii_data_IO
      use MPI_binary_head_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank, num_pe
!
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
      type(interpolate_coefs_dest), intent(inout) :: IO_itp_c_dest
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read binary export coefs file: ', trim(file_name)
!
      call open_read_mpi_file_b(file_name, num_pe, id_rank, IO_param)
      call mpi_read_itp_domain_dest_b(IO_param, IO_itp_dest)
      call mpi_read_itp_table_dest_b(IO_param, IO_itp_dest)
      call mpi_read_itp_coefs_dest_b                                    &
     &   (IO_param, IO_itp_dest, IO_itp_c_dest)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_read_itp_coefs_dest_file_b
!
!-----------------------------------------------------------------------
!
      subroutine mpi_read_itp_table_dest_file_b                         &
     &         (file_name, id_rank, num_pe, IO_itp_dest)
!
      use MPI_itp_table_data_IO_b
      use MPI_ascii_data_IO
      use MPI_binary_head_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank, num_pe
!
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read binary interapolate export file: ', trim(file_name)
!
      call open_read_mpi_file_b(file_name, num_pe, id_rank, IO_param)
      call mpi_read_itp_domain_dest_b(IO_param, IO_itp_dest)
      call mpi_read_itp_table_dest_b(IO_param, IO_itp_dest)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_read_itp_table_dest_file_b
!
!-----------------------------------------------------------------------
!
      subroutine mpi_read_itp_domain_dest_file_b                        &
     &         (file_name, id_rank, num_pe, IO_itp_dest)
!
      use MPI_itp_table_data_IO_b
      use MPI_ascii_data_IO
      use MPI_binary_head_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: id_rank, num_pe
!
      type(interpolate_table_dest), intent(inout) :: IO_itp_dest
!
!
      if(my_rank.eq.0 .or. i_debug .gt. 0) write(*,*)                   &
     &   'Read binary export domain file: ', trim(file_name)
!
      call open_read_mpi_file_b(file_name, num_pe, id_rank, IO_param)
      call mpi_read_itp_domain_dest_b(IO_param, IO_itp_dest)
      call close_mpi_file(IO_param)
!
      end subroutine mpi_read_itp_domain_dest_file_b
!
!-----------------------------------------------------------------------
!
      end module MPI_itp_table_file_IO_b

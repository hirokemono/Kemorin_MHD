!>@file  gz_field_file_MPI_IO_b.f90
!!       module c
!!
!!@author H. Matsui
!!@date   Programmed in May, 2015
!
!> @brief Output merged binary field file using MPI-IO
!!
!!@verbatim
!!      subroutine gz_write_step_fld_file_mpi_b(file_name, t_IO, fld_IO)
!!        type(time_data), intent(in) :: t_IO
!!        type(field_IO), intent(in) :: fld_IO
!!
!!      subroutine gz_read_step_field_file_mpi_b                        &
!!     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!!      subroutine gz_rd_alloc_st_fld_file_mpi_b                        &
!!     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!!      subroutine gz_rd_alloc_st_fld_head_mpi_b                        &
!!     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!!        type(time_data), intent(inout) :: t_IO
!!        type(field_IO), intent(inout) :: fld_IO
!!@endverbatim
!
      module gz_field_file_MPI_IO_b
!
      use m_precision
      use m_constants
!
      use calypso_mpi
      use m_machine_parameter
      use m_calypso_mpi_IO
      use t_time_data
      use t_field_data_IO
      use t_calypso_mpi_IO_param
!
      implicit none
!
      type(calypso_MPI_IO_params), save, private :: IO_param
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine gz_write_step_fld_file_mpi_b(file_name, t_IO, fld_IO)
!
      use m_error_IDs
      use MPI_binary_head_IO
      use gz_MPI_binary_datum_IO
      use gz_field_block_MPI_IO_b
      use MPI_ascii_data_IO
      use transfer_to_long_integers
!
      character(len=kchara), intent(in) :: file_name
!
      type(time_data), intent(in) :: t_IO
      type(field_IO), intent(in) :: fld_IO
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'write gzipped binary data by MPI-IO: ', trim(file_name)
!
      call open_write_gz_mpi_file_b(file_name, IO_param)
!
      call gz_write_field_time_mpi_b(IO_param,                          &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt)
      call gz_write_field_data_mpi_b                                    &
     &   (IO_param, cast_long(fld_IO%nnod_IO), fld_IO%num_field_IO,     &
     &    fld_IO%ntot_comp_IO, fld_IO%num_comp_IO, fld_IO%fld_name,     &
     &    fld_IO%istack_numnod_IO, fld_IO%d_IO)
!
      call close_mpi_file(IO_param)
!
      end subroutine gz_write_step_fld_file_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_read_step_field_file_mpi_b                          &
     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!
      use field_file_MPI_IO
      use MPI_binary_head_IO
      use gz_MPI_binary_data_IO
      use gz_MPI_binary_head_IO
      use gz_MPI_binary_datum_IO
      use gz_field_block_MPI_IO_b
      use MPI_ascii_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      integer(kind = kint_gl) :: num64
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'read gzipped binary data by MPI-IO: ', trim(file_name)
      call open_read_gz_mpi_file_b                                      &
     &   (file_name, num_pe, id_rank, IO_param)
!
      call gz_read_step_data_mpi_b(IO_param,                            &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt)
!
      call alloc_merged_field_stack(num_pe, fld_IO)
      call gz_read_field_header_mpi_b                                   &
     &   (IO_param, num64, fld_IO%num_field_IO)
      fld_IO%nnod_IO = int(num64, KIND(fld_IO%nnod_IO))
!
      call gz_mpi_read_mul_inthead_b                                    &
     &   (IO_param, fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call cal_istack_phys_comp_IO(fld_IO)
!
      call gz_mpi_read_mul_charahead_b                                  &
     &   (IO_param, fld_IO%num_field_IO, fld_IO%fld_name)
      num64 = fld_IO%nnod_IO
      call gz_mpi_read_2d_vector_b                                      &
     &   (IO_param, num64, fld_IO%ntot_comp_IO, fld_IO%d_IO)
!
      call close_mpi_file(IO_param)
!
      call dealloc_merged_field_stack(fld_IO)
!
      end subroutine gz_read_step_field_file_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_rd_alloc_st_fld_file_mpi_b                          &
     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!
      use field_file_MPI_IO
      use MPI_binary_head_IO
      use gz_MPI_binary_data_IO
      use gz_MPI_binary_head_IO
      use gz_MPI_binary_datum_IO
      use gz_field_block_MPI_IO_b
      use MPI_ascii_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      integer(kind = kint_gl) :: num64
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &      'read gzipped binary data MPI-IO: ', trim(file_name)
      call open_read_gz_mpi_file_b                                      &
     &   (file_name, num_pe, id_rank, IO_param)
!
      call gz_read_step_data_mpi_b(IO_param,                            &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt)
!
      call alloc_merged_field_stack(num_pe, fld_IO)
      call gz_read_field_header_mpi_b                                   &
     &   (IO_param, num64, fld_IO%num_field_IO)
      fld_IO%nnod_IO = int(num64, KIND(fld_IO%nnod_IO))
!
      call alloc_phys_name_IO(fld_IO)
      call gz_mpi_read_mul_inthead_b                                    &
     &   (IO_param, fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call cal_istack_phys_comp_IO(fld_IO)
      call alloc_phys_data_IO(fld_IO)
!
      call gz_mpi_read_mul_charahead_b                                  &
     &   (IO_param, fld_IO%num_field_IO, fld_IO%fld_name)
      num64 = fld_IO%nnod_IO
      call gz_mpi_read_2d_vector_b                                      &
     &   (IO_param, num64, fld_IO%ntot_comp_IO, fld_IO%d_IO)
!
      call close_mpi_file(IO_param)
!
      call dealloc_merged_field_stack(fld_IO)
      if(id_rank .ge. num_pe) then
        call dealloc_phys_data_IO(fld_IO)
        call dealloc_phys_name_IO(fld_IO)
      end if
!
      end subroutine gz_rd_alloc_st_fld_file_mpi_b
!
! -----------------------------------------------------------------------
!
      subroutine gz_rd_alloc_st_fld_head_mpi_b                          &
     &         (file_name, num_pe, id_rank, t_IO, fld_IO)
!
      use field_file_MPI_IO
      use MPI_binary_head_IO
      use gz_MPI_binary_head_IO
      use gz_MPI_binary_datum_IO
      use gz_field_block_MPI_IO_b
      use MPI_ascii_data_IO
!
      character(len=kchara), intent(in) :: file_name
      integer, intent(in) :: num_pe, id_rank
!
      type(time_data), intent(inout) :: t_IO
      type(field_IO), intent(inout) :: fld_IO
!
      integer(kind = kint_gl) :: num64
!
!
      if(my_rank .eq. 0) write(*,*)                                     &
     &    'read gzipped binary data by MPI-IO: ', trim(file_name)
      call open_read_gz_mpi_file_b                                      &
     &   (file_name, num_pe, id_rank, IO_param)
!
      call gz_read_step_data_mpi_b(IO_param,                            &
     &    t_IO%i_time_step, t_IO%time, t_IO%dt)
!
      call alloc_merged_field_stack(num_pe, fld_IO)
      call gz_read_field_header_mpi_b                                   &
     &   (IO_param, num64, fld_IO%num_field_IO)
      fld_IO%nnod_IO = int(num64, KIND(fld_IO%nnod_IO))
!
      call alloc_phys_name_IO(fld_IO)
      call gz_mpi_read_mul_inthead_b                                    &
     &   (IO_param, fld_IO%num_field_IO, fld_IO%num_comp_IO)
!
      call cal_istack_phys_comp_IO(fld_IO)
!
      call gz_mpi_read_mul_charahead_b                                  &
     &   (IO_param, fld_IO%num_field_IO, fld_IO%fld_name)
!
      call close_mpi_file(IO_param)
!
      call dealloc_merged_field_stack(fld_IO)
      if(id_rank .ge. num_pe) call dealloc_phys_name_IO(fld_IO)
!
      end subroutine gz_rd_alloc_st_fld_head_mpi_b
!
! -----------------------------------------------------------------------
!
      end module gz_field_file_MPI_IO_b

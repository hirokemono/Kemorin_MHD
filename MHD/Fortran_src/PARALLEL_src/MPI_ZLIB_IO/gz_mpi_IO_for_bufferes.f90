!>@file  gz_mpi_IO_for_bufferes.f90
!!       module gz_mpi_IO_for_bufferes
!!
!!@author H. Matsui
!!@date   Programmed in Aug., 2016
!
!> @brief gzipped binary data IO to MPI-IO buffer
!!
!!@verbatim
!!      subroutine gz_mpi_write_int8_vector_mul(IO_param)
!!      subroutine gz_mpi_write_int_vector_mul(IO_param)
!!      subroutine gz_mpi_write_int2d_vector_mul(IO_param)
!!      subroutine gz_mpi_write_1d_vector_mul(IO_param)
!!      subroutine gz_mpi_write_2d_vector_mul(IO_param)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!
!!      subroutine gz_mpi_read_int8_vector_mul(IO_param)
!!      subroutine gz_mpi_read_int_vector_mul(IO_param)
!!      subroutine gz_mpi_read_int2d_vector_mul(IO_param)
!!      subroutine gz_mpi_read_1d_vector_mul(IO_param)
!!      subroutine gz_mpi_read_2d_vector_mul(IO_param)
!!        type(calypso_MPI_IO_params), intent(inout) :: IO_param
!!@endverbatim
!
      module gz_mpi_IO_for_bufferes
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
!
      use t_calypso_mpi_IO_param
      use gz_MPI_binary_head_IO
      use MPI_binery_IO_4_buffers
      use defleat_4_merged_arrays
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine gz_mpi_write_int8_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call defleat_int8_vector_mul                                      &
     &   (IO_param%nloop, IO_param%i8_array, IO_param%c_array)
      call set_istack_by_chara_length(IO_param%nprocs_in,               &
     &   IO_param%nloop, IO_param%c_array, IO_param%istack_merged)
!
      call gz_mpi_write_i8stack_head_b                                  &
     &   (IO_param, IO_param%nprocs_in, IO_param%istack_merged)
      call mpi_write_chara_array_mul                                    &
     &   (IO_param%id_file, IO_param%nprocs_in, IO_param%nloop,         &
     &    IO_param%ioff_gl, IO_param%istack_merged, IO_param%c_array)
      call dealloc_character_buffers(IO_param%nloop, IO_param%c_array)
!
      end subroutine gz_mpi_write_int8_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_int_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call defleat_int_vector_mul                                       &
     &   (IO_param%nloop, IO_param%i_array, IO_param%c_array)
      call set_istack_by_chara_length(IO_param%nprocs_in,               &
     &   IO_param%nloop, IO_param%c_array, IO_param%istack_merged)
!
      call gz_mpi_write_i8stack_head_b                                  &
     &   (IO_param, IO_param%nprocs_in, IO_param%istack_merged)
      call mpi_write_chara_array_mul                                    &
     &   (IO_param%id_file, IO_param%nprocs_in, IO_param%nloop,         &
     &    IO_param%ioff_gl, IO_param%istack_merged, IO_param%c_array)
      call dealloc_character_buffers(IO_param%nloop, IO_param%c_array)
!
      end subroutine gz_mpi_write_int_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_int2d_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call defleat_int2d_vector_mul                                     &
     &   (IO_param%nloop, IO_param%iv_array, IO_param%c_array)
      call set_istack_by_chara_length(IO_param%nprocs_in,               &
     &   IO_param%nloop, IO_param%c_array, IO_param%istack_merged)
!
      call gz_mpi_write_i8stack_head_b                                  &
     &   (IO_param, IO_param%nprocs_in, IO_param%istack_merged)
      call mpi_write_chara_array_mul                                    &
     &   (IO_param%id_file, IO_param%nprocs_in, IO_param%nloop,         &
     &    IO_param%ioff_gl, IO_param%istack_merged, IO_param%c_array)
      call dealloc_character_buffers(IO_param%nloop, IO_param%c_array)
!
      end subroutine gz_mpi_write_int2d_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_1d_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call defleat_1d_vector_mul                                        &
     &   (IO_param%nloop, IO_param%r_array, IO_param%c_array)
      call set_istack_by_chara_length(IO_param%nprocs_in,               &
     &   IO_param%nloop, IO_param%c_array, IO_param%istack_merged)
!
      call gz_mpi_write_i8stack_head_b                                  &
     &   (IO_param, IO_param%nprocs_in, IO_param%istack_merged)
      call mpi_write_chara_array_mul                                    &
     &   (IO_param%id_file, IO_param%nprocs_in, IO_param%nloop,         &
     &    IO_param%ioff_gl, IO_param%istack_merged, IO_param%c_array)
      call dealloc_character_buffers(IO_param%nloop, IO_param%c_array)
!
      end subroutine gz_mpi_write_1d_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_write_2d_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call defleat_2d_vector_mul                                        &
     &   (IO_param%nloop, IO_param%v_array, IO_param%c_array)
      call set_istack_by_chara_length(IO_param%nprocs_in,               &
     &   IO_param%nloop, IO_param%c_array, IO_param%istack_merged)
!
      call gz_mpi_write_i8stack_head_b                                  &
     &   (IO_param, IO_param%nprocs_in, IO_param%istack_merged)
      call mpi_write_chara_array_mul                                    &
     &   (IO_param%id_file, IO_param%nprocs_in, IO_param%nloop,         &
     &    IO_param%ioff_gl, IO_param%istack_merged, IO_param%c_array)
      call dealloc_character_buffers(IO_param%nloop, IO_param%c_array)
!
      end subroutine gz_mpi_write_2d_vector_mul
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_int8_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call gz_mpi_read_i8stack_head_b                                   &
     &   (IO_param, IO_param%nprocs_in, IO_param%istack_merged)
      call mpi_read_chara_array_mul                                     &
     &   (IO_param%id_file, IO_param%nprocs_in, IO_param%nloop,         &
     &    IO_param%ioff_gl, IO_param%istack_merged, IO_param%c_array)
!
      call infleat_int8_vector_mul(IO_param%iflag_bin_swap,             &
     &    IO_param%nloop, IO_param%c_array, IO_param%i8_array)
!
      end subroutine gz_mpi_read_int8_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_int_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call gz_mpi_read_i8stack_head_b                                   &
     &   (IO_param, IO_param%nprocs_in, IO_param%istack_merged)
      call mpi_read_chara_array_mul                                     &
     &   (IO_param%id_file, IO_param%nprocs_in, IO_param%nloop,         &
     &    IO_param%ioff_gl, IO_param%istack_merged, IO_param%c_array)
!
      call infleat_int_vector_mul(IO_param%iflag_bin_swap,              &
     &    IO_param%nloop, IO_param%c_array, IO_param%i_array)
!
      end subroutine gz_mpi_read_int_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_int2d_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call gz_mpi_read_i8stack_head_b                                   &
     &   (IO_param, IO_param%nprocs_in, IO_param%istack_merged)
      call mpi_read_chara_array_mul                                     &
     &   (IO_param%id_file, IO_param%nprocs_in, IO_param%nloop,         &
     &    IO_param%ioff_gl, IO_param%istack_merged, IO_param%c_array)
!
      call infleat_int2d_vector_mul(IO_param%iflag_bin_swap,            &
     &    IO_param%nloop, IO_param%c_array, IO_param%iv_array)
!
      end subroutine gz_mpi_read_int2d_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_1d_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call gz_mpi_read_i8stack_head_b                                   &
     &   (IO_param, IO_param%nprocs_in, IO_param%istack_merged)
      call mpi_read_chara_array_mul                                     &
     &   (IO_param%id_file, IO_param%nprocs_in, IO_param%nloop,         &
     &    IO_param%ioff_gl, IO_param%istack_merged, IO_param%c_array)
!
      call infleat_1d_vector_mul(IO_param%iflag_bin_swap,               &
     &    IO_param%nloop, IO_param%c_array, IO_param%r_array)
!
      end subroutine gz_mpi_read_1d_vector_mul
!
! -----------------------------------------------------------------------
!
      subroutine gz_mpi_read_2d_vector_mul(IO_param)
!
      type(calypso_MPI_IO_params), intent(inout) :: IO_param
!
!
      call gz_mpi_read_i8stack_head_b                                   &
     &   (IO_param, IO_param%nprocs_in, IO_param%istack_merged)
      call mpi_read_chara_array_mul                                     &
     &   (IO_param%id_file, IO_param%nprocs_in, IO_param%nloop,         &
     &    IO_param%ioff_gl, IO_param%istack_merged, IO_param%c_array)
!
      call infleat_2d_vector_mul(IO_param%iflag_bin_swap,               &
     &    IO_param%nloop, IO_param%c_array, IO_param%v_array)
!
      end subroutine gz_mpi_read_2d_vector_mul
!
! -----------------------------------------------------------------------
!
      end module gz_mpi_IO_for_bufferes
